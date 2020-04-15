open SmartRecipes.Playground.Library
open SmartRecipes.Playground
open SmartRecipes.Playground.FoodToVector
open SmartRecipes.Playground.Model
open System
open Calibration

type IngredientMatchResult =
    | Binary of bool
    | Distance of float
    
let printRecipe doesIngredientMatch (recipe: Recipe) =
    printfn "<h3>%s</h3><br>" recipe.Name
    printfn "Ingredients: <br>"
    for ingredient in recipe.Ingredients do
        let matchResult =
            match doesIngredientMatch ingredient with
            | Binary b -> if b then "X" else ""
            | Distance d -> d.ToString("0.00")
        printfn "[%s] %s <br>" matchResult ingredient.DisplayLine
    printfn "<br>"
    
let printRecipes doesIngredientMatch recipes =
    for recipe in recipes do
        printRecipe doesIngredientMatch recipe
    
let toInfoLessAmounts = List.map (fun (id: Guid) -> { FoodstuffId = id; Unit = None; Value = None })
    
let showRecommendations recipes food2vecData32 food2vecData256 foodstuffAmounts foodstuffWords =
    let statistics = TfIdf.computeStatistics recipes
    let foodstuffIds = foodstuffAmounts |> List.map (fun a -> a.FoodstuffId)
    
    let (jacccardResults, jaccardResultsMs) = profilePerformance (fun () ->
        JaccardSimilarity.recommend recipes foodstuffIds |> Seq.take 10 |> Seq.toList)
    
    let (plainTfIdfResults, plainTfIdfResultsMs) = profilePerformance (fun () ->
        TfIdf.recommend statistics foodstuffAmounts |> Seq.take 10 |> Seq.map (fun i -> i.Recipe) |> Seq.toList)
    
    let (textTfIdfResults, textTfIdfResultsMs) = profilePerformance (fun () ->
        TextTfIdf.recommend recipes foodstuffWords |> Seq.take 10 |> Seq.toList)
    
    let (calibratedTfIdfResults, calibratedTfIdfResultsMs) = profilePerformance (fun () ->
        // TODO: implement calibration.
        TfIdf.recommend statistics foodstuffAmounts |> Seq.take 10 |> Seq.map (fun i -> i.Recipe) |> Seq.toList)
    
    let (diversifiedTfIdfResults, diversifiedTfIdfResultsMs) = profilePerformance (fun () ->
        let infos = TfIdf.recommend statistics foodstuffAmounts
        Diversity.postProcess infos (TfIdf.recipeSimilarity statistics) 10 |> List.map (fun r -> r.Recipe))
    
    let (plainWordToVecResults, plainWordToVecResultsMs) = profilePerformance (fun () ->
        FoodToVector.recommend food2vecData256 TfIdf.termFrequency statistics.InverseIndex (toInfoLessAmounts foodstuffIds) |> Seq.take 10 |> Seq.map (fun i -> i.Info.Recipe) |> Seq.toList)
    
    let (tfWeightedWordToVecResults, tfWeightedWordToVecResultsMs) = profilePerformance (fun () ->
        FoodToVector.recommend food2vecData256 TfIdf.termFrequency statistics.InverseIndex foodstuffAmounts |> Seq.take 10 |> Seq.map (fun i -> i.Info.Recipe) |> Seq.toList)
    
    let (tfIdfWeightedWordToVecResults, tfIdfWeightedWordToVecResultsMs) = profilePerformance (fun () ->
        FoodToVector.recommend food2vecData256 ((TfIdf.tfIdf statistics) >> second) statistics.InverseIndex foodstuffAmounts |> Seq.take 10 |> Seq.map (fun i -> i.Info.Recipe) |> Seq.toList)
    
    let (diversifiedTfIdfWeightedWordToVec256Results, diversifiedTfIdfWeightedWordToVec256ResultsMs) = profilePerformance (fun () ->
        let weight = (TfIdf.tfIdf statistics) >> second
        let infos = FoodToVector.recommend food2vecData256 weight statistics.InverseIndex foodstuffAmounts |> List.map (fun i -> i.Info)
        Diversity.postProcess infos (FoodToVector.recipeSimilarity food2vecData256 weight) 10 |> List.map (fun r -> r.Recipe))

    let (diversifiedTfIdfWeightedWordToVec32Results, diversifiedTfIdfWeightedWordToVec32ResultsMs) = profilePerformance (fun () ->
        let weight = (TfIdf.tfIdf statistics) >> second
        let infos = FoodToVector.recommend food2vecData32 weight statistics.InverseIndex foodstuffAmounts |> List.map (fun i -> i.Info)
        Diversity.postProcess infos (FoodToVector.recipeSimilarity food2vecData32 weight) 10 |> List.map (fun r -> r.Recipe))
    
    let (calibratedWithElectionTfIdfWeightedWordToVec256Results, calibratedWithElectionTfIdfWeightedWordToVec256ResultsMs) = profilePerformance (fun () ->
        let weight = (TfIdf.tfIdf statistics) >> second
        let infos = FoodToVector.recommend food2vecData256 weight statistics.InverseIndex foodstuffAmounts
        let recipes = infos |> List.map (fun i -> i.Info.Recipe)
        let recipeVectors = infos |> List.map (fun i -> (i.Info.Recipe.Id, i.Vector)) |> Map.ofList
        let similarity foodstuffAmountInfos recipe =
            let weightsByFoodstuffAmount = foodstuffAmountInfos |> List.map (fun i -> (i.Amount.FoodstuffId, i.Weight)) |> Map.ofList
            let queryVector = vectorize food2vecData256 (fun a -> Map.find a.FoodstuffId weightsByFoodstuffAmount) foodstuffAmounts
            let recipeVector = Map.find recipe.Id recipeVectors
            cosineSimilarity queryVector recipeVector
            
        Calibration.calibrate similarity weight recipes foodstuffAmounts 10)
    
    let allRecipes = List.concat [
        jacccardResults;
        plainTfIdfResults;
        textTfIdfResults;
        calibratedTfIdfResults;
        diversifiedTfIdfResults;
        plainWordToVecResults;
        tfWeightedWordToVecResults;
        tfIdfWeightedWordToVecResults;
        diversifiedTfIdfWeightedWordToVec256Results;
        calibratedWithElectionTfIdfWeightedWordToVec256Results;
        diversifiedTfIdfWeightedWordToVec32Results;
    ]
    
    let counts =
        allRecipes
        |> List.groupBy (fun r -> r.Name)
        |> List.map (fun (name, recipes) -> (name, Seq.length recipes))
        |> List.sortByDescending second
        
    let findMaxSimilarity foodstuffIds (ingredient: Ingredient) =
        let toVector fId = Map.find fId food2vecData256
        let ingredientVector = toVector ingredient.Amount.FoodstuffId
        foodstuffIds
            |> List.map (toVector >> (FoodToVector.cosineSimilarity ingredientVector))
            |> List.max
        
    let printPerformance p =
        printfn "<h3>%d ms</h3><br>" p
        
    let printMethod name recommendations performance similarity =
        printfn "<div>"
        printfn "<div style=\"float: left;\">"
        printfn "--------------------------- <br>"
        printfn "<h2>%s</h2><br>" name
        printPerformance performance 
        printfn "--------------------------- <br>"
        printRecipes similarity recommendations
        printfn "</div>"
    
    printfn "Overall statistics <br>"
    for (recipe, count) in counts do
        printfn "Recipe: %s; Count: %i <br>" recipe count
    printfn ""
    
    printMethod "Jaccard" jacccardResults jaccardResultsMs (fun i -> List.exists (fun id -> id = i.Amount.FoodstuffId) foodstuffIds |> Binary)
    printMethod "TF-IDF with structured data" plainTfIdfResults plainTfIdfResultsMs (fun i -> List.exists (fun a -> a.FoodstuffId = i.Amount.FoodstuffId) foodstuffAmounts |> Binary)
    printMethod "TF-IDF with text data" textTfIdfResults textTfIdfResultsMs (fun i -> List.exists (fun (t: string) -> i.DisplayLine.ToLowerInvariant().Contains(t)) foodstuffWords |> Binary)
    printMethod "TF-IDF with structured data (Calibration)" calibratedTfIdfResults calibratedTfIdfResultsMs (fun i -> List.exists (fun a -> a.FoodstuffId = i.Amount.FoodstuffId) foodstuffAmounts |> Binary)
    printMethod "TF-IDF with structured data (MMR)" diversifiedTfIdfResults diversifiedTfIdfResultsMs (fun i -> List.exists (fun a -> a.FoodstuffId = i.Amount.FoodstuffId) foodstuffAmounts |> Binary)
    printMethod "Food2Vec 256/10 (mean)" plainWordToVecResults plainWordToVecResultsMs ((findMaxSimilarity foodstuffIds) >> Distance)
    printMethod "Food2Vec 256/10 (TF weighted mean)" tfWeightedWordToVecResults tfWeightedWordToVecResultsMs ((findMaxSimilarity foodstuffIds) >> Distance)
    printMethod "Food2Vec 256/10 (TF-IDF weighted mean)" tfIdfWeightedWordToVecResults tfIdfWeightedWordToVecResultsMs ((findMaxSimilarity foodstuffIds) >> Distance)
    printMethod "Food2Vec 256/10 (TF-IDF weighted mean, MMR)" diversifiedTfIdfWeightedWordToVec256Results diversifiedTfIdfWeightedWordToVec256ResultsMs ((findMaxSimilarity foodstuffIds) >> Distance)
    printMethod "Food2Vec 32/8 (TF-IDF weighted mean, MMR)" diversifiedTfIdfWeightedWordToVec32Results diversifiedTfIdfWeightedWordToVec32ResultsMs ((findMaxSimilarity foodstuffIds) >> Distance)
    printMethod "Food2Vec 256/10 (TF-IDF weighted mean, Election Calibration)" calibratedWithElectionTfIdfWeightedWordToVec256Results calibratedWithElectionTfIdfWeightedWordToVec256ResultsMs ((findMaxSimilarity foodstuffIds) >> Distance)
    
    printfn "</div>"
    printfn "<div style=\"clear: both;\"></div>"

let printHeader () =
    printfn "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\"><title>SmartRecipes recommendations</title></head><body style=\"width: 6000px\">"
    
let printFooter () =
    printfn "</body></html>"
    
[<EntryPoint>]
let main argv =
    printHeader ()
    
    let recipes = DataStore.getRecipes ()
    let food2vecData256 = Data.loadFoodstuffVectors "vectors-256.txt"
    let food2vecData32 = Data.loadFoodstuffVectors "vectors-32.txt"
    let run (introText: string) amounts words =
        printfn "<h1>%s</h1><br>" (introText.Replace("\n", "<br>"))
        showRecommendations recipes food2vecData32 food2vecData256 amounts words
        
    run
        @"
            Case 1: Searching with common ingredients with amounts specified (no specific edge-case).
            User profile:
                - beef (1 pound)
                - bell peppers (4 pieces)
                - mushrooms (5 pieces)
        "
        [
            {
                Value = Some 1.0
                Unit = Some "pound"
                FoodstuffId = Guid("fa9a10a7-50ab-41ad-9b12-dfd1f9c4b241") // Beef
            };
            {
                Value = Some 4.0
                Unit = Some "pieces"
                FoodstuffId = Guid("27b43955-3361-48a1-b16f-9d339c808b20") // Bell peppers
            };
            {
                Value = Some 5.0
                Unit = Some "pieces"
                FoodstuffId = Guid("491ed56e-1c1f-4d3f-8c61-27e3f4dcb32c") // Mushrooms
            }
        ]
        [
            "ground";
            "beef";
            "bell";
            "peppers";
            "mushrooms";
        ]
        
    run
        @"
            Case 2: Searching with very common ingredients.
            User profile:
                - salt (very common)
                - pepper (very common)
                - garam masala (uncommon)
        "
        [
            {
                Value = None
                Unit = None
                FoodstuffId = Guid("24b1b115-07e9-4d8f-b0a1-a38639654b7d") // Garam masala
            };
            {
                Value = None
                Unit = None
                FoodstuffId = Guid("2c6d80e8-f3ef-4845-bfc2-bd8e84c86bd9") // Pepper
            };
            {
                Value = None
                Unit = None
                FoodstuffId = Guid("cc8f46dd-27a3-4042-8b25-459f6d4a3679") // Salt
            }
        ]
        [
            "salt";
            "pepper";
            "garam";
            "masala";
        ]

    run
        @"
            Case 3.1: Testing relevance of ingredient amounts.
            User profile:
                - chicken breasts (5 pounds)
                - parmesan chees (not specified)
        "
        [
            {
                Value = Some 5.0
                Unit = Some "pound"
                FoodstuffId = Guid("cbd25042-ef0b-467f-8dfd-4ff70c2e5824") // Chicken breasts	
            };
            {
                Value = None
                Unit = None
                FoodstuffId = Guid("7dc3db3c-8422-473d-8344-2f8653157581") // Parmesan cheese	
            }
        ]
        [
            "chicken";
            "breasts";
            "parmesan";
            "cheese";
        ]
        
    run
        @"
            Case 3.2: Testing relevance of ingredient amounts.
            User profile:
                - chicken breasts (not specified)
                - parmesan chees (4 cups)
        "
        [
            {
                Value = None
                Unit = None
                FoodstuffId = Guid("cbd25042-ef0b-467f-8dfd-4ff70c2e5824") // Chicken breasts	
            };
            {
                Value = Some 4.0
                Unit = Some "cups"
                FoodstuffId = Guid("7dc3db3c-8422-473d-8344-2f8653157581") // Parmesan cheese	
            }
        ]
        [
            "chicken";
            "breasts";
            "parmesan";
            "cheese";
        ]
 
    run
        @"
            Case 4: Input aiming to get 2 recipes recommended instead of just 1 combining everything.
            User profile:
                - chicken breasts (1 pounds)
                - parmesan cheese (2 cups)
                - garam masala (1 cup)
                - chickpea (2 cups)
        "
        [
            {
                Value = Some 1.0
                Unit = Some "pound"
                FoodstuffId = Guid("cbd25042-ef0b-467f-8dfd-4ff70c2e5824") // Chicken breasts
            };
            {
                Value = Some 2.0
                Unit = Some "cups"
                FoodstuffId = Guid("7dc3db3c-8422-473d-8344-2f8653157581") // Parmesan cheese
            };
            {
                Value = Some 1.0
                Unit = Some "cup"
                FoodstuffId = Guid("24b1b115-07e9-4d8f-b0a1-a38639654b7d") // Garam masala
            };
            {
                Value = Some 2.0
                Unit = Some "cups"
                FoodstuffId = Guid("b17a087c-dcd1-4bec-b481-00d2165fd18a") // Chickpeas
            }
        ]
        [
            "chicken";
            "breasts";
            "parmesan";
            "cheese";
            "chickpeas";
            "garam";
            "masala";
        ]

    run
        @"
            Case 5: Simulating real shopping list when shopping.
            User profile:
                - beef (4 pounds)
                - carrots (5 pieces)
                - tomatoes (5 pieces)
                - yogurt (3 pieces)
                - bell peppers (3 pounds)
        "
        [
            {
                Value = Some 4.0
                Unit = Some "pound"
                FoodstuffId = Guid("fa9a10a7-50ab-41ad-9b12-dfd1f9c4b241") // Beef
            };
            {
                Value = Some 5.0
                Unit = Some "pieces"
                FoodstuffId = Guid("274f4bc5-63c8-4f46-aba1-a409b5e78dd4") // Carrots
            };
            {
                Value = Some 5.0
                Unit = Some "pieces"
                FoodstuffId = Guid("241505a7-c6d7-4a7b-a913-aad0389c4606") // Tomatoes
            };
            {
                Value = Some 3.0
                Unit = Some "pieces"
                FoodstuffId = Guid("80a641dd-f9a3-4484-ba6e-466ceda111f1") // Yogurt
            };
            {
                Value = Some 3.0
                Unit = Some "pieces"
                FoodstuffId = Guid("27b43955-3361-48a1-b16f-9d339c808b20") // Bell peppers
            }
        ]
        [
            "pork";
            "chop";
            "chicken";
            "thighs";
            "cheddar";
            "cheese";
            "yogurt";
            "bell";
            "peppers"
        ]
        
    printFooter ()
    
    0 // return an integer exit code
