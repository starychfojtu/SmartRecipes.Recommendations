open SmartRecipes.Playground.Library
open SmartRecipes.Playground
open SmartRecipes.Playground.FoodToVector
open SmartRecipes.Playground.Model
open System
open System
open Calibration
open FSharp.Json

type IngredientMatchResult =
    | Binary of bool
    | Distance of float
    
let toInfoLessAmounts = List.map (fun (id: Guid) -> { FoodstuffId = id; Unit = None; Value = None })
    
let showRecommendations recipes food2vecData32 food2vecData256 foodstuffAmounts foodstuffWords =
    let statistics = TfIdf.computeStatistics recipes
    let foodstuffIds = foodstuffAmounts |> List.map (fun a -> a.FoodstuffId)
    
    let (jacccardResults, jaccardResultsMs) = profilePerformance (fun () ->
        JaccardSimilarity.recommend recipes foodstuffIds |> Seq.take 10 |> Seq.toList)
    
    let (plainTfIdfResults, plainTfIdfResultsMs) = profilePerformance (fun () ->
        TfIdf.recommend statistics foodstuffAmounts |> Seq.take 10 |> Seq.map (fun i -> i.Info.Recipe) |> Seq.toList)
    
    let (textTfIdfResults, textTfIdfResultsMs) = profilePerformance (fun () ->
        TextTfIdf.recommend recipes foodstuffWords |> Seq.take 10 |> Seq.toList)
    
    let (calibratedTfIdfResults, calibratedTfIdfResultsMs) = profilePerformance (fun () ->
        let weight = (TfIdf.tfIdf statistics) >> second
        let infos = TfIdf.recommend statistics foodstuffAmounts |> Seq.toList
        let recipes = infos |> List.map (fun i -> i.Info.Recipe)
        let recipeVectors = infos |> List.map (fun i -> (i.Info.Recipe.Id, i.Vector)) |> Map.ofList
        let similarity foodstuffAmountInfos recipe =
            let queryVector = foodstuffAmountInfos |> List.map (fun i -> (i.Amount.FoodstuffId, i.Weight)) |> Map.ofList
            let recipeVector = Map.find recipe.Id recipeVectors
            Library.cosineSimilarity queryVector recipeVector
            
        Calibration.calibrate similarity weight recipes foodstuffAmounts 10)
    
    let (diversifiedTfIdfResults, diversifiedTfIdfResultsMs) = profilePerformance (fun () ->
        let infos = TfIdf.recommend statistics foodstuffAmounts |> Seq.map (fun i -> i.Info)
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
            let queryVector = FoodToVector.vectorize food2vecData256 (fun a -> Map.find a.FoodstuffId weightsByFoodstuffAmount) foodstuffAmounts
            let recipeVector = Map.find recipe.Id recipeVectors
            FoodToVector.cosineSimilarity queryVector recipeVector
            
        Calibration.calibrate similarity weight recipes foodstuffAmounts 10)
        
    let findMaxSimilarity foodstuffIds (ingredient: Ingredient) =
        let toVector fId = Map.find fId food2vecData256
        let ingredientVector = toVector ingredient.Amount.FoodstuffId
        foodstuffIds
            |> List.map (toVector >> (FoodToVector.cosineSimilarity ingredientVector))
            |> List.max
        
    let method id name recommendations performance similarity =
        let ingredient (ingredient: Ingredient) =
            {
                JsonExport.Ingredient.FoodstuffId = ingredient.Amount.FoodstuffId.ToString()
                JsonExport.Ingredient.DisplayLine = ingredient.DisplayLine
                JsonExport.Ingredient.IsInputMatch = match similarity ingredient with Binary m -> m | Distance d -> Math.Abs (d - 1.0) < 0.2
            }
            
        let recipe (recipe: Recipe) =
            {
                JsonExport.Recipe.Id = (recipe.Id.ToString())
                JsonExport.Recipe.Name = recipe.Name
                JsonExport.Recipe.Uri = (recipe.Url.ToString())
                JsonExport.Recipe.ImageUri = (recipe.ImageUrl.ToString())
                JsonExport.Recipe.Ingredients = recipe.Ingredients |> List.map ingredient
            }
        {
            JsonExport.RecommendationMethod.Id = id
            JsonExport.RecommendationMethod.Name = name
            JsonExport.RecommendationMethod.Recommendations = recommendations |> List.map recipe
        }
        
    [
        method "f2v-256-10-tf-idf-cal" "Food2Vec 256/10 (TF-IDF weighted mean, Calibration)" calibratedWithElectionTfIdfWeightedWordToVec256Results calibratedWithElectionTfIdfWeightedWordToVec256ResultsMs ((findMaxSimilarity foodstuffIds) >> Distance)
        method "tf-idf-cal" "TF-IDF with structured data (Calibration)" calibratedTfIdfResults calibratedTfIdfResultsMs (fun i -> List.exists (fun a -> a.FoodstuffId = i.Amount.FoodstuffId) foodstuffAmounts |> Binary)
        method "f2v-256-10-tf-idf-mmr" "Food2Vec 256/10 (TF-IDF weighted mean, MMR)" diversifiedTfIdfWeightedWordToVec256Results diversifiedTfIdfWeightedWordToVec256ResultsMs ((findMaxSimilarity foodstuffIds) >> Distance)
        method "tf-idf-mmr" "TF-IDF with structured data (MMR)" diversifiedTfIdfResults diversifiedTfIdfResultsMs (fun i -> List.exists (fun a -> a.FoodstuffId = i.Amount.FoodstuffId) foodstuffAmounts |> Binary)
        method "jaccard" "Jaccard" jacccardResults jaccardResultsMs (fun i -> List.exists (fun id -> id = i.Amount.FoodstuffId) foodstuffIds |> Binary)
        method "tf-idf" "TF-IDF with structured data" plainTfIdfResults plainTfIdfResultsMs (fun i -> List.exists (fun a -> a.FoodstuffId = i.Amount.FoodstuffId) foodstuffAmounts |> Binary)
        method "tf-idf-text" "TF-IDF with text data" textTfIdfResults textTfIdfResultsMs (fun i -> List.exists (fun (t: string) -> i.DisplayLine.ToLowerInvariant().Contains(t)) foodstuffWords |> Binary)
        method "f2v-256-10" "Food2Vec 256/10 (mean)" plainWordToVecResults plainWordToVecResultsMs ((findMaxSimilarity foodstuffIds) >> Distance)
        method "f2v-256-10-tf" "Food2Vec 256/10 (TF weighted mean)" tfWeightedWordToVecResults tfWeightedWordToVecResultsMs ((findMaxSimilarity foodstuffIds) >> Distance)
        method "f2v-256-10-tf-idf" "Food2Vec 256/10 (TF-IDF weighted mean)" tfIdfWeightedWordToVecResults tfIdfWeightedWordToVecResultsMs ((findMaxSimilarity foodstuffIds) >> Distance)
        method "f2c-32-8-tf-idf-mmr""Food2Vec 32/8 (TF-IDF weighted mean, MMR)" diversifiedTfIdfWeightedWordToVec32Results diversifiedTfIdfWeightedWordToVec32ResultsMs ((findMaxSimilarity foodstuffIds) >> Distance)
    ]
    
[<EntryPoint>]
let main argv =
    let recipes = DataStore.getRecipes ()
    let food2vecData256 = Data.loadFoodstuffVectors "vectors-256.txt"
    let food2vecData32 = Data.loadFoodstuffVectors "vectors-32.txt"
    let run description input amounts words =
        let methods = showRecommendations recipes food2vecData32 food2vecData256 amounts words
        {
            JsonExport.RecommendationScenario.Description = description
            JsonExport.RecommendationScenario.Input = input
            JsonExport.RecommendationScenario.Recommendations = methods
        }
    
    let scenarios =
        [    
            run
                "You want to cook just a single recipe, not more. You are shopping and below ingredients are in your basket. You would like to know what are your options, possibly as many diverse options as possible."
                [
                    "beef (1 pound)"
                    "bell peppers (4 pieces)"
                    "mushrooms (5 pieces)"
                ]
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
                "In this scenario, just evaluate whole methods, not recipes. Which ones did a good job in recognizing that garam masala is much more important and rare than salt and pepper?"
                [
                    "salt (very common)"
                    "pepper (very common)"
                    "garam masala (uncommon)"
                ]
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
                "You have 2 ingredients, a lot of chicken breasts, but just a little of parmesan cheese. Which methods did the best job in recognizing that?"
                [
                    "chicken breasts (5 pounds)"
                    "parmesan cheese"
                ]
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
                "Now you have lots of parmesan cheese, but not as many chicken breasts. Which methods did the best job in recognizing the change compared to previous method and recommended recipes more suiting these amounts?"
                [
                    "chicken breasts"
                    "parmesan cheese (4 cups)"
                ]
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
                "You just started shopping and grabbed a few ingredients. Now you want to know what to buy next in order to cook multiple recipes, such that you use most of the ingredients. Mark good recommendations and evaluate whole methods."
                [
                    "beef (4 pounds)"
                    "carrots (5 pieces)"
                    "tomatoes (5 pieces)"
                    "yogurt (3 pieces)"
                    "bell peppers (3 pounds)"
                ]
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
                    "beef";
                    "carrots";
                    "tomatoes";
                    "yogurt";
                    "bell";
                    "peppers"
                ]
            run
                "You just started shopping and grabbed a few more ingredients. Now you want to know what to buy next in order to cook multiple recipes, such that you use most of the ingredients. Mark good recommendations and evaluate whole methods."
                [
                    "Beef (2 pounds)"
                    "Chicken breasts (2 pounds)"
                    "Green bell peppers (3 pieces)"
                    "Tomatoes (3 pieces)"
                    "Pasta (2 pounds)"
                    "Rice (1 pound)"
                    "Cheddar cheese (0.5 pound)"
                    "Heavy cream (2 cups)"
                    "Carrots (3 pieces)"
                    "Avocado (2 pieces)"
                ]
                [
                    {
                        Value = Some 2.0
                        Unit = Some "pound"
                        FoodstuffId = Guid("fa9a10a7-50ab-41ad-9b12-dfd1f9c4b241") // Beef
                    };
                    {
                        Value = Some 2.0
                        Unit = Some "pound"
                        FoodstuffId = Guid("cbd25042-ef0b-467f-8dfd-4ff70c2e5824") // Chicken breasts
                    };
                    {
                        Value = Some 3.0
                        Unit = Some "pieces"
                        FoodstuffId = Guid("c77e775b-d0c3-4ac2-8fe0-63e8a0f400a9") // Green bell pepper
                    };
                    {
                        Value = Some 3.0
                        Unit = Some "pieces"
                        FoodstuffId = Guid("241505a7-c6d7-4a7b-a913-aad0389c4606") // Tomatoes
                    };
                    {
                        Value = Some 2.0
                        Unit = Some "pounds"
                        FoodstuffId = Guid("1dd72985-3c83-4218-bdca-e74fe38e2a03") // Pasta
                    };
                    {
                        Value = Some 1.0
                        Unit = Some "pound"
                        FoodstuffId = Guid("1c5681bb-12af-4d53-b93b-a4e3f3b16893") // Rice
                    };
                    {
                        Value = Some 0.5
                        Unit = Some "pound"
                        FoodstuffId = Guid("20f8d6a5-77a9-44a2-a35c-5bfc5b431936") // Cheddar cheese
                    };
                    {
                        Value = Some 2.0
                        Unit = Some "cups"
                        FoodstuffId = Guid("c1d7cad4-2ded-46ff-b238-bfa24da78040") // Heavy cream
                    };
                    {
                        Value = Some 3.0
                        Unit = Some "pieces"
                        FoodstuffId = Guid("274f4bc5-63c8-4f46-aba1-a409b5e78dd4") // Carrots
                    };
                    {
                        Value = Some 2.0
                        Unit = Some "pieces"
                        FoodstuffId = Guid("7f95dc9e-1955-4fdb-a7c2-b9cef645ced8") // Avocado
                    };
                ]
                [
                    "beef";
                    "chicken";
                    "breast";
                    "bell";
                    "pepper";
                    "tomatoes";
                    "pasta";
                    "rice";
                    "cheddar";
                    "cheese";
                    "heavy";
                    "cream";
                    "carrots";
                    "avocado";
                ]
        ]
        
    printfn "%s" (Json.serialize scenarios)
        
        

// 
//    run
//        @"
//            Case 4: Input aiming to get 2 recipes recommended instead of just 1 combining everything.
//            User profile:
//                - chicken breasts (1 pounds)
//                - parmesan cheese (2 cups)
//                - garam masala (1 cup)
//                - chickpea (2 cups)
//        "
//        [
//            {
//                Value = Some 1.0
//                Unit = Some "pound"
//                FoodstuffId = Guid("cbd25042-ef0b-467f-8dfd-4ff70c2e5824") // Chicken breasts
//            };
//            {
//                Value = Some 2.0
//                Unit = Some "cups"
//                FoodstuffId = Guid("7dc3db3c-8422-473d-8344-2f8653157581") // Parmesan cheese
//            };
//            {
//                Value = Some 1.0
//                Unit = Some "cup"
//                FoodstuffId = Guid("24b1b115-07e9-4d8f-b0a1-a38639654b7d") // Garam masala
//            };
//            {
//                Value = Some 2.0
//                Unit = Some "cups"
//                FoodstuffId = Guid("b17a087c-dcd1-4bec-b481-00d2165fd18a") // Chickpeas
//            }
//        ]
//        [
//            "chicken";
//            "breasts";
//            "parmesan";
//            "cheese";
//            "chickpeas";
//            "garam";
//            "masala";
//        ]
//

//        

    
    0 // return an integer exit code
