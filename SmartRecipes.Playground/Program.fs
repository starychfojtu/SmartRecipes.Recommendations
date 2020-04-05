open SmartRecipes.Playground.Library
open SmartRecipes.Playground
open SmartRecipes.Playground.FoodToVector
open SmartRecipes.Playground.Model
open System

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
    
let showRecommendations recipes food2vecData input1 input2 input3 =
    let (firstMethodRecommendations, firstMs) = profilePerformance (fun () -> JaccardSimilarity.recommend recipes input1 |> Seq.take 10 |> Seq.toList)
    let (secondMethodRecommendations, secondMs) = profilePerformance (fun () -> TfIdfCosineSimilarityStructuredData.recommend recipes input2 |> Seq.take 10 |> Seq.toList)
    let (thirdMethodRecommendations, thirdMs) = profilePerformance (fun () -> TfIdfCosineSimilarityTextData.recommend recipes input3 |> Seq.take 10 |> Seq.toList)
    let (fourthMethodRecommendations, fourthMs) = profilePerformance (fun () -> TfIdfCosineSimilarityStructuredDataWithDynamicAmountAltering.recommend recipes input2 3 10)
    let (fifthMethodRecommendations, fifthMs) = profilePerformance (fun () -> TfIdfCosineSimilarityStructuredDataWithDiversity.recommend recipes input2 10)
    let (sixthMethodRecommendations, sixthMs) = profilePerformance (fun () -> FoodToVector.recommend food2vecData recipes (toInfoLessAmounts input1) |> Seq.take 10 |> Seq.toList)
    let (seventhMethodRecommendations, seventhMs) = profilePerformance (fun () -> FoodToVector.recommend food2vecData recipes input2 |> Seq.take 10 |> Seq.toList)
    
    let allRecipes = List.concat [
        firstMethodRecommendations;
        secondMethodRecommendations;
        thirdMethodRecommendations;
        fourthMethodRecommendations;
        fifthMethodRecommendations;
        sixthMethodRecommendations;
        seventhMethodRecommendations
    ]
    
    let counts =
        allRecipes
        |> List.groupBy (fun r -> r.Name)
        |> List.map (fun (name, recipes) -> (name, Seq.length recipes))
        |> List.sortByDescending second
        
    let findMaxSimilarity foodstuffIds ingredient =
        let toVector fId = Map.find fId food2vecData
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
    
    printMethod "Jaccard" firstMethodRecommendations firstMs (fun i -> List.exists (fun id -> id = i.Amount.FoodstuffId) input1 |> Binary)
    printMethod "TF-IDF with structured data" secondMethodRecommendations secondMs (fun i -> List.exists (fun a -> a.FoodstuffId = i.Amount.FoodstuffId) input2 |> Binary)
    printMethod "TF-IDF with text data" thirdMethodRecommendations thirdMs (fun i -> List.exists (fun (t: string) -> i.DisplayLine.ToLowerInvariant().Contains(t)) input3 |> Binary)
    printMethod "TF-IDF with structured data (Iterative)" fourthMethodRecommendations fourthMs (fun i -> List.exists (fun a -> a.FoodstuffId = i.Amount.FoodstuffId) input2 |> Binary)
    printMethod "TF-IDF with structured data (MMR)" fifthMethodRecommendations fifthMs (fun i -> List.exists (fun a -> a.FoodstuffId = i.Amount.FoodstuffId) input2 |> Binary)
    printMethod "Food2Vec (mean)" sixthMethodRecommendations sixthMs ((findMaxSimilarity input1) >> Distance)
    printMethod "Food2Vec (weighted mean)" seventhMethodRecommendations seventhMs ((findMaxSimilarity input1) >> Distance)
    
    printfn "</div>"
    printfn "<div style=\"clear: both;\"></div>"

let printHeader () =
    printfn "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\"><title>SmartRecipes recommendations</title></head><body style=\"width: 5000px\">"
    
let printFooter () =
    printfn "</body></html>"
    
[<EntryPoint>]
let main argv =
    printHeader ()
    
    let recipes = DataStore.getRecipes ()
    let food2vecData = Data.loadFoodstuffVectors "vectors.txt"
    let run introText input1 input2 input3 =
        printfn "-------------RUN-START-------------- <br>"
        printfn "<h1>%s</h1><br>" introText
        showRecommendations recipes food2vecData input1 input2 input3
        printfn "-------------RUN-END---------------- <br>"
    
    // INPUT: small number of ingredients
    //
    // Jaccard similarity recommends recipe with too few ingredients -> tries to minimize the union.
    // Structured one recommends 
    //
    run
        "Ingredient based - searching for ground beef and peppers."
        [
            Guid("fa9a10a7-50ab-41ad-9b12-dfd1f9c4b241"); // Beef
            Guid("27b43955-3361-48a1-b16f-9d339c808b20"); // Bell peppers
        ]
        [
            {
                Value = Some 2.0
                Unit = Some "pound"
                FoodstuffId = Guid("fa9a10a7-50ab-41ad-9b12-dfd1f9c4b241") // Beef
            };
            {
                Value = Some 4.0
                Unit = Some "pieces"
                FoodstuffId = Guid("27b43955-3361-48a1-b16f-9d339c808b20") // Bell peppers
            }
        ]
        [
            "ground";
            "beef";
            "bell";
            "peppers";
        ]
        
    // INPUT: aiming to get this recipe recommended - https://www.allrecipes.com/recipe/223042/chicken-parmesan/
    //
    // Database is polluted with a lot of chicken breast variations = hard to select for user which one he wants with the structured method.
    // Structured method again recommended first recipes with just a chicken breast.
    // Neither of them recommended desired recipe, but structured approach recommended close enough recipe - Garlic Chicken, which has parmesan
    //
    // Unstructured method recommended first a big recipe (pizza with lot of ingredient), however the parmesan wasn't used directly.
    // The ingredient was parmesan cheese sauce. This could not happen with structured data.
    //
    // Most of the recipes don't have parmesan at all, since it is a very common ingredient.
    run
        "Aiming to get chicken parmesan without much amount info."
        [
            Guid("cbd25042-ef0b-467f-8dfd-4ff70c2e5824"); // Chicken breasts
            Guid("7dc3db3c-8422-473d-8344-2f8653157581"); // Parmesan cheese
        ]
        [
            {
                Value = Some 2.0
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
        
    // Section with https://www.allrecipes.com/recipe/223042/chicken-parmesan/,
    // It is a simple recipe with common ingredients, not much of them
        
    // INPUT: aiming to get this recipe recommended - https://www.allrecipes.com/recipe/223042/chicken-parmesan/,
    // but providing more info about amounts in structured method
    //
    // I put crazy values there, since small ones did not change much, but it really helped.
    // The structured method provided very relevant results, even tho the desired recipe still misses because of chicken ingredient mismatch.
    // TODO: put more weight on the ingredient amounts.
    run
        "Aiming to get chicken parmesan with more amount info."
        [
            Guid("cbd25042-ef0b-467f-8dfd-4ff70c2e5824"); // Chicken breasts
            Guid("7dc3db3c-8422-473d-8344-2f8653157581"); // Parmesan cheese
        ]
        [
            {
                Value = Some 30.0
                Unit = Some "pound"
                FoodstuffId = Guid("cbd25042-ef0b-467f-8dfd-4ff70c2e5824") // Chicken breasts
            };
            {
                Value = Some 100.0
                Unit = Some "cup"
                FoodstuffId = Guid("7dc3db3c-8422-473d-8344-2f8653157581") // Parmesan cheese
            }
        ]
        [
            "chicken";
            "breasts";
            "parmesan";
            "cheese";
        ]
        
    // Section with https://www.allrecipes.com/recipe/90105/butter-chickpea-curry/
    // It is a bit harder recipe with specific ingredients
        
    // INPUT: aiming to get this recipe recommended - https://www.allrecipes.com/recipe/90105/butter-chickpea-curry/
    //
    // Here we can see that we got pretty similar results from both methods - 2 recipes were recommended by both.
    //
    // Structured method did recommend desired recipe. We can see pretty great results for this case.
    //
    // Text based method did recommend masala recipes first, but also lot of them without any chickpeas.
    run
        "Aiming to get butter chickpea curry."
        [
            Guid("24b1b115-07e9-4d8f-b0a1-a38639654b7d"); // Garam masala
            Guid("b17a087c-dcd1-4bec-b481-00d2165fd18a"); // Chickpeas
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
                FoodstuffId = Guid("b17a087c-dcd1-4bec-b481-00d2165fd18a") // Chickpeas
            }
        ]
        [
            "chickpeas";
            "garam";
            "masala";
        ]
        
    // INPUT https://www.allrecipes.com/recipe/223042/chicken-parmesan/ and https://www.allrecipes.com/recipe/90105/butter-chickpea-curry/
    // Both recipes were recommended individually, this will merge their inputs together
    //
    // Structured one recommended mainly Garam masala, since it is special (which is pretty good on one hand, if I bought garam masala, I probably want to use it).
    // However, when I put a lot of value/unit on chicken and parmasan, it got recommended over masala.
    // TODO: I might want to tune spices like garam masala or other super rare ingredients.
    //
    // Text based method did recommend also only Garam masala, but there is no way to tune it from the user perspective by amounts.
    // On the other hand, it did recommend some masala + chicken recipe (combining accross recipe), probably due to bad structured data for chicken.
    run
        "Aiming to get chicken parmesan and butter chickpea curry."
        [
            Guid("cbd25042-ef0b-467f-8dfd-4ff70c2e5824"); // Chicken breasts
            Guid("7dc3db3c-8422-473d-8344-2f8653157581"); // Parmesan cheese
            Guid("24b1b115-07e9-4d8f-b0a1-a38639654b7d"); // Garam masala
            Guid("b17a087c-dcd1-4bec-b481-00d2165fd18a"); // Chickpeas
        ]    
        [
            {
                Value = Some 2.0
                Unit = Some "pound"
                FoodstuffId = Guid("cbd25042-ef0b-467f-8dfd-4ff70c2e5824") // Chicken breasts
            };
            {
                Value = Some 1.0
                Unit = Some "pound"
                FoodstuffId = Guid("7dc3db3c-8422-473d-8344-2f8653157581") // Parmesan cheese
            };
            {
                Value = None
                Unit = None
                FoodstuffId = Guid("24b1b115-07e9-4d8f-b0a1-a38639654b7d") // Garam masala
            };
            {
                Value = Some 1.0
                Unit = Some "pound"
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
        
        
    // Section - much more ingredients, simulating real shopping list
    //
    // Pretty disappointing results, pretty low on matches, not really much ingredients combined (probably due to lack of such recipes?).
    run
        "Simulating classic shopping list."
        [
            Guid("fa9a10a7-50ab-41ad-9b12-dfd1f9c4b241"); // Beef
            Guid("274f4bc5-63c8-4f46-aba1-a409b5e78dd4"); // Carrots
            Guid("241505a7-c6d7-4a7b-a913-aad0389c4606"); // Tomatoes
            Guid("80a641dd-f9a3-4484-ba6e-466ceda111f1"); // Yogurt
            Guid("04c7dad3-657b-4fb6-8df9-a4cc3fb30408"); // Potato
            Guid("27b43955-3361-48a1-b16f-9d339c808b20"); // Bell peppers
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
                Unit = Some "pound"
                FoodstuffId = Guid("04c7dad3-657b-4fb6-8df9-a4cc3fb30408") // Potato
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
            "potato";
            "bell";
            "peppers"
        ]
        
    // INPUT: Lasagna basket
    //
    // Neither did recommend lasagna directly, but we can see pretty good recommendations, similar foods to Lasagna even.
    //
    // Structured based recommended recipe even with 5/5 hits, basically ideal recipe.
    //
    // Text based is noticeably worse.
    run
        "Lasagna basket."
        [
            Guid("fa9a10a7-50ab-41ad-9b12-dfd1f9c4b241"); // Beef
            Guid("274f4bc5-63c8-4f46-aba1-a409b5e78dd4"); // Carrots
            Guid("241505a7-c6d7-4a7b-a913-aad0389c4606"); // Tomatoes
            Guid("7dc3db3c-8422-473d-8344-2f8653157581"); // Parmesan cheese
        ]
        [
            {
                Value = None
                Unit = None
                FoodstuffId = Guid("fa9a10a7-50ab-41ad-9b12-dfd1f9c4b241") // Beef
            };
            {
                Value = None
                Unit = None
                FoodstuffId = Guid("274f4bc5-63c8-4f46-aba1-a409b5e78dd4") // Carrots
            };
            {
                Value = None
                Unit = None
                FoodstuffId = Guid("241505a7-c6d7-4a7b-a913-aad0389c4606") // Tomatoes
            };
            {
                Value = None
                Unit = None
                FoodstuffId = Guid("7dc3db3c-8422-473d-8344-2f8653157581") // Parmesan cheese
            };
        ]
        [
            "beef";
            "ground";
            "carrot";
            "tomatoes";
            "parmesan";
        ]
        
    printFooter ()
        
    // Overall impressions
    // Structured:
    // Pros:
    // - ability to define exactly the ingredient I have
    // - ability to put amount of given ingredient (which proven to be very relevant for the results)
    // - overall good recommendations with decent hit rate
    // Cons:
    // - sometimes single-ingredient meals, too strict
    // - impossibility to define what is not in database
    // - heavily dependent on data quality (deduplication)
    // - no sense of similarity between foodstuffs (red vs white onion)
    //
    // Text-based:
    // Pros:
    // - ability to define more vaguely what is the content (red vs white onion will still hit onion word)
    // - not too dependent on data quality
    // Cons:
    // - no ability to define exactly what I have (the first advantage is also a disadvantage, potato vs potato chips)
    // - not really precise matches, overall worse recommendations
    //
    // Both methods failed on large basket.
    // user has to pick recipes one-by-one, we can subtract the chosen recipe from the shopping list, but our algorithms are not designed
    // for proposing multiple complementary recipes at once.
    
    // TODO:
    // - 
    // - (Maybe, probably not now)SVD ?? changing the basis from foodstuff to foodstuff groups which can be substituted (red onion, vs white onion),
    //     problem is that we have to decide this on binary basis, but only other solution is 2vec probably
    
    0 // return an integer exit code
