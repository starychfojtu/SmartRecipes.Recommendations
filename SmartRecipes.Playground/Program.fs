open System
open SmartRecipes.Playground.Library
open SmartRecipes.Playground
open SmartRecipes.Playground.Model

// Learn more about F# at http://fsharp.org

let printRecipe doesIngredientMatch (recipe: Recipe) =
    printfn "Name: %s" recipe.Name
    printfn "Ingredients: "
    for ingredient in recipe.Ingredients do
        printfn "[%s] %s" (if doesIngredientMatch ingredient then "X" else "") ingredient.DisplayLine
    printfn ""
    
let printRecipes doesIngredientMatch recipes =
    for recipe in recipes do
        printRecipe doesIngredientMatch recipe
    
let showRecommendations recipes input1 input2 =
    let firstMethodRecommendations = TfIdfCosineSimilarityStructuredData.recommend recipes input1
    let secondMethodRecommendations = TfIdfCosineSimilarityTextData.recommend recipes input2
    
    let allRecipes = List.concat [ firstMethodRecommendations; secondMethodRecommendations ]
    let counts =
        allRecipes
        |> List.groupBy (fun r -> r.Name)
        |> List.map (fun (name, recipes) -> (name, Seq.length recipes))
        |> List.sortByDescending second
    
    printfn "Overall statistics"
    for (recipe, count) in counts do
        printfn "Recipe: %s; Count: %i" recipe count
    printfn ""
    
    printfn "---------------------------"
    printfn "TF-IDF with structured data"
    printfn "---------------------------"
    printRecipes (fun i -> List.exists (fun a -> a.FoodstuffId = i.Amount.FoodstuffId) input1) firstMethodRecommendations
    printfn ""
    
    printfn "---------------------------"
    printfn "TF-IDF with text data"
    printfn "---------------------------"
    printRecipes (fun i -> List.exists (fun (t: string) -> i.DisplayLine.ToLowerInvariant().Contains(t)) input2) secondMethodRecommendations
    printfn ""

    
[<EntryPoint>]
let main argv =
    let recipes = DataStore.getRecipes ()
    let run introText input1 input2 =
        printfn "-------------RUN-START--------------"
        printfn introText
        showRecommendations recipes input1 input2
        printfn "-------------RUN-END----------------"
    
    // INPUT: small number of ingredients
    //
    // TOP 1 for structured is a meat made only of beef -> not using all ingredients
    // TOP 1 for text-based is much better
    run
        "Ingredient based - searching for ground beef and peppers."
        [
            {
                Value = Some 2.0
                Unit = Some "pound"
                FoodstuffId = Guid("fa9a10a7-50ab-41ad-9b12-dfd1f9c4b241") // Beef
            };
            {
                Value = None
                Unit = None
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
        
    // INPUT: aiming to get this recipe recommended - https://www.allrecipes.com/recipe/223042/chicken-parmesan/,
    // but providing more info about amounts in structured method
    //
    // I put crazy values there, since small ones did not change much, but it really helped.
    // The structured method provided very relevant results, even tho the desired recipe still misses because of chicken ingredient mismatch.
    // TODO: put more weight on the ingredient amounts.
    run
        "Aiming to get chicken parmesan with more amount info."
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
        
        
    // Section - much more ingredients, simulating real shopping list
    //
    // Pretty disappointing results, pretty low on matches, not really much ingredients combined (probably due to lack of such recipes?).
    run
        "Simulating classic shopping list."
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
                FoodstuffId = Guid("80a641dd-f9a3-4484-ba6e-466ceda111f1") // Yogurt
            };
            {
                Value = None
                Unit = None
                FoodstuffId = Guid("04c7dad3-657b-4fb6-8df9-a4cc3fb30408") // Potato
            };
            {
                Value = None
                Unit = None
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
    
    0 // return an integer exit code
