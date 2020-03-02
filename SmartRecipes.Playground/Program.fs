open System
open SmartRecipes.Playground.Library
open SmartRecipes.Playground
open SmartRecipes.Playground
open SmartRecipes.Playground.Model

// Learn more about F# at http://fsharp.org

let printRecipe (recipe: Recipe) =
    printfn "Name: %s" recipe.Name
    printfn "Ingredients: "
    for ingredient in recipe.Ingredients do
        printfn "%s" ingredient.DisplayLine
    printfn ""
    
let printRecipes recipes =
    for recipe in recipes do
        printRecipe recipe
    
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
    printRecipes firstMethodRecommendations
    printfn ""
    
    printfn "---------------------------"
    printfn "TF-IDF with text data"
    printfn "---------------------------"
    printRecipes secondMethodRecommendations
    printfn ""

    
[<EntryPoint>]
let main argv =
    let recipes = DataStore.getRecipes ()
    let run introText input1 input2 =
        printfn "-------------RUN-START--------------"
        printfn introText
        showRecommendations recipes input1 input2
        printfn "-------------RUN-END----------------"
    
    // Notes on this run:
    // INPUT: small number of ingredients
    //
    // TOP 1 for structured is a meat made only of beef -> not using all ingredients
    // TOP 1 for text-based is much better
    run
        "Ingredient based - searching for ground beef and peppers"
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
    
    0 // return an integer exit code
