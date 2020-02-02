open System
open SmartRecipes.Playground

// Learn more about F# at http://fsharp.org


    
[<EntryPoint>]
let main argv =
    let recipes = DataStore.getRecipes ()
    let firstMethodRecommendations = TfIdfCosineSimilarityStructuredData.recommend recipes [
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
    
    let firstBest1 = List.item 0 firstMethodRecommendations
    let firstBest2 = List.item 1 firstMethodRecommendations
    let firstBest3 = List.item 2 firstMethodRecommendations
    let firstBest4 = List.item 3 firstMethodRecommendations
    let firstBest5 = List.item 4 firstMethodRecommendations
    
    let (secondInputVector, secondMethodRecommendations) = TfIdfCosineSimilarityTextData.recommend recipes [
        "ground";
        "beef";
        "peppers";
    ]
    
    let secondBest1 = List.item 0 secondMethodRecommendations
    let secondBest2 = List.item 1 secondMethodRecommendations
    let secondBest3 = List.item 2 secondMethodRecommendations
    let secondBest4 = List.item 3 secondMethodRecommendations
    let secondBest5 = List.item 4 secondMethodRecommendations
    
    0 // return an integer exit code
