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
            FoodstuffId = Guid("cbd25042-ef0b-467f-8dfd-4ff70c2e5824") // Chicken breast
        };
        {
            Value = None
            Unit = None
            FoodstuffId = Guid("1c5681bb-12af-4d53-b93b-a4e3f3b16893") // Rice
        }
    ]
    
    let firstBest1 = List.item 0 firstMethodRecommendations
    let firstBest2 = List.item 1 firstMethodRecommendations
    let firstBest3 = List.item 2 firstMethodRecommendations
    let firstBest4 = List.item 3 firstMethodRecommendations
    let firstBest5 = List.item 4 firstMethodRecommendations
    
    let secondMethodRecommendations = TfIdfCosineSimilarityTextData.recommend recipes [
        "chicken";
        "breasts";
        "rice"
    ]
    
    let secondBest1 = List.item 0 secondMethodRecommendations
    let secondBest2 = List.item 1 secondMethodRecommendations
    let secondBest3 = List.item 2 secondMethodRecommendations
    let secondBest4 = List.item 3 secondMethodRecommendations
    let secondBest5 = List.item 4 secondMethodRecommendations
    
    0 // return an integer exit code
