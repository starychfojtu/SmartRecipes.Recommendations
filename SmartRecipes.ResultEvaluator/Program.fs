// Learn more about F# at http://fsharp.org

open System
open SmartRecipes.Playground
    
let euclideanDistance v1 v2 =
    Array.zip v1 v2
    |> Array.map (fun (a, b) -> Math.Pow(a - b, 2.0))
    |> Array.sum
    |> Math.Sqrt
    
let sortByNearest vector vectors =
    vectors
    |> Map.toList
    |> Seq.map (fun (f, v) -> (f, FoodToVector.cosineSimilarity v vector))
    |> Seq.sortByDescending (fun (_, d) -> d)

[<EntryPoint>]
let main argv =
    let foodstuffByIds =
        DataStore.getFoodstuff()
        |> List.map (fun f -> (f.Id, f))
        |> Map.ofList

//    let ingredients = DataStore.getIngredients()
//
//    let recipeIdsByFoodstuffId =
//        ingredients
//        |> List.groupBy (fun i -> i.Amount.FoodstuffId)
//        |> List.map (fun (fId, is) -> (fId, is |> List.map (fun i -> i.RecipeId)))
//        |> Map.ofList
//
//    let foodstuffIdsByRecipeId =
//        ingredients
//        |> List.groupBy (fun i -> i.RecipeId)
//        |> List.map (fun (rId, is) ->
//            (rId,
//             is
//             |> List.map (fun i -> i.Amount.FoodstuffId)
//             |> Set.ofList))
//        |> Map.ofList

    let fileLocation = argv.[0]
    let vectors = FoodToVector.Data.loadFoodstuffVectors fileLocation
    
    printfn "WordId:"
    let mutable word = Console.ReadLine()
    
    while (word <> "") do
        let redOnionId = Guid.Parse(word)
        let redOnionVector = Map.find redOnionId vectors
        let nearest = sortByNearest redOnionVector vectors |> Seq.take 30
        
        for (foodstuffId, distance) in nearest do
            let foodstuff = Map.find foodstuffId foodstuffByIds
            printfn "%s %f" foodstuff.Name distance
        
        printfn "WordId:"
        word <- Console.ReadLine()
        
        
    0
