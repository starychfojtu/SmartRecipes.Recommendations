module SmartRecipes.Playground.FoodToVector

open System
open SmartRecipes.Playground.Model

let magnitude vector =
    vector
    |> Array.map (fun v -> Math.Pow(v, 2.0))
    |> Array.sum
    |> Math.Sqrt
    
let dotProduct v1 v2 =
    Array.zip v1 v2
    |> Array.map (fun (a, b) -> a * b)
    |> Array.sum
    
let cosineSimilarity v1 v2 =
    (dotProduct v1 v2) / ((magnitude v1) * (magnitude v2))

let mean a b =
    Array.zip a b
    |> Array.map (fun (a, b) -> (a + b) / 2.0)

let vectorize foodstuffVectors foodstuffIds =
    foodstuffIds
    |> List.map (fun id -> Map.find id foodstuffVectors)
    |> List.reduce mean

let recommend foodstuffVectors recipes foodstuffAmounts =
    let inputFoodstuffIds = foodstuffAmounts |> List.map (fun a -> a.FoodstuffId)
    let inputVector = vectorize foodstuffVectors inputFoodstuffIds
    
    recipes
    |> List.map (fun (r: Recipe) ->
        let foodstuffIds = r.Ingredients |> List.map (fun i -> i.Amount.FoodstuffId)
        let vector = vectorize foodstuffVectors foodstuffIds
        let distance = cosineSimilarity vector inputVector
        (r, vector, distance)
    )
    |> List.sortByDescending (fun (_, _, distance) -> distance)