module SmartRecipes.Playground.FoodToVector

open System
open System.IO
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
    
module Data =
    let readLines (filePath : string) =
        seq {
            use sr = new StreamReader(filePath)
            while not sr.EndOfStream do
                yield sr.ReadLine()
        }
        
    let parseLine (line: string) =
        let parts = line.Split(" ")
        let foodstuffId = Guid(parts.[0])
        let vector =
            parts
            |> Seq.skip 1
            |> Seq.map float
            |> Seq.toArray
            
        (foodstuffId, vector)
    
    let loadFoodstuffVectors file =
         readLines file
         |> Seq.map parseLine
         |> Map.ofSeq

let recommend foodstuffVectors recipes foodstuffIds =
    let inputVector = vectorize foodstuffVectors foodstuffIds
    
    recipes
    |> List.map (fun (r: Recipe) ->
        let foodstuffIds = r.Ingredients |> List.map (fun i -> i.Amount.FoodstuffId)
        let vector = vectorize foodstuffVectors foodstuffIds
        let distance = cosineSimilarity vector inputVector
        (r, vector, distance)
    )
    |> List.sortByDescending (fun (_, _, distance) -> distance)
    |> List.map (fun (r, _, _) -> r)