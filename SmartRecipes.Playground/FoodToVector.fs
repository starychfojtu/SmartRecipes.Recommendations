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
    
type WeightedVector = {
    Vector: float[]
    Weight: float
}

let mean (vectors: WeightedVector list) =
    let count = List.length vectors |> float
    let dimension = Array.length vectors.[0].Vector
    let result: float[] = Array.zeroCreate dimension
    for d in 0..dimension-1 do
        let sum = vectors |> List.map (fun v -> v.Vector.[d] * v.Weight) |> List.sum
        let mean = sum / count
        result.[d] <- mean
        
    result

type WeightedFoodstuff = {
    FoodstuffId: Guid
    Weight: float
}

let vectorize foodstuffVectors weightedFoodstuffs =
    weightedFoodstuffs
    |> List.map (fun f -> { Vector = Map.find f.FoodstuffId foodstuffVectors; Weight = f.Weight })
    |> mean
    
module Data =
    let readLines (filePath : string) =
        seq {
            use sr = new StreamReader(filePath)
            sr.ReadLine() |> ignore // Skip header.
            sr.ReadLine() |> ignore // Skip space token.
            while not sr.EndOfStream do
                yield sr.ReadLine()
        }
        
    let parseLine (line: string) =
        let parts = line.Split(" ")
        let foodstuffId = Guid(parts.[0])
        let vector =
            parts
            |> Seq.skip 1
            |> Seq.filter (fun s -> s <> "")
            |> Seq.map float
            |> Seq.toArray
            
        (foodstuffId, vector)
    
    let loadFoodstuffVectors file =
         readLines file
         |> Seq.map parseLine
         |> Map.ofSeq

let recommend foodstuffVectors recipes foodstuffAmounts =
    let amountsToWeightedFoodstuffs = List.map (fun (a: FoodstuffAmount) -> { FoodstuffId = a.FoodstuffId; Weight = TfIdfCosineSimilarityStructuredData.termFrequency a })
    let weightedFoodstuffs = amountsToWeightedFoodstuffs foodstuffAmounts
    let inputVector = vectorize foodstuffVectors weightedFoodstuffs
    
    recipes
    |> List.map (fun (r: Recipe) ->
        let foodstuffAmounts = r.Ingredients |> List.map (fun i -> i.Amount)
        let vector = vectorize foodstuffVectors (amountsToWeightedFoodstuffs foodstuffAmounts)
        let distance = cosineSimilarity vector inputVector
        (r, vector, distance)
    )
    |> List.sortByDescending (fun (_, _, distance) -> distance)
    |> List.map (fun (r, _, _) -> r)