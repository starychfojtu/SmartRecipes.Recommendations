module SmartRecipes.Playground.TfIdfCosineSimilarityStructuredData

open System
open SmartRecipes.Playground
open Model
open Library

type DataSetStatistics = {
    NumberOfRecipes: float
    FoodstuffFrequencies: Map<Guid, float>
    InverseIndex: Map<Guid, Recipe list>
}

// Term frequency ~ ingredient frequency in single recipe is 1 by default, but for chosen and most used unit, its amount is taken if present
let termFrequency foodstuffAmount =
    let amount = 
        match foodstuffAmount.Unit with
        | Some "cup" -> foodstuffAmount.Value |> Option.map ((*) 2.0)
        | Some "pieces" -> foodstuffAmount.Value |> Option.map ((*) 2.0)
        | Some "pound" -> foodstuffAmount.Value |> Option.map ((*) 7.0)
        | Some _ -> None
        | None -> None
        
    Option.defaultValue 1.0 amount

let tfIdf statistics foodstuffAmount =
    let documentFrequency = Map.find foodstuffAmount.FoodstuffId statistics.FoodstuffFrequencies
    foodstuffAmount.FoodstuffId, Math.Log10(statistics.NumberOfRecipes / documentFrequency) * (termFrequency foodstuffAmount)
    
let vectorize statistics foodstuffAmounts: Vector =
    List.map (tfIdf statistics) foodstuffAmounts |> Map.ofList
    
let getInverseIndex (recipes: Recipe list): Map<Guid, Recipe list> =
    recipes
    |> List.collect (fun r -> r.Ingredients |> List.map (fun i -> (i.Amount.FoodstuffId, r)))
    |> List.groupBy (fun (foodstuffId, _) -> foodstuffId)
    |> List.map (fun (foodstuffId, values) ->
        let distinctRecipes =
            values
            |> List.map (fun (_, r) -> r)
            |> List.distinctBy (fun r -> r.Id)
        (foodstuffId, distinctRecipes))
    |> Map.ofList
    
let computeStatistics (recipes: Recipe list) =
    let recipeCount = List.length recipes |> float
    let inverseIndex = getInverseIndex recipes
    let foodstuffFrequencies =
        inverseIndex
        |> Map.map (fun k v -> List.length v |> float)
        
    {
        NumberOfRecipes = recipeCount
        FoodstuffFrequencies = foodstuffFrequencies
        InverseIndex = inverseIndex
    }
    
let vectorizeRecipe statistics r =
    r.Ingredients |> List.map (fun i -> i.Amount) |> vectorize statistics
    
let recommend recipes foodstuffAmounts =
    let statistics = computeStatistics recipes
    let inputVector = vectorize statistics foodstuffAmounts
    let relevantRecipes =
        foodstuffAmounts
        |> List.collect (fun a -> Map.find a.FoodstuffId statistics.InverseIndex)
        |> List.distinctBy (fun r -> r.Id)
        
    let recipesToRecommend = 
        relevantRecipes
        |> Seq.map (fun r -> (r, vectorizeRecipe statistics r |> cosineSimilarity inputVector))
        |> Seq.sortByDescending second
        |> Seq.map first
        
    recipesToRecommend