module SmartRecipes.Playground.TfIdfCosineSimilarityStructuredData

open System
open SmartRecipes.Playground
open Model
open Library

// This method works well, recommends very similar recipes.
// Since it uses structured data, it gives a relevant implementation of term frequency, that can be improved.
// Problem is that it recommends too similar recipes and suffers from poorly cleaned structured data.
// Consider "lean beef" vs "beef", this method gives those ingredients no similarity.
// Therefore we are forced to choose (which we might anyway) to distinguish such data directly on them = changing the data.

type DataSetStatistics = {
    NumberOfRecipes: float
    FoodstuffFrequencies: Map<Guid, float>
}

// TODO: Notes in improvements
// - put much more weight on termFrequency (exponential)
// - penalize recipes with small number of ingredients (especially 1-3, some normal distribution, to also penalize big ones ?)
// - foodstuff similarity (food2vec) to give at least some sense to red vs. white onion - maybe even consider some foodstuff for identic ?
//     - this might lead to different method (like finding the nearest ingredient for every ingredient in the other recipe)

// Term frequency ~ ingredient frequency in single recipe is 1 by default, but for chosen and most used unit, its amount is taken if present
let termFrequency foodstuffAmount =
    let amount = 
        match foodstuffAmount.Unit with
        | Some "cup" -> foodstuffAmount.Value
        | Some "pieces" -> foodstuffAmount.Value
        | Some "pound" -> foodstuffAmount.Value
        | Some _ -> None
        | None -> None
        
    Option.defaultValue 1.0 amount

let tfIdf statistics foodstuffAmount =
    let documentFrequency = Map.find foodstuffAmount.FoodstuffId statistics.FoodstuffFrequencies
    foodstuffAmount.FoodstuffId, Math.Log10(statistics.NumberOfRecipes / documentFrequency) * (termFrequency foodstuffAmount)
    
type Vector = Map<Guid, float>

let vectorize statistics foodstuffAmounts: Vector =
    List.map (tfIdf statistics) foodstuffAmounts |> Map.ofList
    
let computeStatistics (recipes: Recipe list) =
    let recipeCount = List.length recipes
    let foodstuffFrequencies =
        recipes
        |> List.collect (fun r -> r.Ingredients)
        |> List.groupBy (fun i -> i.Amount.FoodstuffId)
        |> List.map (mapSecond (List.distinctBy (fun i -> i.RecipeId) >> List.length >> float))
        |> Map.ofList
        
    { NumberOfRecipes = float recipeCount; FoodstuffFrequencies = foodstuffFrequencies }
    
let vectorizeRecipe statistics r =
    r.Ingredients |> List.map (fun i -> i.Amount) |> vectorize statistics
    
let recommend recipes foodstuffAmounts =
    let statistics = computeStatistics recipes
    
    let inputVector = vectorize statistics foodstuffAmounts
    let recipesToRecommend = 
        recipes
        |> Seq.map (fun r -> (r, vectorizeRecipe statistics r |> cosineSimilarity inputVector))
        |> Seq.sortByDescending second
        |> Seq.map first
        
    recipesToRecommend