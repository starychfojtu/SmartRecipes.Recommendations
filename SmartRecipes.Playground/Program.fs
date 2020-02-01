// Learn more about F# at http://fsharp.org

open System

open SmartRecipes.Playground
open Model
// TO TRY:
// TF-IDF
// Cosine similarity

type DataSetStatistics = {
    NumberOfRecipes: float
    FoodstuffFrequencies: Map<Guid, float>
}

let shoppingList = [
    
]

// Term frequency ~ ingredient frequency in single recipe is 1 by default, but for chosen and most used unit, its amount is taken if present
let termFrequency ingredient =
    let amount = 
        match ingredient.Unit with
        | Some "cup" -> ingredient.Amount
        | Some "pieces" -> ingredient.Amount
        | Some "pound" -> ingredient.Amount
        | Some _ -> None
        | None -> None
        
    Option.defaultValue 1.0 amount

let tfIdf statistics ingredient =
    let documentFrequency = Map.find ingredient.FoodstuffId statistics.FoodstuffFrequencies
    ingredient.FoodstuffId, Math.Log10(statistics.NumberOfRecipes / documentFrequency) * (termFrequency ingredient)
    
type Vector = Map<Guid, float>

let vectorize statistics recipe: Vector =
    List.map (tfIdf statistics) recipe.Ingredients |> Map.ofList
    
let mapSecond f (fst, snd) = (fst, f snd)
let first (fst, _) = fst
let second (_, snd) = snd
    
let computeStatistics (recipes: Recipe list) =
    let recipeCount = List.length recipes
    let foodstuffFrequencies =
        recipes
        |> List.collect (fun r -> r.Ingredients)
        |> List.groupBy (fun i -> i.FoodstuffId)
        |> List.map (mapSecond (List.distinctBy (fun i -> i.RecipeId) >> List.length >> float))
        |> Map.ofList
        
    { NumberOfRecipes = float recipeCount; FoodstuffFrequencies = foodstuffFrequencies }
    
let magnitude vector =
    vector
    |> Map.toList
    |> List.map (second)
    |> List.map (fun v -> Math.Pow(v, 2.0))
    |> List.sum
    |> Math.Sqrt
    
let dotProduct v1 v2 =
    let getKeys map = Map.toSeq map |> Seq.map first
    Seq.append (getKeys v1) (getKeys v2)
    |> Seq.distinct
    |> Seq.map (fun k ->
        match (Map.tryFind k v1), (Map.tryFind k v2) with
        | Some a, Some b -> Some <| a * b
        | _ -> None)
    |> Seq.collect (function Some a -> [a] | None -> [])
    |> Seq.sum
    
let cosineSimilarity v1 v2 =
    (dotProduct v1 v2) / ((magnitude v1) * (magnitude v2))
    
[<EntryPoint>]
let main argv =
    let recipes = DataStore.getRecipes ()
    let statistics = computeStatistics recipes
    
    let recipe = List.item 20 recipes
    let inputVector = vectorize statistics recipe
    
    let topRecipesForFirstOne =
        recipes
        |> List.map (fun r -> (r, vectorize statistics r |> cosineSimilarity inputVector))
        |> List.sortByDescending second
        |> List.take 10
        
    let best1 = List.item 1 topRecipesForFirstOne
    let best2 = List.item 2 topRecipesForFirstOne
    let best3 = List.item 3 topRecipesForFirstOne
    let best4 = List.item 4 topRecipesForFirstOne
    0 // return an integer exit code
