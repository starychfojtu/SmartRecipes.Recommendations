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
    
let mapSecond f (fst, snd) = (fst, f snd)
let first (fst, _) = fst
let second (_, snd) = snd
    
let computeStatistics (recipes: Recipe list) =
    let recipeCount = List.length recipes
    let foodstuffFrequencies =
        recipes
        |> List.collect (fun r -> r.Ingredients)
        |> List.groupBy (fun i -> i.Amount.FoodstuffId)
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
    
    let inputVector = vectorize statistics [
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
    
    let topRecipesForFirstOne =
        recipes
        |> List.map (fun r -> (r, r.Ingredients |> List.map (fun i -> i.Amount) |> vectorize statistics |> cosineSimilarity inputVector))
        |> List.sortByDescending second
        |> List.take 10
        
    let best0 = List.item 0 topRecipesForFirstOne
    let best1 = List.item 1 topRecipesForFirstOne
    let best2 = List.item 2 topRecipesForFirstOne
    let best3 = List.item 3 topRecipesForFirstOne
    let best4 = List.item 4 topRecipesForFirstOne
    0 // return an integer exit code
