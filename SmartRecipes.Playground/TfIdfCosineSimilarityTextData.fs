module SmartRecipes.Playground.TfIdfCosineSimilarityTextData

open System
open SmartRecipes.Playground
open Model
open Library

// This method gives almost no meaning to term frequency for given recipe, since it uses text, not structured amounts.
// On the other hand, it improves similarity in cases like "lean beef" vs "beef".
// This can be also a negative for ingredients that have similar name, but are totally different (chicken broth vs. chicken breast).
// One benefit this allows is that user would be able to give any text input and get results opposed to searching for structured data defined by the system.

type DataSetStatistics = {
    NumberOfRecipes: float
    TermFrequencies: Map<string, float>
}

let termFrequency = 1.0

let tfIdf statistics term =
    let documentFrequency = Map.find term statistics.TermFrequencies
    term, Math.Log10(statistics.NumberOfRecipes / documentFrequency) * termFrequency
    
type Vector = Map<string, float>

let vectorize statistics terms: Vector =
    List.map (tfIdf statistics) terms |> Map.ofList
    
let ingredientToWords ingredient =
    ingredient.DisplayLine.Split(' ', StringSplitOptions.None)
    |> Array.toList
    |> List.where (Seq.forall (Char.IsDigit >> not))
    |> List.where (Seq.exists (Char.IsLetter))
    
let computeStatistics (recipes: Recipe list) =
    let recipeCount = List.length recipes
    let termFrequencies =
        recipes
        |> List.collect (fun r -> r.Ingredients)
        |> List.collect ingredientToWords
        |> List.groupBy id
        |> List.map (mapSecond (List.length >> float))
        |> Map.ofList
        
    { NumberOfRecipes = float recipeCount; TermFrequencies = termFrequencies }
    
let recommend recipes inputs =
    let statistics = computeStatistics recipes
    
    let inputVector = vectorize statistics inputs 
    recipes
    |> List.map (fun r -> let vector = r.Ingredients |> List.collect ingredientToWords |> vectorize statistics in (r,  vector, cosineSimilarity inputVector vector))
    |> List.sortByDescending (fun (_, _, sim) -> sim)
    |> List.take 10