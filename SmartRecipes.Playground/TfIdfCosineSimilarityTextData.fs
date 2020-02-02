module SmartRecipes.Playground.TfIdfCosineSimilarityTextData

open System
open SmartRecipes.Playground
open Model
open Library

// This method gives almost no meaning to term frequency for given recipe, since it uses text, not structured amounts.
// On the other hand, it improves similarity in cases like "lean beef" vs "beef".
// Meaning that for example from "chicken breasts" "chicken" is not that relevant, but "breasts" is very relevant.
// This can be also a negative for ingredients that have similar name, but are totally different (chicken breasts vs. duck breasts).
// Another disadvantage is that attributes mentioned by the recipe, but not in input takes it far away, even they might be true.
// For example "skinless boneless chicken breasts" in recipe and "chicken breasts" on input.
// Another disadvantage (that can be improved) is that it does give the word a context, thus if there is a ground pepper and beef in the recipe, it will match ground beef.
// Since ground pepper is everywhere, this made ground very unimportant in the vector representation.
// Those are basically the same for the user in most cases, but the vector representation is further. The same goes opposite way.
// One benefit this allows is that user would be able to give any text input and get results opposed to searching for structured data defined by the system.
// Since this uses unstructured uncategorized data, the whole vector of recipe is pretty messy, thus requires polishing in which words to consider relevant.
// Another disadvantage is that we need to put all forms of given word (breast and breasts).

type DataSetStatistics = {
    NumberOfRecipes: float
    TermFrequencies: Map<string, float>
}

let termFrequency = 1.0

let tfIdf statistics term =
    let documentFrequency = Map.tryFind term statistics.TermFrequencies |> Option.defaultValue 1.0
    term, Math.Log10(statistics.NumberOfRecipes / documentFrequency) * termFrequency
    
type Vector = Map<string, float>

let vectorize statistics terms: Vector =
    List.map (tfIdf statistics) terms |> Map.ofList
    
let ingredientToWords ingredient =
    ingredient.DisplayLine.Split(' ', StringSplitOptions.None)
    |> Array.toList
    |> List.where (fun w -> match ingredient.Amount.Unit with Some u -> u <> w | None -> true)
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
    let recommendations = 
        recipes
        |> List.map (fun r -> let vector = r.Ingredients |> List.collect ingredientToWords |> vectorize statistics in (r,  vector, cosineSimilarity inputVector vector))
        |> List.sortByDescending (fun (_, _, sim) -> sim)
        |> List.take 10
        
    inputVector, recommendations