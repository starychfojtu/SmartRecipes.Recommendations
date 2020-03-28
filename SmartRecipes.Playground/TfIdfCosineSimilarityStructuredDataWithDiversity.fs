module SmartRecipes.Playground.TfIdfCosineSimilarityStructuredDataWithDiversity

open Library
open SmartRecipes.Playground.Model
open SmartRecipes.Playground.TfIdfCosineSimilarityStructuredData

/// Problem this method is trying to solve that for first case (beef and peppers) we only get stuffed pepper recipes.
/// There is no diversity given to the user and he only receives single option with some variations.
/// What we want is to actually get different recipes, to see what is available and thus increase the density of the information given to the user.
/// On option would be to say, that the cosine similarity among recommendation must be greater than some constant.
/// Looking at data, 0.5 could be reasonable.
/// That is very hard to tune tho, especially with our dataset.
/// Printing diversity:
//    let recipesAndVectors = List.toArray recipesToRecommend
//    let sim a b = cosineSimilarity (vectorizeRecipe statistics a) (vectorizeRecipe statistics b)
//    for i in 0..9 do
//        for j in (i + 1)..9 do
//            let iRecipe = recipesAndVectors.[i]
//            let jRecipe = recipesAndVectors.[j]
//            printfn "%d (%s) <-> %d (%s): %f" i iRecipe.Name j jRecipe.Name (sim iRecipe jRecipe)
/// Second more dynamic method to use is MRR (maximalMarginalRelevance), where we also need to tune a constant, although a very different one.
/// There we only tune how much diversity we want, which in the end, can be tuned by the user himself.

type RecipeInfo = {
    Recipe: Recipe
    Vector: Vector
    InputSimilarity: float
}

let toRecipeInfo statistics inputVector recipe =
    let vector = vectorizeRecipe statistics recipe
    {
        Recipe = recipe
        Vector = vector
        InputSimilarity = cosineSimilarity inputVector vector
    }
    
let tryMax list =
    try
        Some <| List.max list
    with _ ->
        None

let maximalMarginalRelevance lambda results recipeInfo =
    let maxSimilarityToRankedRecipes =
        results
        |> List.map (fun r -> cosineSimilarity recipeInfo.Vector r.Vector)
        |> tryMax
        |> Option.defaultValue 0.0
        
    lambda * recipeInfo.InputSimilarity - (1.0 - lambda) * maxSimilarityToRankedRecipes
    
let pickNextBestRecipe lambda results candidates =
    candidates
    |> List.map (fun r -> (r, maximalMarginalRelevance lambda results r))
    |> List.maxBy second
    |> first
    
let rec getRecommendations lambda results candidates n =
    if List.isEmpty candidates
        then results
        else 
            let nextBest = pickNextBestRecipe lambda results candidates
            let newResults = nextBest::results
            if List.length newResults >= n
                then List.rev newResults
                else getRecommendations lambda newResults (List.except [nextBest] candidates) n

let recommend recipes foodstuffAmounts n =
    let lambda = 0.5 // accuracy vs diversity ratio (the higher the better for accuracy)
    let statistics = computeStatistics recipes
    let inputVector = vectorize statistics foodstuffAmounts
    
    let recipeInfoCandidates =
        recipes
        |> List.map (toRecipeInfo statistics inputVector)
        |> List.where (fun r -> r.InputSimilarity > 0.1) // Filter only at least somehow relevant candidates.
    
    getRecommendations lambda List.empty recipeInfoCandidates n
    |> List.map (fun r -> r.Recipe)
            