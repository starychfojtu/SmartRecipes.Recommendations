module SmartRecipes.Playground.TfIdfCosineSimilarityStructuredDataWithDiversity

open SmartRecipes.Playground
open SmartRecipes.Playground.Model

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


let maximalMarginalRelevance = ()

let recommend recipes foodstuffAmounts =
    TfIdfCosineSimilarityStructuredData.recommend recipes foodstuffAmounts
            