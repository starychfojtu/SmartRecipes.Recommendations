module SmartRecipes.Playground.TfIdfCosineSimilarityStructuredDataWithDiversity

open SmartRecipes.Playground
open SmartRecipes.Playground.Model

let recommend recipes foodstuffAmounts n =
    TfIdfCosineSimilarityStructuredData.recommend recipes foodstuffAmounts n
            