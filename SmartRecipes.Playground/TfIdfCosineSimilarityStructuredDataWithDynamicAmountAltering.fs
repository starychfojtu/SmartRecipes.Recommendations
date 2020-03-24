module SmartRecipes.Playground.TfIdfCosineSimilarityStructuredDataWithDynamicAmountAltering

open SmartRecipes.Playground
open SmartRecipes.Playground.Model
    
let transformInput (recipes: Recipe list) (foodstuffAmounts: FoodstuffAmount list)=
    let foodstuffsIncludedInRecommendations =
        recipes
        |> List.collect (fun r -> r.Ingredients)
        |> List.map (fun i -> i.Amount.FoodstuffId)
        |> Set.ofList
        
    foodstuffAmounts
    |> List.map (fun a ->
        if Set.contains a.FoodstuffId foodstuffsIncludedInRecommendations 
            then a
            else { a with Value = a.Value |> Option.map ((*) 2.0) })
    
let recommend recipes foodstuffAmounts k n =
    let rec recommendRecursive allRecommendations foodstuffAmounts k =
        let recommendations =
            TfIdfCosineSimilarityStructuredData.recommend recipes foodstuffAmounts (10 * n)
            |> List.where (fun r -> not <| List.contains r allRecommendations)
            |> List.take k
            |> List.append allRecommendations
            
        let enoughRecipesRecommended = List.length recommendations >= n
        if enoughRecipesRecommended
            then recommendations
            else recommendRecursive recommendations (transformInput recommendations foodstuffAmounts) (n - k)
            
    recommendRecursive [] foodstuffAmounts k