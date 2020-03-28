module SmartRecipes.Playground.TfIdfCosineSimilarityStructuredDataWithDynamicAmountAltering

open SmartRecipes.Playground
open SmartRecipes.Playground.Model

// The problem this method tries to solve is that when we have 2 pairs of ingredients that go together,
// what we would like to get is half a meals recommended for one pair and then the other one to the other pair.
// However, TF-IDF evaluates that one pair is more significant than the other and we end up with recommendations only for one pair.
// That goes strongly against our shopping list use-case. Thus this methods is iterative and over time, it gives more importance
// to ingredients that haven't been utilized yet.
    
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
    
let recommend recipes foodstuffAmounts step n =
    let rec recommendRecursive allRecommendations foodstuffAmounts k =
        let recommendations =
            TfIdfCosineSimilarityStructuredData.recommend recipes foodstuffAmounts
            |> Seq.where (fun r -> not <| List.contains r allRecommendations)
            |> Seq.take k
            |> Seq.append allRecommendations
            |> Seq.toList
            
        let enoughRecipesRecommended = List.length recommendations >= n
        if enoughRecipesRecommended
            then recommendations
            else recommendRecursive recommendations (transformInput recommendations foodstuffAmounts) (n - k)
            
    recommendRecursive [] foodstuffAmounts step