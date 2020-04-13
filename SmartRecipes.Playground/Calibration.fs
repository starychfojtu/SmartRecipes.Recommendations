module SmartRecipes.Playground.Calibration

open SmartRecipes.Playground.Model

let transformInput (recipes: Recipe list) (foodstuffAmounts: FoodstuffAmount list)=
    let foodstuffsIncludedInRecommendations =
        recipes
        |> List.collect (fun r -> r.Ingredients)
        |> List.map (fun i -> i.Amount.FoodstuffId)
        |> Set.ofList
        
    foodstuffAmounts
    |> List.map (fun a ->
        let isAlreadyInRecommended = Set.contains a.FoodstuffId foodstuffsIncludedInRecommendations 
        let changeAmountFunction = if isAlreadyInRecommended then ((/) 2.0) else ((*) 2.0)
        { a with Value = a.Value |> Option.map changeAmountFunction })
       
let postProcess recommend recipes foodstuffAmounts step n =
    let rec recommendRecursive allRecommendations foodstuffAmounts k =
        let recommendableRecipes = List.except allRecommendations recipes
        let recommendations =
            recommend recommendableRecipes foodstuffAmounts
            |> Seq.take k
            |> Seq.append allRecommendations
            |> Seq.toList
            
        let enoughRecipesRecommended = List.length recommendations >= n
        if enoughRecipesRecommended
            then recommendations
            else recommendRecursive recommendations (transformInput recommendations foodstuffAmounts) (n - k)
            
    recommendRecursive [] foodstuffAmounts step