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
        let changeAmountFunction = if isAlreadyInRecommended then id else ((*) 2.0)
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
    
// Election algorithm.

type FoodstuffAmountInfo = {
    Amount: FoodstuffAmount
    InitialWeight: float
    WeightModifier: float
} with
    member this.Weight = this.InitialWeight / this.WeightModifier

let relevance recipe foodstuffAmount =
    let ingredient = Map.tryFind foodstuffAmount.FoodstuffId recipe.IngredientByFoodstuffId
    ingredient
    |> Option.map (fun i -> TfIdf.termFrequency i.Amount)
    |> Option.defaultValue 0.0
    
let adjustInfo bestRecipe info =
    {
        Amount = info.Amount
        InitialWeight = info.InitialWeight
        WeightModifier = info.WeightModifier + (relevance bestRecipe info.Amount)
    }

let calibrateStep similarity (recipes, foodstuffInfos, recommendations: Recipe list) =
    let bestRecipe = recipes |> List.maxBy (similarity foodstuffInfos) 
    let newFoodstuffInfos = foodstuffInfos |> List.map (adjustInfo bestRecipe)
    let newRecipeInfos = List.except [bestRecipe] recipes
    (newRecipeInfos, newFoodstuffInfos, bestRecipe::recommendations)
    
let toInitialInfo weight amount =
    {
        Amount = amount
        InitialWeight = weight amount
        WeightModifier = 1.0
    }
    
let calibrate similarity weight recipes foodstuffAmounts n =
    let initialFoodstuffInfos = foodstuffAmounts |> List.map (toInitialInfo weight)
    let initialState = (recipes, initialFoodstuffInfos, [])
    let (_, _, recommendations) = 
        Seq.replicate n ()
        |> Seq.fold (fun state _ -> calibrateStep similarity state) initialState
        
    recommendations
    |> List.rev
    