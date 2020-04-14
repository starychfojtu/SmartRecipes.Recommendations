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
    VoteNumber: float
    VoteNumberModifier: float
}

let relevance recipeInfo foodstuffAmount =
    let ingredient = Map.tryFind foodstuffAmount.FoodstuffId recipeInfo.Recipe.IngredientByFoodstuffId
    let foodstuffRelevance =
        ingredient
        |> Option.map (fun i -> TfIdf.termFrequency i.Amount)
        |> Option.defaultValue 0.0
        
    foodstuffRelevance * recipeInfo.InputSimilarity

let relevanceSum foodstuffInfos recipeInfo  =
    foodstuffInfos
    |> List.map (fun i -> i.VoteNumber * (relevance recipeInfo i.Amount))
    |> List.sum

let pickBest recipeInfos foodstuffAmounts =
    recipeInfos
    |> List.maxBy (relevanceSum foodstuffAmounts)
    
let adjustInfo bestRecipe info =
    let newVoteNumberModifier = info.VoteNumberModifier + (relevance bestRecipe info.Amount)
    {
        Amount = info.Amount
        VoteNumber = info.VoteNumber / newVoteNumberModifier
        VoteNumberModifier = newVoteNumberModifier
    }
    
let calibrateOne recipeInfos foodstuffInfos =
    let bestRecipeInfo = pickBest recipeInfos foodstuffInfos
    let newFoodstuffInfos = foodstuffInfos |> List.map (adjustInfo bestRecipeInfo)
    (bestRecipeInfo, newFoodstuffInfos)


let calibrateStep (recipeInfos, foodstuffInfos, recommendations: RecipeInfo list) =
    let (bestRecipeInfo, newFoodstuffInfos) = calibrateOne recipeInfos foodstuffInfos
    let newRecipeInfos = List.except [bestRecipeInfo] recipeInfos
    (newRecipeInfos, newFoodstuffInfos, bestRecipeInfo::recommendations)
    
let toInitialInfo amount =
    {
        Amount = amount
        VoteNumber = TfIdf.termFrequency amount
        VoteNumberModifier = 1.0
    }
    
let calibrate recipeInfos foodstuffAmounts n =
    let initialFoodstuffInfos = foodstuffAmounts |> List.map toInitialInfo
    let initialState = (recipeInfos, initialFoodstuffInfos, [])
    let (_, _, recommendations) = 
        Seq.replicate n ()
        |> Seq.fold (fun state _ -> calibrateStep state) initialState
        
    recommendations
    |> List.rev