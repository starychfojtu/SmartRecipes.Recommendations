module SmartRecipes.Playground.JsonExport

type Ingredient = {
    FoodstuffId: string
    DisplayLine: string
    IsInputMatch: bool
}

type RecipeId = string

type Recipe = {
    Id: RecipeId
    Uri: string
    ImageUri: string
    Name: string
    Ingredients: Ingredient list
}

type RecommendationMethodId = string

type RecommendationMethod = {
    Id: RecommendationMethodId
    Name: string
    Recommendations: Recipe list
}

type Input = string list

type RecommendationScenario = {
    Description: string
    Input: Input
    Recommendations: RecommendationMethod list
}