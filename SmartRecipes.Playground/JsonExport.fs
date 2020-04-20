module SmartRecipes.Playground.JsonExport

type Ingredient = {
    DisplayLine: string
    IsInputMatch: bool
}

type RecipeId = string

type Recipe = {
    Id: RecipeId
    Uri: string
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