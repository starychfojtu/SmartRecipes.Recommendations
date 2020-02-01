module SmartRecipes.Playground.Model
open System

type Foodstuff = {
    Id: Guid
    Name: string
}

type Ingredient = {
    Unit: string option
    Amount: float option
    Comment: string option
    DisplayLine: string
    FoodstuffId: Guid
    RecipeId: Guid
}

type Recipe = {
    Id: Guid
    Name: string
    PersonCount: int
    ImageUrl: Uri
    Url: Uri
    Description: string
    Difficulty: string option
    CookingTime: string option
    Rating: int
    Tags: string list
    Ingredients: Ingredient list
}
