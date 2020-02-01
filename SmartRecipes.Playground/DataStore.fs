module SmartRecipes.Playground.DataStore

open System
open Model
open Npgsql.FSharp
open Npgsql.FSharp.OptionWorkflow

let connection = Sql.fromUri <| Uri "postgresql://localhost:5432/smartrecipes"

let getFoodstuff () =
    connection
    |> Sql.connect
    |> Sql.query "SELECT * From dbo.foodstuff"
    |> Sql.executeReader (fun reader ->
        let row = Sql.readRow reader
        option {
            let! id = Sql.readUuid "id" row
            let! name = Sql.readString "name" row
            return { Id = id; Name = name; }
        })
    
let getIngredients () =
    connection
    |> Sql.connect
    |> Sql.query "SELECT * From dbo.ingredient "
    |> Sql.executeReader (fun reader ->
        let row = Sql.readRow reader
        option {
            let unit = Sql.readString "unit" row
            let comment = Sql.readString "comment" row
            let! displayLine = Sql.readString "displayline" row
            let amount = Sql.readDecimal "amount" row |> Option.map float
            let! foodstuffId = Sql.readUuid "foodstuffid" row
            let! recipeId = Sql.readUuid "recipeid" row
            return { Unit = unit; Amount = amount; Comment = comment; DisplayLine = displayLine; FoodstuffId = foodstuffId; RecipeId = recipeId }
        })
    
let getRecipes () =
    let ingredients =
        getIngredients ()
        |> List.groupBy (fun i -> i.RecipeId)
        |> Map.ofList
            
    connection
    |> Sql.connect
    |> Sql.query "SELECT * From dbo.recipe"
    |> Sql.executeReader (fun reader ->
        let row = Sql.readRow reader
        option {
            let! id = Sql.readUuid "id" row
            let! name = Sql.readString "name" row
            let! personCount = Sql.readInt "personcount" row
            let! imageUrl = Sql.readString "imageurl" row
            let! url = Sql.readString "url" row
            let! description = Sql.readString "description" row
            let cookingTime = Sql.readString "cookingtime" row
            let difficulty = Sql.readString "difficulty" row
            let! rating = Sql.readInt "rating" row
            return {
                Id = id;
                Name = name;
                PersonCount = personCount;
                ImageUrl = Uri imageUrl;
                Url = Uri url;
                Description = description;
                Difficulty = difficulty;
                CookingTime = cookingTime;
                Rating = rating;
                Tags = [];
                Ingredients = Map.find id ingredients;
            }
        })