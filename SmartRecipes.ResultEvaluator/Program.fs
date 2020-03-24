// Learn more about F# at http://fsharp.org

open System
open System.IO
open SmartRecipes.Playground

let readLines (filePath : string) =
    seq {
        use sr = new StreamReader(filePath)
        while not sr.EndOfStream do
            let line = sr.ReadLine()
            yield if (line.Contains("]")) then line
                  else line + sr.ReadLine()
    }

[<EntryPoint>]
let main argv =

    let foodstuffByIds =
        DataStore.getFoodstuff()
        |> List.map (fun f -> (f.Id, f))
        |> Map.ofList

    let ingredients = DataStore.getIngredients()

    let recipeIdsByFoodstuffId =
        ingredients
        |> List.groupBy (fun i -> i.Amount.FoodstuffId)
        |> List.map (fun (fId, is) -> (fId, is |> List.map (fun i -> i.RecipeId)))
        |> Map.ofList

    let foodstuffIdsByRecipeId =
        ingredients
        |> List.groupBy (fun i -> i.RecipeId)
        |> List.map (fun (rId, is) ->
            (rId,
             is
             |> List.map (fun i -> i.Amount.FoodstuffId)
             |> Set.ofList))
        |> Map.ofList

    let fileLocation = argv.[0]
    let lines = readLines fileLocation
    for line in lines do
        let parts = line.Split(',')
        let foodstuff = Map.find (Guid(parts.[0])) foodstuffByIds
        let vector = parts.[1]
        let closestFoodstuff = Map.find (Guid(parts.[2])) foodstuffByIds

        let matches =
            Map.find foodstuff.Id recipeIdsByFoodstuffId
            |> List.map (fun rId -> Map.find rId foodstuffIdsByRecipeId)
            |> List.filter (fun fIds -> Set.contains closestFoodstuff.Id fIds)
            |> List.length

        printfn "%d %s -> %s" matches foodstuff.Name closestFoodstuff.Name

    0
