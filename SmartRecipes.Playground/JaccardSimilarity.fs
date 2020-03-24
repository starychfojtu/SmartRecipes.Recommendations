module SmartRecipes.Playground.JaccardSimilarity

open SmartRecipes.Playground
open Model
open Library

// This method is just a reference point for the others.
// Has no way of distinguishing importance of foodstuff (no amount factor, no relevance in the whole dataset factor).
// Moreover it prefers small size recipes.

let private jaccardDistance a b =
    let intersectionCount = Set.intersect a b |> Set.count |> float
    let unionCount = Set.union a b |> Set.count |> float
    intersectionCount / unionCount
    
let recommend recipes inputs n =
    let inputSet = Set.ofList inputs
    let recommendations = 
        recipes
        |> List.map (fun r -> (r, r.Ingredients |> List.map (fun i -> i.Amount.FoodstuffId) |> Set.ofList |> jaccardDistance inputSet))
        |> List.sortByDescending second
        |> List.take n
        |> List.map first
        
    recommendations