type Result = float * int
let delta = 1e-8

// Part one

let factorial n =
    let rec factorial' i acc =
        match i with
        | 0 | 1 -> acc
        | _ -> factorial' (i - 1) (acc * i)
    factorial' n 1

let fTaylor x : float = (1. + 2. * (x ** 2.)) * exp(x ** 2.) // function we need to expand
let taylorCoeff i : float = (float (2 * i + 1) / float (factorial i))
let a, b, n = 0., 1., 20 // interval

// saving the previous coefficient
let taylor x : Result = 
    let rec taylor prevCoeff (i : int) sum = 
        if (abs(sum - fTaylor x) < delta) then (sum, i) else
            taylor (prevCoeff * x * x) (i + 1) (sum + (taylorCoeff i) * prevCoeff * x * x)
    taylor 1. 1 1.

// bruteforce coefficient count
let taylorA x : Result = 
    let rec taylorA' (i : int) sum =
        if (abs(sum - fTaylor x) < delta) then (sum, i) else
            taylorA' (i + 1) (sum + (taylorCoeff i) * (x ** float (2 * i)))
    taylorA' 0 0.

let printTaylor () = 
    [a .. (b - a) / (float n) .. b] 
    |> List.map (fun x -> let (firstRes, firstCou), (secondRes, secondCou) = taylor x, taylorA x in (x, firstRes, firstCou, secondRes, secondCou, fTaylor x))
    |> List.iter (fun (a, b, c, d, e, f) -> printf "%f\t%f\t%d\t%f\t%d\t%f\n" a b c d e f )

// Part two

let fSolve1 = fun (x : float) -> 3. * sin(sqrt(x)) + 0.35 * x - 3.8
let fSolvex1 = fun (x : float) -> (3.8 - 3. * (sin(sqrt(x)))) / 0.35
let dfSolve1 = fun (x : float) -> (3. * cos(sqrt(x))) / (2. * sqrt(x)) + 0.35
let df2Solve1 = fun (x : float) -> -(3. * (sqrt(x) * sin(sqrt(x)) + cos(sqrt(x)))) / (4. * (sqrt(x ** 3.)))
let left1, right1 = 2., 3.

let fSolve2 = fun (x : float) -> 0.25 * (x ** 3.) + x - 1.2502
let fSolvex2 = fun (x : float) -> -0.25 * (x ** 3.) + 1.2502
let dfSolve2 = fun (x : float) -> 0.75 * x * x + 1.
let df2Solve2 = fun (x : float) -> 1.5 * x
let left2, right2 = 0., 2.

let fSolve3 = fun (x : float) -> x + sqrt(x) + (x ** (1. / 3.)) - 2.5
let fSolvex3 = fun (x : float) -> -sqrt(x) - (x ** (1. / 3.)) + 2.5
let dfSolve3 = fun (x : float) -> 1. / (3. * (x ** (2. / 3.))) + 1. / (2. * sqrt(x)) + 1.
let df2Solve3 = fun (x : float) -> -1. / (4. * (x ** (3. / 2.))) - 2. / (9. * (x ** (5. / 3.)))
let left3, right3 = 0.01, 1.

let iter f fx df df2 left right : Result = 
    let rec iter' i current previous =
        if abs(current - previous) < delta then (current, i) else
            iter' (i + 1) (fx current) current
    iter' 1 (fx ((right - left) / 2.)) ((right - left) / 2.)

let newton f fx df df2 left right : Result = 
    let rec newton' i c = 
        if abs(c - (f c) / (df c) - c) < delta then (c, i) else
            newton' (i + 1) (c - (f c) / (df c))
    if ((f left) * (df2 left)) > 0. then newton' 0 left else newton' 0 right

let dichotomy f fx df df2 left right : Result =
    let rec dichotomy' i (f : float->float) (a : float) (b : float) : Result = 
        if abs(b - a) < delta then ((a + b) / 2., i) else
            if ((f a) * (f ((a + b) / 2.))) < 0. then dichotomy' (i + 1) f a ((a + b) / 2.) else
                dichotomy' (i + 1) f ((a + b) / 2.) b
    dichotomy' 0 f left right

let printSolve fSolve fSolvex dfSolve df2Solve left right =
    [iter; newton; dichotomy] 
    |> List.map (fun f -> f fSolve fSolvex dfSolve df2Solve left right) 
    |> List.iter (fun (res, cou) -> printf "%f\t%d\n" res cou)
   
printTaylor()
printSolve fSolve1 fSolvex1 dfSolve1 df2Solve1 left1 right1
printSolve fSolve2 fSolvex2 dfSolve2 df2Solve2 left2 right2
printSolve fSolve3 fSolvex3 dfSolve3 df2Solve3 left3 right3
