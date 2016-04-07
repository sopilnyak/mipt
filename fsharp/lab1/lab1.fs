type Result = float * int
let delta = 1e-10

// First part

let ftaylor x : float = (1. + 2. * (x ** 2.)) * exp(x ** 2.) // function we need to expand
let a, b = 0., 1. // interval
// let n = int ((b - a) / delta)
let n = 30

let rec factorial n =
    match n with
    | 0 | 1 -> 1
    | _ -> n * factorial(n - 1)

// saving the previous coefficient
let taylor x : Result = 
    let rec taylor next prev_coeff (i : int) sum = 
        if i = n then (sum, i) else
            taylor ((float (2 * i + 1) / float (factorial i)) * prev_coeff * x * x) (prev_coeff * x * x) (i + 1) (sum + next)
    taylor 0. 1. 1 1.

// bruteforce coefficient count
let taylorA x : Result = 
    let rec taylorA' next (i : int) sum  =
        if i = n then (sum, i) else
            taylorA' ((float (2 * i + 1) / float (factorial i)) * (x ** float (2 * i))) (i + 1) (sum + next)
    taylorA' 0. 0 0.

let printtaylor () = 
    [a .. (b - a) / (float n) .. b] 
    |> List.map (fun x -> let (firstRes, firstCou), (secondRes, secondCou) = taylor x, taylorA x in (x, firstRes, firstCou, secondRes, secondCou, ftaylor x))
    |> List.iter (fun (a, b, c, d, e, f) -> printf "%f\t%f\t%d\t%f\t%d\t%f\n" a b c d e f )

printtaylor()
