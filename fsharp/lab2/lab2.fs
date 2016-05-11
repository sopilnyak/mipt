open System
open System.IO
open System.Net
open System.Text
open System.Collections.Specialized

// JSON parser
let explode (s:string) =
  [for c in s -> c]

type Token =
  | OpenBrace | CloseBrace
  | OpenBracket | CloseBracket
  | Colon | Comma
  | String of string
  | Number of int
  | Boolean of bool
  | Null

let tokenize source =
  let rec parseString acc = function
    | '\\' :: '"' :: t -> parseString (acc + "\"") t
    | '\\' :: 'n' :: t -> parseString (acc + "\n") t
    | '"' :: t -> acc, t
    | c :: t -> parseString (acc + c.ToString()) t
    | _ -> failwith "Malformed string."
 
  let rec token acc = function
    | (x :: _) as t when List.exists ((=)x) [')'; ':'; ','; ']'] -> acc, t
    | w :: t when Char.IsWhiteSpace(w) -> acc, t
    | [] -> acc, [] // end of list terminates
    | c :: t -> token (acc + (c.ToString())) t

  let rec tokenize' acc = function
    | w :: t when Char.IsWhiteSpace(w) -> tokenize' acc t
    | '{' :: t -> tokenize' (OpenBrace :: acc) t
    | '}' :: t -> tokenize' (CloseBrace :: acc) t
    | '[' :: t -> tokenize' (OpenBracket :: acc) t
    | ']' :: t -> tokenize' (CloseBracket :: acc) t
    | ':' :: t -> tokenize' (Colon :: acc) t
    | ',' :: t -> tokenize' (Comma :: acc) t
    | '"' :: t -> // start of string
      let s, t' = parseString "" t
      tokenize' (String s :: acc) t'    
    | 'n' :: 'u' :: 'l' :: 'l' :: t -> tokenize' (Null :: acc) t
    | 't' :: 'r' :: 'u' :: 'e' :: t -> tokenize' (Boolean true :: acc) t
    | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: t -> tokenize' (Boolean false :: acc) t
    | d :: t -> // остались числа
      let n, t' = token (d.ToString()) t
      tokenize' (Number (try Convert.ToInt32 n with e -> 0)  :: acc) t'
    | [] -> List.rev acc
    | _ -> failwith "Tokinzation error"
  tokenize' [] source

type JSON =
  | Object of (string * JSON) list
  | Array of JSON list
  | Number of int
  | String of string
  | Boolean of bool
  | Null

let rec parse json =
  let rec parse' json =
    let rec parseObject list = function
      | CloseBrace :: t -> (Object (List.rev list)), t
      | Comma :: t -> parseObject list t
      | Token.String s :: Colon :: t ->
        let a, t = parse' t
        parseObject ((s, a) :: list) t
      | _ -> failwith "Incorrect object"
    let rec parseArray list = function
      | CloseBracket :: t -> (Array (List.rev list)), t
      | Comma :: t -> parseArray list t
      | ob -> 
        let a, t = parse' ob
        parseArray (a :: list) t  
    match json with
      | OpenBrace :: t -> parseObject [] t
      | OpenBracket :: t -> parseArray [] t
      | Token.Null :: t -> JSON.Null, t
      | Token.String s :: t -> JSON.String s, t
      | Token.Number s :: t -> JSON.Number s, t
      | Token.Boolean s :: t -> JSON.Boolean s, t
      | _ -> failwith "Incorrect identification"
  match parse' json with
    | res, [] -> res
    | _ -> failwith "Wrong JSON structure"

// 16: sum of integers in the tree
let rec sum = function 
  | Object list -> List.fold (fun acc x -> acc + (sum (snd x))) 0 list
  | Array list -> List.fold (fun acc x -> acc + (sum  x)) 0 list
  | Number int -> int
  | _ -> 0


// JSON object to string
let rec stringify = function
  | Object list -> "{" + (String.concat ", " (List.map (fun (a, b) -> "\"" + a + "\": " + (stringify b)) list)) + "}"
  | Array list -> "[" + (String.concat ", " (List.map stringify list)) + "]"
  | Number int -> string int
  | String string -> "\"" + string + "\""
  | Boolean bool -> if bool then "true" else "false"
  | Null -> "null"

// generate random JSON object
let generate () = 
  let rnd = new Random()

  let randomString length = 
    let chars = Array.append [|'A'..'Z'|] [|'0'..'9'|]
    let randomChars = [|for i in 1..rnd.Next(length) -> chars.[rnd.Next(chars.Length)]|]
    System.String(randomChars)

  let rec generate' n =
    match rnd.Next(n) with
      | 0 -> Array[for i in 1..rnd.Next(n) -> generate' (i - 1)]
      | 1 -> Number(rnd.Next(n))
      | 2 -> String(randomString (rnd.Next(n)))
      | 3 -> Boolean(false)
      | 4 -> Boolean(true)
      | 5 -> Null
      | _ -> Object[for i in 1..rnd.Next(n) -> (rnd.Next(n).ToString(), generate' (i - 1))]

  generate' 42

// Tests

let tree0 = generate()
sum tree0
stringify tree0

let string1 = """
{
  "a": 1
  "b": {
    "c": [1,2,3]
  }
}
"""

let tree1 = string1 |> explode |> tokenize |> parse
let sum1 = sum tree1
stringify tree1

let string2 = """
{
  "firstName": "John",
  "lastName": "Smith",
  "isAlive": true,
  "age": 25,
  "address": {
    "streetAddress": "21 2nd Street",
    "city": "New York",
    "state": "NY",
    "postalCode": "10021-3100"
  },
  "phoneNumbers": [
    {
      "type": "home",
      "number": "212 555-1234"
    },
    {
      "type": "office",
      "number": "646 555-4567"
    },
    {
      "type": "mobile",
      "number": "123 456-7890"
    }
  ],
  "children": [],
  "spouse": null
}
"""

let tree2 = string2 |> explode |> tokenize |> parse
sum tree2
stringify tree2
