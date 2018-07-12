let f n : (int -> int)= printfn "%s" n; id

let fs = List.replicate 3 f
let ts = [1..3] |> List.map string

fs 
|> List.zip ts
|> List.map (fun (t, f) -> f t)
|> List.reduce (>>)