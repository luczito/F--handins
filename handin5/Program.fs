//5.1
let sum (m: int) n = 
    let rec loop acc x = 
        match x with
        | 0 -> acc
        | x -> loop (m + acc + x) (x - 1)
    loop m n;;

//5.2
let length lst = 
    let rec loop lst acc = 
        match lst with
        | [] -> acc
        | _::xs -> loop xs (acc + 1)
    loop lst 0

//5.3
let foldBack folder lst acc =
  let rec Loop lst cont =
    match lst with
    | h :: t -> Loop t (fun racc -> cont (folder h racc))
    | [] -> cont acc
  Loop lst id;; 

//5.4
let factA x =
    let rec aux acc =
        function
        | 0 -> acc
        | x -> aux (x * acc) (x - 1)
    aux 1 x;;

//Real: 00:00:00.003, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0 -     input = 0
//Real: 00:00:00.003, CPU: 00:00:00.015, GC gen0: 0, gen1: 0, gen2: 0 -     input = 10
//Real: 00:00:00.003, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0 -     input = 1000
//Real: 00:00:00.004, CPU: 00:00:00.031, GC gen0: 0, gen1: 0, gen2: 0 -     input = 1000000
//Real: 00:00:00.015, CPU: 00:00:00.015, GC gen0: 0, gen1: 0, gen2: 0 -     input = 100000000

let rec factC x =
    let rec loop acc cont =
        match acc with
        | 0 -> cont 1
        | _ -> loop (acc-1) (fun racc -> cont (acc * racc))
    in loop x (fun racc -> racc);;

//Real: 00:00:00.004, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0 -     input = 0
//Real: 00:00:00.003, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0 -     input = 10
//Real: 00:00:00.003, CPU: 00:00:00.000, GC gen0: 0, gen1: 0, gen2: 0 -     input = 1000
//Real: 00:00:00.055, CPU: 00:00:00.046, GC gen0: 4, gen1: 3, gen2: 0 -     input = 1000000
//Real: 00:00:10.262, CPU: 00:00:10.312, GC gen0: 388, gen1: 387, gen2: 5 - input = 100000000
