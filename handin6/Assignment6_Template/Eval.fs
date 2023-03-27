module Eval

    open StateMonad
    open Types

    (* Code for testing *)

    let hello = [ ('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1)] 
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    let add (a: SM<'a>) (b: SM<'a>) = 
        a >>= fun x -> 
            b >>= fun y ->
                ret (x + y)

    let div a b = 
        a >>= fun x -> 
            b >>= fun y ->
                if y = 0 then fail DivisionByZero
                else ret (x / y)   

    let sub (a: SM<'a>) (b: SM<'a>) = 
        a >>= fun x -> 
            b >>= fun y ->
                ret (x - y)

    let mul (a: SM<'a>) (b: SM<'a>) = 
        a >>= fun x -> 
            b >>= fun y ->
                ret (x * y)

    let modu a b = 
        a >>= fun x -> 
            b >>= fun y ->
                if y = 0 then fail DivisionByZero
                else ret (x % y)

    let IsVowelOwn x =
        ['a';'e';'i';'o';'u'] |> List.contains x

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval a =
       match a with
       | N n -> ret n
       | V x -> lookup x
       | WL -> wordLength
       | PV a -> arithEval a >>= (fun (a: int) -> pointValue a)
       | Add (a1, a2) -> add (arithEval a1) (arithEval a2)
       | Sub (a1, a2) -> sub (arithEval a1) (arithEval a2)
       | Mul (a1, a2) -> mul (arithEval a1) (arithEval a2)
       | Div (a1, a2) -> div (arithEval a1) (arithEval a2)
       | Mod (a1, a2) -> modu (arithEval a1) (arithEval a2)
       | CharToInt a -> charEval a >>= (fun c -> ret(System.Convert.ToInt32 c))

    and charEval c : SM<char> = 
        match c with
        | C c -> ret c
        | ToUpper c -> charEval c >>= (fun c -> ret (System.Char.ToUpper c))
        | ToLower c -> charEval c >>= (fun c -> ret (System.Char.ToLower c))
        | CV c -> arithEval c >>= (fun c -> characterValue c)
        | IntToChar c -> arithEval c >>= (fun c -> ret(System.Convert.ToChar c))    

    and boolEval b : SM<bool> =
        match b with
        | TT -> ret(true)
        | FF -> ret(false)
        | AEq (c1, c2) -> arithEval c1 >>= (fun c1 -> arithEval c2 >>= (fun c2 -> ret (c1 = c2)))
        | ALt(b1, b2) -> arithEval b1 >>= (fun b1 -> arithEval b2 >>= (fun b2 -> ret (b1 < b2)))
        | Not b -> boolEval b >>= (fun b -> ret(not b))
        | Conj(b1, b2) -> boolEval b1 >>= (fun b1 -> boolEval b2 >>= (fun b2 -> ret (b1 && b2)))
        | IsVowel c -> charEval c >>= (fun c -> c |> System.Char.ToLower |> IsVowelOwn |> ret)
        | IsLetter c -> charEval c >>= (fun c -> c |> System.Char.IsLetter |> ret)
        | IsDigit c -> charEval c >>= (fun c -> c |> System.Char.IsDigit |> ret)


    type stmnt =                  (* statements *)
    | Declare of string           (* variable declaration *)
    | Ass of string * aExp        (* variable assignment *)
    | Skip                        (* nop *)
    | Seq of stmnt * stmnt        (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt       (* while statement *)

    let rec stmntEval stmnt : SM<unit> = failwith "Not implemented"

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *) 

    let stmntToSquareFun stm = failwith "Not implemented"

    let stmntToBoardFun stm m = failwith "Not implemented"

    type squareStmnt = Map<int, stmnt>
    let stmntsToSquare stms = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
    