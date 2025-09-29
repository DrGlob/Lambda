(* Тип данных для лямбда-выражений *)
(*
App [t1; t2; t3; t4] представляет последовательную аппликацию (((t1 t2) t3) t4) - посути удобная апликация
Atom - расширяемый примитив
*)
(*<> - неравно*)
type term = 
  | Var of string
  | Atom of string * int * (term list -> term)
  | App of term list
  | Lambda of string * term

(* (λx.λy.((λx.λy.(x+y)) (x+5) (y+11))) 5 10 *)
let rec substitute arg n body =
  match body with
  | Var m when m = n -> arg        1
  | Var m -> Var m (*оставляем как есть*).   2
  | App terms -> App (List.map (substitute arg n) terms).  3
  | Lambda (bound_var, body_term) -> 
      if bound_var = n then Lambda (bound_var, body_term). 4
      else Lambda (bound_var, substitute arg n body_term)   5
  | Atom (name, arity, impl) -> body. 6

let rec reduce t =
  match t with
  | Var n -> Var n. 1
  | Atom (name, arity, impl) -> Atom (name, arity, impl). 2
  | App ((Atom (name, arity, impl)) :: tail) ->
      if List.length tail >= arity then. 3
        reduce (impl tail)
      else. 4
        App (Atom (name, arity, impl) :: List.map reduce tail)
  | App ((Lambda (n, body)) :: arg :: tail) ->. 5
      reduce (App (reduce arg :: substitute (reduce arg) n body :: tail))
  | App terms ->.
      let reduced_terms = List.map reduce terms in
      if reduced_terms <> terms then 6
        reduce (App reduced_terms)
      else. 7
        App reduced_terms

let cS = Atom ("S", 3, fun args ->  
  match args with  
  | x :: y :: z :: tail -> App (x :: z :: (App [y; z]) :: tail)  
  | _ -> failwith "error")

let getAtom = fun t ->  
  match t with  
  | Atom (n, arity, f) -> (n, arity, f)  
  | _ -> failwith "error"