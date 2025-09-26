(* Тип данных для лямбда-выражений *)
type term = 
  | Var of string
  | Atom of string * int * (term list -> term)
  | App of term list
  | Lambda of string * term

let rec substitute arg n body =
  match body with
  | Var m when m = n -> arg
  | App terms -> App (List.map (substitute arg n) terms)
  | Lambda (bound_var, body_term) -> 
      if bound_var = n then body
      else Lambda (bound_var, substitute arg n body_term) 
  | Atom (name, arity, impl) -> body

let rec reduce t =
  match t with
  | Var n -> Var n
  | Atom (name, arity, impl) -> Atom (name, arity, impl)
  | App ((Atom (name, arity, impl)) :: tail) ->
      if List.length tail >= arity then
        impl tail
      else
        App ((Atom (name, arity, impl)) :: tail)
  | App ((Lambda (n, body)) :: arg :: tail) ->
      reduce (App ((substitute arg n body) :: tail))
  | App terms -> App (List.map reduce terms)

let cS = Atom ("S", 3, fun args ->  
  match args with  
  | x :: y :: z :: tail -> App (x :: z :: (App [y; z]) :: tail)  
  | _ -> failwith "error")

let getAtom = fun t ->  
  match t with  
  | Atom (n, arity, f) -> (n, arity, f)  
  | _ -> failwith "error"