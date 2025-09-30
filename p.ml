(* This is an OCaml editor.
Enter your program here and send it to the toplevel using the "Eval code"
button or [Ctrl-e]. *)

(* Тип данных для лямбда-выражений *)
type term = 
  | Var of string
  | Atom of string * int * (term list -> term)
  | App of term list
  | Lambda of string * term

              
let rec substitute arg n body =
  match body with
  | Var m when m = n -> arg
  | Var m -> Var m 
  | App terms -> App (List.map (substitute arg n) terms)
  | Lambda (bound_var, body_term) -> 
      if bound_var = n then Lambda (bound_var, body_term)
      else Lambda (bound_var, substitute arg n body_term)
  | Atom (name, arity, impl) -> body 
    
    
let list_to_string = fun t -> "("^String.concat " " t ^")"
    
let rec term_to_string = fun t ->
  match t with
  | Var n -> n
  | Atom (n,arity,f) -> n
  | Lambda (bound_var, body_term) ->
      "(L"^bound_var^"."^term_to_string(body_term)^")"
      
  | App terms -> list_to_string(List.map term_to_string terms)
                   
                   


          
  
let rec reduce = fun t ->
  match t with 
  | Var n -> Var n
  | Atom (n,arity,f) -> Atom (n,arity,f)
  | App ((Atom (n,arity,f)) :: tail) ->
      if List.length tail >= arity then
        reduce (f tail)
      else
        App ((Atom (n,arity,f)) :: tail) 
  | App ((Lambda (n, body)) :: arg :: tail ) ->
      reduce (App ((substitute arg n body) :: tail))
  | App ((Lambda (n, body)) :: [] ) -> 
      Lambda (n, reduce(body))
  | Lambda (n, body) ->
      Lambda (n, reduce (body)) 
  | App(h::tail) ->
      App(reduce(h)::tail)
  |_ -> failwith (term_to_string(t))

          

let cK = Atom ("K", 2, fun args ->
    match args with
    | x :: y :: [] -> x
    | _ -> failwith "K: wrong number of arguments")
    
    
let cS = Atom ("S", 3, fun args ->
    match args with
    | x :: y :: z :: tail -> App (x :: z :: (App [y;z]) :: tail)
    | _ -> failwith "error")

               
let y = reduce(App[cS; cK; cK;  Var "x"]) ;; 
let r = term_to_string(y)


let os1 = App[Lambda("x", Lambda("y", App[Var "x"; Var "y"; Var "z"])); (App[cK; Var "z"]) ] 
    
let o1 = reduce(os1)
    
let o3 = term_to_string(o1)
    
    
    (*let o2 = reduce (o1)*)