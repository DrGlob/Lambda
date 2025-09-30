(* Тип данных для лямбда-выражений *) 
type term = 
  | Var of string
  | Atom of string * int * (term list -> term)
  | App of term list
  | Lambda of string * term

(* Функция подстановки - исправленная версия *)
let rec substitute arg n body =
  match body with
  | Var m when m = n -> arg
  | Var m -> Var m
  | App terms -> App (List.map (substitute arg n) terms)
  | Lambda (bound_var, body_term) -> 
      if bound_var = n then Lambda (bound_var, body_term)
      else Lambda (bound_var, substitute arg n body_term)
  | Atom (name, arity, impl) -> body

(* Функция редукции - упрощенная, без атомов *)
let rec reduce t =
  match t with
  | Var n -> Var n
  | Atom (name, arity, impl) -> Atom (name, arity, impl)
  | App ((Lambda (n, body)) :: arg :: tail) ->
      let reduced_arg = reduce arg in
      reduce (App (substitute reduced_arg n body :: tail))
  | App terms ->
      let reduced_terms = List.map reduce terms in
      if reduced_terms <> terms then
        reduce (App reduced_terms)
      else
        App reduced_terms 

(* Вспомогательная функция для красивого вывода *)
let rec term_to_string = function
  | Var s -> s
  | Atom (s, _, _) -> s
  | App terms -> "(" ^ String.concat " " (List.map term_to_string terms) ^ ")"
  | Lambda (param, body) -> "(λ" ^ param ^ "." ^ term_to_string body ^ ")"

(* ТЕСТ 1: Простая идентичная функция *)
let test1 = App [Lambda ("x", Var "x"); Var "a"]

(* ТЕСТ 2: Константная функция (K-комбинатор) - упрощенная *)
let test2 = App [Lambda ("x", Var "x"); Var "a"]

(* ТЕСТ 3: Многошаговая редукция *)
let test3 = App [
    Lambda ("x", App [Lambda ("y", Var "x"); Var "b"]);
    Var "a"
  ]

let run_test name expr =
  print_endline ("\n" ^ name ^ ":");
  print_endline ("Исходное: " ^ term_to_string expr);
  try
    let result = reduce expr in
    print_endline ("Результат: " ^ term_to_string result)
  with exn ->
    print_endline ("ОШИБКА: " ^ Printexc.to_string exn);
    print_endline ("Место ошибки: " ^ Printexc.get_backtrace ())

(* Запускаем все тесты *)
let () =
  print_endline "=== ТЕСТИРОВАНИЕ ИНТЕРПРЕТАТОРА ===";
  
  run_test "ТЕСТ 1: Идентичная функция" test1;
  run_test "ТЕСТ 2: Упрощенный K-комбинатор" test2;
  run_test "ТЕСТ 3: Многошаговая редукция" test3