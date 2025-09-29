🧠 Интерпретатор Лямбда-Исчисления

Привет! Это моя реализация интерпретатора лямбда-исчисления на OCaml. Я сделал этот проект чтобы глубже разобраться в основах функционального программирования и теоритической информатике.

Что это такое?

Лямбда-исчисление — это формальная система, которая лежит в основе многих языков программирования. По сути, это минималистичный язык где есть только функции и их применение.

В моей реализации есть:

Переменные, абстракции (лямбды) и аппликации
β-редукция для вычислений
Расширяемые примитивы через систему атомов
Подстановка с правильной обработкой конфликта имён
Как это работает?

Основные типы данных

ocaml
type term = 
  | Var of string
  | Atom of string * int * (term list -> term)
  | App of term list
  | Lambda of string * term
Var "x" — обычная переменная
Lambda ("x", body) — функция λx.body
App [f; x; y] — применение f(x, y)
Atom — специальные примитивы вроде арифметики
Сердце системы — подстановка

ocaml
let rec substitute arg n body =
  match body with
  | Var m when m = n -> arg                    (* Заменяем переменную *)
  | Var m -> Var m                             (* Другая переменная — оставляем *)
  | App terms -> App (List.map (substitute arg n) terms) (* Рекурсия в аргументы *)
  | Lambda (bound_var, body_term) -> 
      if bound_var = n then Lambda (bound_var, body_term) (* Не лезем внутрь *)
      else Lambda (bound_var, substitute arg n body_term) (* Рекурсия в тело *)
  | Atom (name, arity, impl) -> body           (* Атомы не трогаем *)
Эта функция аккуратно заменяет переменные, следя чтобы не было конфликтов имён.

Вычисления через редукцию

ocaml
let rec reduce t =
  match t with
  | App ((Lambda (n, body)) :: arg :: tail) -> (* β-редукция *)
      let reduced_arg = reduce arg in
      reduce (App (substitute reduced_arg n body :: tail))
  | App ((Atom (name, arity, impl)) :: tail) -> (* Вычисление примитивов *)
      if List.length tail >= arity then
        reduce (impl tail)
      else
        App (Atom (name, arity, impl) :: List.map reduce tail)
  (* ... остальные случаи ... *)
Примеры из жизни

Базовые комбинаторы

ocaml
(* Тождественная функция: λx.x *)
let id = Lambda ("x", Var "x")

(* K-комбинатор: λx.λy.x *)
let const = Lambda ("x", Lambda ("y", Var "x"))

(* S-комбинатор: λx.λy.λz.x z (y z) *)
let s = Atom ("S", 3, function  
  | x :: y :: z :: [] -> App [x; z; App [y; z]]
  | _ -> failwith "Нужно 3 аргумента")


  Разберём сложный пример

Давайте посмотрим как вычисляется выражение:

text
(λx.λy.((λx.λy.(x+y)) (x+5) (y+11))) 5 10
В синтаксисе моего интерпретатора это выглядит так:

ocaml
App [
  Lambda ("x", Lambda ("y", 
    App [
      App [
        Lambda ("x", Lambda ("y", App [plus; Var "x"; Var "y"])),
        App [plus; Var "x"; Atom "5"]
      ],
      App [plus; Var "y"; Atom "11"]
    ]
  )),
  Atom "5",
  Atom "10"
]
Шаг за шагом:

Внешняя β-редукция — применяем функцию к 5
Подстановка — заменяем x на 5 во внутреннем выражении
Вычисление 5+5 → 10
Следующая β-редукция — применяем к 10
Подстановка — заменяем y на 10
Вычисление 10+11 → 21
Финальное вычисление — получаем 31
Весь этот процесс управляется функциями substitute и reduce.

