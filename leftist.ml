(*Autor: Bartosz Ruszewski*)
(*Code review: Mateusz Nowakowski*)

(*--------DEKLARACJA KOLEJKI---------*)

(*Kolejka priorytetowa w formie drzewa lewicowego, kolejne elementy oznaczaja 
  odpowiednio: element o najwyższym priorytecie, prawa wysokosc drzewa, 
  lewe poddrzewo, prawe poddrzewo*)
type 'a queue = 
  | Node of 'a * int * 'a queue * 'a queue
  | Leaf
  
(*--------FUNKCJE POMOCNICZE---------*)
  
(*Dla danych dwóch kolejek, zwraca parę której pierwszym elementem
  jest mniejsza prawa wysokosc zwiekszona o 1, a drugim para kolejek 
  ustawionych w kolejności malejącej względem prawej wysokosci*)
let ustaw que1 que2 =
  match (que1, que2) with
    | Node (_, h1, _, _), Node (_, h2, _, _)->
      if h1 > h2 then (h2 + 1, (que1, que2))
      else (h1 + 1, (que2, que1))
    | _, _ -> 
      if que2 = Leaf then (1, (que1, Leaf))
      else (1, (que2, Leaf))
  
(*--------OPERACJE NA KOLEJCE--------*)
  
let empty = Leaf
  
exception Empty
  
(*Funkcja sprawdzajaca czy dana kolejka nie jest pusta*)
let is_empty que =
  match que with
  | Leaf -> true
  | _ -> false
  
(*Laczy dwie kolejki priorytetowe*)  
let rec join que1 que2 =
  match (que1,que2) with
  | Leaf, _ ->  que2
  | _, Leaf ->  que1
  | Node (pr1, h1, l1, r1), Node (pr2, h2, l2, r2) ->
    if pr1 > pr2  then join que2 que1
    else
      let que3 = join r1 que2 in 
      let set = ustaw que3 l1 in
      let poddrzewa = snd set 
      in (Node (pr1, fst set, fst poddrzewa, snd poddrzewa))
  
(*Dodaje element (priorytet) do kolejki*)
let add priorytet que = 
  join que (Node (priorytet, 1, Leaf, Leaf))
  
(*Zwraca elemnt o najwyższym priorytecie, i kolejke powstala
  po usunieciu go*)
let delete_min que =
  match que with
  | Leaf -> raise Empty
  | Node (pr, h, l, r) -> (pr, join l r)

(*----------------TESTY--------------*)

let b = add 1 empty;;
let b = add 3 b;;
let b = add (-1) b;;
let b = add 2 b;;
let b = add 1 b;;
  
let c = add 10 empty;;
let c = add (-5) c;;
let c = add 1 c;;
let c = add 4 c;;
let c = add 0 c;;
  
let b = join b c;;
  
let (a,b) = delete_min b;;
assert (a = (-5));;
  
let (a,b) = delete_min b;;
assert (a = (-1));;
  
let (a,b) = delete_min b;;
assert (a = 0);;
  
let (a,b) = delete_min b;;
assert (a = 1);;
  
let (a,b) = delete_min b;;
assert (a = 1);;
  
let (a,b) = delete_min b;;
assert (a = 1);;
  
let (a,b) = delete_min b;;
assert (a = 2);;
  
let (a,b) = delete_min b;;
assert (a = 3);;
  
let (a,b) = delete_min b;;
assert (a = 4);;
  
let (a,b) = delete_min b;;
assert (a = 10);;
  
assert (try let _ = delete_min b in false with Empty -> true);;