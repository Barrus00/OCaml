(*
 * PSet - Polymorphic sets
 * Copyright (C) 1996-2003 Xavier Leroy, Nicolas Cannasse, Markus Mottl
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(*Autor: Bartosz Ruszewski*)
(*Code review: Krzysztof Lagodzinski, GRUPA 7, nr indeksu: 320548 *)

(*Typ t reprezentuje drzewo AVL i BST, którego elementami są unikalne przedziały liczbowe, 
  wartosci oznaczają kolejno: 
  lewe poddrzewo, przedział, prawe poddrzewo, wysokosc drzewa, ilosc elementow w danym drzewie*)
type t =
  | Empty
  | Node of t * (int * int)  * t * int * int

exception Not_found

(*Dodatkowa operacja dodawania,
  ktora kontroluje czy suma nie wykracza poza max_int
  Z zalozenia suma dwoch tych liczb powinna byc dodatnia, 
  jezeli okaze sie ona ujemna, to znaczy ze wykroczylismy poza max_int*)
let (++) x y =
  if x + y < 0 then max_int
  else if y > 0 && x > 0 && x + y = 0 then max_int
  else x + y

(*+++++++++++++++++FUNKCJE Z MODULU PSET+++++++++++++++++++*)

let empty = Empty

let is_empty x = 
  match x with
  | Empty -> true
  | _ -> false

let height t = match t with
  | Node (_, _, _, h, _) -> h
  | Empty -> 0

let size t = match t with
  | Node (_, _, _, _, s) -> s
  | Empty -> 0

let make l k r = Node (l, k, r, max (height l) (height r) + 1, (snd k ++ (- fst k) ++ 1) ++ size l ++ size r)

let bal l k r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 2 then
    match l with
    | Node (ll, lk, lr, _, _) ->
        if height ll >= height lr then make ll lk (make lr k r)
        else
          (match lr with
          | Node (lrl, lrk, lrr, _, _) ->
              make (make ll lk lrl) lrk (make lrr k r)
          | Empty -> assert false)
    | Empty -> assert false
  else if hr > hl + 2 then
    match r with
    | Node (rl, rk, rr, _, _) ->
        if height rr >= height rl then make (make l k rl) rk rr
        else
          (match rl with
          | Node (rll, rlk, rlr, _, _) ->
              make (make l k rll) rlk (make rlr rk rr)
          | Empty -> assert false)
    | Empty -> assert false
  else make l k r

let rec add_one x t = match t with 
  | Node (l, k, r, h, s) ->
      let c = compare x k in
      if c = 0 then Node (l, x, r, h, s)
      else if c < 0 then
        let nl = add_one x l in
        bal nl k r
      else
        let nr = add_one x r in
        bal l k nr
  | Empty -> Node (Empty, x, Empty, 1, snd x ++ (-(fst x)) ++ 1 )  

let rec min_elt t = match t with
  | Node (Empty, k, _ , _, _) -> k
  | Node (l, _, _, _, _) -> min_elt l
  | Empty -> raise Not_found 

let rec max_elt t = match t with
  | Node (_, k, Empty, _, _) -> k
  | Node (_, _, r, _, _) -> max_elt r
  | Empty ->  raise Not_found

let rec remove_min_elt t = match t with
  | Node (Empty, _, r, _, _) -> r
  | Node (l, k, r, _, _) -> bal (remove_min_elt l) k r
  | Empty -> invalid_arg "PSet.remove_min_elt"

let rec remove_max_elt t = match t with
  | Node (l, _, Empty, _, _) -> l
  | Node (l, k, r, _, _) -> bal l k (remove_max_elt r)
  | Empty -> invalid_arg "PSet.remove_min_elt"     

let rec join l v r =
  match (l, r) with
  | (_, Empty) -> add_one v l
  | (Empty, _) -> add_one v r
  | (Node(ll, lv, lr, lh, _), Node(rl, rv, rr, rh, _)) ->
    if lh > rh + 2 then bal ll lv (join lr v r) 
    else if rh > lh + 2 then bal (join l v rl) rv rr 
    else make l v r

let iter f t =
  let rec loop = function
    | Empty -> ()
    | Node (l, k, r, _, _) -> loop l; f k; loop r in
  loop t

let fold f t acc =
  let rec loop acc = function
    | Empty -> acc
    | Node (l, k, r, _, _) ->
          loop (f k (loop acc l)) r in
  loop acc t  

(*++++++++++++++++++FUNKCJE POMOCNICZE+++++++++++++++++++++*)

(*Dla danych dwoch nakladajacych sie przedzialow, zwraca ich sume*)
let sum (l_v1, r_v1) (l_v2, r_v2) = (min l_v1 l_v2, max r_v1 r_v2)

(*Dla danych dwoch przedzialow, zwraca TRUE gdy na siebie nachodza,
  lub FALSE w przeciwnym wypadku*)
let overlap_sum a b =
  let rec overlap_pom (l_v1, r_v1) (l_v2, r_v2) b x =
    if (l_v1 + x >= l_v2 && l_v1 - x <= r_v2) 
    || (r_v1 - x <= r_v2 && r_v1 + x >= l_v2) 
    || (l_v1 >= l_v2 && l_v1 <= r_v2) then true
    else if b = 0 then (overlap_pom (l_v2, r_v2) (l_v1, r_v1) 1 x)
    else false
  in overlap_pom a b 0 1

(*Funkcja sprawdzajaca czy dana liczba nalezy do danego przedzialu, 
  jednoczesnie zwraca jej polozenie wzgledem przedzialu*)
let cmp x (minv, maxv) =
  if x >= minv && x <= maxv then 0
  else if x < minv then (-1)
  else 1

(*Podmienia maksymalny element w drzewie*)  
let rec change_min_elt x t = match t with
  | Node (Empty, _, r, _, _) -> make Empty x r
  | Node (l, k, r, _, _) -> make (change_min_elt x l) k r
  | Empty -> invalid_arg "PSet.remove_min_elt"

(*Podmienia minimalny element w drzewie*)  
let rec change_max_elt x t = match t with
  | Node (l, _, Empty, _, _) -> make l x Empty
  | Node (l, k, r, _, _) -> make l k (change_max_elt x r)
  | Empty -> invalid_arg "PSet.remove_min_elt"

(*++++++++++++++FUNCKJE OKRESLONE W MLI++++++++++++++++++++*)

(*Funkcja zwracająca ilość liczb w drzewie mniejszych od x*)
let below x t = 
  let rec below_pom x acc = function
    | Empty -> acc
    | Node (l, k, r, h, s) ->
      if x >= fst k && x <= snd k then (acc ++ size l ++ ((x ++ (-fst k) ++ 1)))
      else if x < fst k then below_pom x acc l
      else below_pom x (acc ++ size l ++ (snd k ++ (- fst k) ++ 1)) r
  in below_pom x 0 t

(*Funkcja która zwraca listę rosnąco ustawionych przedziałów w drzewie*)  
let elements t = 
  let rec loop acc = function
      Empty -> acc
    | Node(l, k, r, _, _) -> loop (k :: loop acc r) l in
  loop [] t

(*Funkcja sprawdzająca czy dana liczba x zawiera sie w którymś przedziale drzewa*)
let rec mem x = function
  | Empty -> false
  | Node (l, (lv, rv), r, _, _) ->
    let c = cmp x (lv, rv) in
    match c with
    | 0 -> true
    | 1 -> mem x r
    | -1 -> mem x l

(*Funkcja zwraca 3 wartosci: 
    1 - drzewo AVL BST, zawierające przedziały ostro mniejsze od liczby x
    2 - zmienna typu bool, która określa czy dana liczba należy do drzewa
    3 - drzewo AVL BST, zawierające przedziały ostro większe od liczby x*)    
let split x t =
  let rec loop x = function
  | Empty -> (Empty, false, Empty)
  | Node (l, ((minv, maxv) as v), r, _, _) ->
    let c = cmp x v in
      if c = 0 then
        if x = minv && x = maxv then (l, true, r)
        else if minv = x then (l, true, add_one (x + 1, maxv) r)
        else if maxv = x then (add_one (minv, x - 1) l, true, r)
        else (add_one (minv, x - 1) l, true, add_one (x + 1, maxv) r)
      else if c < 0 then
        let (ll, pres, rl) = loop x l in (ll, pres, join rl v r)
      else
        let (lr, pres, rr) = loop x r in (join l v lr, pres, rr)
    in let setl, pres, setr = loop x t in
    setl, pres, setr  

(*Funkcja zwraca drzewo które powstaje po usunięciu z niego danego przedziału
  Dziala w czasie logarytmicznym poniewaz split i join maja taka zlonosc*)    
let remove (minv, maxv) t =
  let nl, _, _ = split minv t
  and _, _, nr = split maxv t in
    let czy_nl = nl <> Empty and czy_nr = nr <> Empty in
    if czy_nl && czy_nr then
      join nl (min_elt nr) (remove_min_elt nr)
    else if czy_nr then nr
    else nl

(*Funkcja która dodaje przedział do drzewa, uwzględniając to, 
  że przedziały mogą na siebie nachodzić. Dzielimy drzewo na dwa poddrzewa,
  takie ktore zawieraja przedzialy mieszczace sie poza tym ktory chcemy dodac.
  (Z lewej te ostro mniejsze, z prawej ostro wieksze)
  Nastepnie laczymy je, jednoczesnie sprawdzajac czy mozemy rozszerzyc 
  dodatkowo jakis przedzial i dodajac nasz okreslony przedzial. 
  Dziala w czasie logarytmicznym poniewaz split ma taka zlozonosc*)    
let rec add ((minv,maxv) as news) t =
  match t with
  | Empty -> Node (Empty, (minv, maxv), Empty, 1, maxv ++ (-minv) ++ 1)
  | Node (l, (lv, rv), r, h, _) -> 
    let nl, _, _ = split minv t
    and _, _, nr = split maxv t in
      match nl, nr with
      | Empty, Empty ->  
        Node (Empty, (minv, maxv), Empty, 1, maxv ++ (-minv) ++ 1)
      | Empty, pp -> 
        let minpp = min_elt pp in
        if overlap_sum minpp news 
            then change_min_elt (sum minpp news) pp
        else add_one news pp
      | ll, Empty -> 
        let maxll = max_elt ll in
        if overlap_sum (maxll) news 
            then change_max_elt (sum (maxll) news) ll
        else add_one news ll
      | ll, pp -> 
        let maxll = max_elt ll and minpp = min_elt pp in
        let news, pp = 
            if overlap_sum minpp news
                then sum minpp news, remove_min_elt pp
            else news, pp in
        let news, ll = 
            if overlap_sum maxll news 
                then sum maxll news, remove_max_elt ll
            else news, ll in
        join ll news pp

(* TESTY
let a = add (0, 5) empty;;
let a = add (7, 8) a;;
let a = add (-3, -3) a;;
let a = add (10, 13) a;;
assert(elements a = [(-3, -3); (0, 5); (7, 8); (10, 13)]);;
assert(below 8 a = 9);;
let b = add (6, 6) a;;
let b = remove (6, 6) b;;
let b = add (-100, -5) b;;
let b = add (-4, 6) b;;
assert(elements b = [(-100, 8); (10, 13)]);;
assert(below 10 b = 110);;
*)