(*Autor: Bartosz Ruszewski (id:418400)*)
(*Code review: Mateusz Nowakowski (id:418323) (GRUPA 3)*)

(*Przedział typu [a,b] gdy bool jest TRUE, 
  dla bool FALSE przedział typu (-inf,a] u [b,+inf]*)
type wartosc = (float * float * bool) 

(*..........FUNKCJE POMOCNICZE..........*)

(*Zbiór pusty*)
let pusty = (nan, nan, false)

(*Zbiór który zawiera tylko 0 [0;0]*)
let zerowy = (0., 0., true)

(*Zbiór liczb rzeczywistych (-inf,inf)*)
let rzecz = (neg_infinity, infinity, true)

(*Zwraca a z przedziału [a,b] i (-inf,a] u [b,inf]*)
let lewa (l, _, _ : wartosc) = l

(*Zwraca a z przedziału [a,b] i (-inf,a] u [b,inf]*)
let prawa (_, p, _ : wartosc) = p

(*Zwraca wartość logiczną, która określa rodzaj przedziału. 
  (TRUE gdy [a,b], FALSE gdy (-inf,a] u [b,inf)*)
let inf_inf (_, _, t : wartosc) = t

(*Sprawdza czy wartość x jest nan*)
let czy_nan x = 
  compare x nan = 0

(*Sprawdza czy przedział zawiera jakieś wartości*)
let czy_pusty (l, p, _ : wartosc) =
  czy_nan l && czy_nan p

(*Sprawdza czy wartość liczby x jest skończona*)
let jest_wartosc x =
  if czy_nan x then false
  else if x = neg_infinity then false
  else if x = infinity then false
  else true

(*Dla l>p zamienia przedział niepoprawny typu [l,p], gdzie l>p na poprawny [p,l],
 W zależności od parametru "czy_laczyc" zamienia przedział typu (-inf,p) u (l,+inf) 
 niepoprawny na poprawny tj. wyżej, lub go łączy w zbiór R*)
let popraw_parametr (l, p, t : wartosc) czy_laczyc  =
  match t with
  | false ->
    if l >= p && czy_laczyc then (neg_infinity, infinity, true : wartosc)
    else if l >= p && not czy_laczyc then (p, l, false : wartosc)
    else (l, p, false : wartosc)
  | true -> 
    if l <= p then (l, p, true : wartosc)
    else (p, l, true : wartosc)

(*Zamienia przedział niepoprawny na poprawny, przy czym może go łączyć*)
let popraw a =
  popraw_parametr a true

(*Zamienia przedział niepoprawny na poprawny, przy czym nie może go łączyć*)
let zamien a =
  popraw_parametr a false                                 

(*Zwraca przedział, który jest przeciwnym przedziałem dla przedziału x 
  (Czyli zwraca przedział x pomnożony razy -1)*)
let przeciwny_przedzial x =                              
  popraw ((-.(prawa x)), (-.(lewa x)), inf_inf x : wartosc)

(*Zwraca maksymalną wartość z 4 wartości*)
let max4 a b c d = 
  max (max a b) (max c d)

(*Zwraca minimalną wartość z 4 wartości*)
let min4 a b c d = 
  min (min a b) (min c d)

(*Mnożenie dwóch wartości, z deklaracją że inf * 0 = 0 i -inf * 0 = 0 *)
let pomnoz a b =
  if (a = 0. || b = 0.) 
  && (b = infinity || a = infinity || b = neg_infinity || a = neg_infinity) then 0.
  else a *. b

(*Zwraca odwrotność liczby a*)
let odwroc a = 1. /. a

(*........../FUNKCJE POMOCNICZE..........*)


(*...........KONSTRUKTORY................*) 

let wartosc_dokladnosc x p = 
  let procent = ((x *. p) /. 100.0) in 
      if x -. procent < x +. procent then (x -. procent, x +. procent, true : wartosc)
      else (x +. procent, x -. procent, true : wartosc)

let wartosc_od_do x y = (x, y, true : wartosc)

let wartosc_dokladna x = (x, x, true : wartosc)

(*..........\KONSTRUKTORY................*)

(*...........SELEKTORY...................*)

let in_wartosc  (l, p, t : wartosc) x =
  match t with
  | true  -> l <= x && x <= p
  | false -> x <= l || x >= p 

let max_wartosc  a =
  if czy_pusty a then nan
  else
    match inf_inf a with
    | true  -> prawa a
    | false -> infinity

let min_wartosc a =
  if czy_pusty a then nan
  else
    match inf_inf a with
    | true  -> lewa a
    | false -> neg_infinity

let sr_wartosc (x : wartosc) = 
  if jest_wartosc (max_wartosc x) || jest_wartosc (min_wartosc x)
    then (((max_wartosc x) +. (min_wartosc x)) /. 2.0)
  else nan

(*Odwraca przedział x*)
let odwrocony_przedzial x =
  match inf_inf x, in_wartosc x 0. with 
  | true, true   -> 
      if lewa x = 0. 
        then zamien (odwroc (abs_float (lewa x)), odwroc (prawa x), true : wartosc)
      else if prawa x = 0. 
        then zamien (odwroc (lewa x), odwroc (-0.0), true : wartosc)
      else zamien (odwroc (lewa x), odwroc (prawa x), false : wartosc)
  | true, false  -> zamien (odwroc (lewa x), odwroc (prawa x), true : wartosc)
  | false, false -> zamien (odwroc (lewa x), odwroc (prawa x), true : wartosc)
  | false, true  -> zamien (odwroc (lewa x), odwroc (prawa x), false : wartosc)

(*..........\SELEKTORY...............*)

(*...........DZIALANIA...............*)

(*FUNCKCJA REKURENCYJNA, Maksymalnie dwa wykonania, gdy nie uda się dopasować 
  wartości true lub false to zamieniamy przedziały ze sobą i je dodajemy, 
  wtedy na pewno uda nam się dopasować wartości logiczne 
  (Dodawanie przedziałów jest przemienne)*)
let rec plus  (a : wartosc) (b : wartosc) = 
  if czy_pusty a || czy_pusty b then pusty
  else
    match inf_inf a, inf_inf b with
    | true, true  -> 
      (min_wartosc a +. min_wartosc b, max_wartosc a +. max_wartosc b, true : wartosc)
    | false, _    -> 
      popraw (lewa a +. max_wartosc b, prawa a +. min_wartosc b, false : wartosc)
    | true, false -> 
      plus b a


let minus (a : wartosc) (b : wartosc) = 
  plus a (przeciwny_przedzial b)

(*FUNCKCJA REKURENCYJNA, Maksymalnie dwa wykonania, gdy nie uda się 
  dopasować wartości true lub false to zamieniamy przedziały ze sobą i je mnożymy, 
  wtedy na pewno uda nam się dopasować wartości logiczne 
  (Mnożenie przedziałów jest przemienne)*)
let rec razy (a : wartosc) (b : wartosc)= 
  if czy_pusty a || czy_pusty b then pusty
  else if b = zerowy || a = zerowy then zerowy
  else 
    let 
    q = pomnoz (lewa a) (prawa b) and
    w = pomnoz (lewa a) (lewa b) and
    e = pomnoz (prawa a) (lewa b) and
    r = pomnoz (prawa a) (prawa b) in
      match (((inf_inf a), (in_wartosc a 0.)), ((inf_inf b), (in_wartosc b 0.))) with
      | (true, _), (true, _)           -> wartosc_od_do (min4 q w e r) (max4 q w e r)
      | (false, _), (true, true)       -> rzecz
      | (false, _), (true, false)      ->
		      if lewa b > 0. then popraw ((max q w), (min e r), false : wartosc)
          else popraw ((max e r), (min q w), false : wartosc)
      | (false, _), (false, true)      -> rzecz
      | (false, false), (false, false) -> popraw((max q e), (min w r), false : wartosc)
      | (_, _), (_, _)                 -> razy b a


let podzielic (a : wartosc) (b : wartosc) =
  if czy_pusty a || czy_pusty b then pusty
  else if b = zerowy then pusty
  else razy a (odwrocony_przedzial b);;

(*.........../DZIALANIA..............*)

(*..............TESTY................*)
let eps = 1e-6;;

let a = wartosc_od_do 3. 7.;;                        (* [3., 7.]                      *)

assert(min_wartosc a = 3.0);;
assert(max_wartosc a = 7.0);;
assert(in_wartosc a 4.);;
assert(not (in_wartosc a 2.));;

let b = wartosc_od_do (-2.) 5.;;                     (* [-2., 5.]                     *)

assert(sr_wartosc b = 1.5);;
assert(min_wartosc b = -2.);;
assert(max_wartosc b = 5.);;
assert(in_wartosc b (-0.));;

let c = podzielic a b;;                              (* [-inf, -1.5] U [0.6, inf]     *)

assert(not (in_wartosc c 0.));;
assert(in_wartosc c 100.);;

let d = podzielic c b;;                              (* [-inf, -0.3] U [0.12, inf]    *)

assert(compare (sr_wartosc d) nan = 0);;
assert(in_wartosc d (-3. /. 10. -. eps));;
assert(not (in_wartosc d (-3. /. 10. +. eps)));;
assert(max_wartosc d = infinity);;
assert(min_wartosc d = neg_infinity);;

let e = plus d (wartosc_dokladna 2.);;               (* [-inf, 1.7] U [2.12, inf]     *)

assert(in_wartosc e 0.);;
assert(in_wartosc e 1.7);;
assert(in_wartosc e 2.12);;
assert(not (in_wartosc e 1.700000000001));;
assert(not (in_wartosc e 2.119999999999));;

let h = wartosc_dokladna 0.;;                        (* [0., 0.]                      *)
let i = wartosc_dokladna (-0.);;                     (* [0., 0.]                      *)

assert((min_wartosc h) = (min_wartosc i));;
assert((max_wartosc h) = (max_wartosc i));;
assert((sr_wartosc h) = (sr_wartosc i));;
assert((min_wartosc h) = 0.);;
assert((max_wartosc h) = 0.);;
assert((sr_wartosc h) = 0.);;