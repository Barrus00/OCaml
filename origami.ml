(*Autor: Bartosz Ruszewski*)
(*Code review: Stanisław Gutowski, grupa 6, id: sg417898*)

(*Typ który charakteryzuje punkt, zawiera parę wspołrzędnych*)
type point = float * float 

(*Typ funkcji kartka, która określa przez ile warst kartki 
  przebije się szpilka wbita w punkcie*)
type kartka = point -> int 

(*Epsilon umożliwiający dokładniejsze porówynwanie wartośći float*)
let ep = 1e-8

(*Funkcja określa położenie punktu względem prostej:
    1) 0 -> punkt znajduje się na prostej
    2) 1 -> punkt znajduje się po prawej stronie prostej
    3) -1 -> punkt znajduje się po lewej stronie*)
let pozycja (y, x ,c) (a, b) =
  let sum = y *. b +. x *. a +. c in
  if sum = 0. then 0
  else if sum > 0. then 1
  else -1

(*Zwraca odległość między dwoma punktami na płaszczyźnie*)
let odleglosc (x1, y1) (x2, y2) =
  sqrt ((x2 -. x1) *. (x2 -. x1) +. (y2 -. y1) *. (y2 -. y1))

(*Zwraca prostą w postaci ogólnej, przechodzącą przez dwa dane punkty*)  
let daj_prosta ((x1, y1) : point) ((x2, y2) : point) =
  let a = y1 -. y2 and b = x1 -. x2 in
  (b, -.a, -.(b *. y1 -. a *. x1))

(*Zwraca współrzędne danego punktu po odbiciu względem danej prostej*)
let odbij_punkt (x1, y1) (a, b, c) =
  ((x1 *. (a *. a -. b *. b) -. 2. *. b *. (a *. y1 +. c)) /. (a *. a +. b *. b),
   (y1 *. (b *. b -. a *. a) -. 2. *. a *. (b *. x1 +. c)) /. (a *. a +. b *. b)) 

(*Funkcja typu kartka, ktora okresla czy szpilka wbita w danym punkcie 
  przechodzi przez kartkę w kształcie prostokątu, dodatkowo dopuszczamy pewne 
  niedokładności związane z floatami (dlatego porównujemy z dodatkowym epsilonem)*)
let prostokat (a, b) (c, d) = function ((x, y) : point) ->
  if a -. ep <= x && x <= c +. ep  && b -. ep <= y && y <= d +. ep then 1 
  else 0

(*Funkcja typu kartka, ktora okresla czy szpilka wbita w danym punkcie 
  przechodzi przez kartkę w kształcie koła*)
let kolko (k : point) r = function (p : point) ->
  if odleglosc p k <= r then 1
  else 0

(*Określa przez ile warstw kartki zagiętej względem danej prostej
  przebije się szpilka wbita w dany punkt na kartce*)  
let zloz p1 p2 (papier : kartka) =
  function (punkt : point) ->  
  let prosta = daj_prosta p1 p2 in
  match pozycja prosta punkt with
  | 0 -> papier punkt
  | 1 -> 0
  | -1 -> (papier punkt) + (papier (odbij_punkt punkt prosta))
  
(*Określa przez ile warstw kartki pozaginanej względem danej 
  listy prostych przebije się szpilka wbita w dany punkt na kartce*) 
let skladaj lista kartka =
  List.fold_left (fun a (p1, p2) -> zloz p1 p2 a) kartka lista

(*TESTY*)
(*let zle = ref 0 and ok = ref 0

let test n = function
    | true -> incr ok (*; printf "Test %d OK\n" n*)
    | false -> incr zle ; printf "Test %d ZLE\n" n

let k = prostokat (-10., -10.) (-10., 5.);;

test 1 (1= (k (-10., -10.)));;
test 2 (1= (k (-10., 0.)));;
test 3 (1= (k (-10., 5.)));;
test 4 (0= (k (-10., 5.5)));;
test 5 (0= (k (-10., -10.5)));;
test 6 (0= (k (-11., 3.)));;

let k = kolko (3., 0.) 5.;;

test 7 (1= (k (3., 0.)));;
test 8 (1= (k (3., 5.)));;
test 9 (1= (k (0., 4.)));;
test 10 (0= (k(-2., 2.)));;
test 11 (0= (k(2., 5.)));;
test 12 (0= (k(8., 1.)));; 
*)