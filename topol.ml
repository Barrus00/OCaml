(*Autor: Bartosz Ruszewski*)
(*Code review: Kamil Zwierzchowski (Numer albumu: kz418510)*)

open PMap

(*Wyjątek który podnosimy gdy w danym grafie istnieje pewien cykl*)
exception Cykliczne

(*Typ danych który określa status danego wierzchołka:
 -BIAŁY oznacza, że ten wierzchołek jeszcze nie został przetworzony
 -SZARY oznacza, że ten wierzchołek jest aktualnie przetwarzany
 -ZIELONY oznacza, że ten wierzchołek został już przetworzony*)
type kolor = 
  | BIALY
  | SZARY
  | ZIELONY

(*Funkcja ta tworzy mapę wszystkich wierzchołków danego grafu, 
  numerując każdy kolejny wierzchołek z listy numerami od 0 do n - 1,
  oraz listę kolejnych wierzchołków tego grafu *)
let zrob_mapa lst = 
  let mapa = ref (create compare)
  and i = ref 0 
  and wierzcholki = ref [] in
  let dodaj x = 
    if not (mem x !mapa) then
      begin
      mapa := add x !i !mapa;
      wierzcholki := x :: !wierzcholki;
      i := !i + 1;
      end
  in
  List.iter (fun (x, sasiedzi) ->
    dodaj x;
    List.iter dodaj sasiedzi) lst;
  (!mapa, (List.rev !wierzcholki));; 

(*Funkcja z danej mapy grafu, oraz listy wierzchołków w nim tworzy graf 
  w postaci tablicy par, z których pierwsza wartość oznacza numer wierzchołka,
  a druga wartość to lista wierzchołków do których można z niego dojsć *)
let zrob_graf lst = 
  let (mapa, wierz) = zrob_mapa lst in
  let dl = List.length wierz in 
  let graf = Array.make dl (0, []) in
  let dodaj_sasiadow v l =
      let i = find v mapa in
      let lista_sasiadow = List.fold_left (fun acc x ->
        (find x mapa) :: acc) (snd graf.(i)) l in
      graf.(i) <- (i, lista_sasiadow);
  in
  List.iter (fun x -> dodaj_sasiadow (fst x) (snd x)) lst;
  (graf, wierz);;

(*Funkcja DFS, która odwiedza wszystkie wierzchołki w grafie, 
  i sortuje je topologicznie. 
  Jeżeli w trakcie przechodzenia grafu trafimy na cykl, wtedy kończymy wyjątkiem, 
  ponieważ nie da się posortować go topologicznie*)
let dfs graf =
  let wynik = ref []
  and dl = Array.length graf in
  let odwiedzone = Array.make dl BIALY in
  let rec dfs_sort wierz = 
    if odwiedzone.(wierz) = SZARY then raise Cykliczne
    else
      begin 
        if odwiedzone.(wierz) = BIALY then
          begin
          odwiedzone.(wierz) <- SZARY;
          List.iter (fun x -> dfs_sort x) (snd graf.(wierz));
          wynik := wierz :: !wynik;
          odwiedzone.(wierz) <- ZIELONY;
          end
      end;
  in Array.iter (fun x -> dfs_sort (fst x)) graf;
  !wynik;;

(*Funkcja która z danej listy przenumerowanych wierzchołków, 
  zwraca listę wierzchołków z początkowymi numeramii*)
let wypisz lst wierz =
  List.rev (List.fold_left (fun acc x -> wierz.(x) :: acc) [] lst);;

(*Przyjmuje graf w postaci listy, i zwraca listę posortowanych 
  topologicznie jego wierzchołków *)
let topol lst = 
  let x = zrob_graf lst in
  wypisz (dfs (fst x)) (Array.of_list (snd x));;

(*TESTY: 
let l = (1,[1])::[];;
try(let _ = topol l in assert(false))
with Cykliczne -> ();;
let l =(1,[2])::(2,[3])::(3,[2])::[];;
try(let _ = topol l in assert(false))
with Cykliczne -> ();;
let l = (1,[2])::(2,[3])::(3,[4;5])::(4,[2;5])::[];;
try(let _ = topol l in assert(false))
with Cykliczne -> ();;*)