(*Autor: Bartosz Ruszewski *)
(*Code review: Grzegorz Zaleski (id: 418494) *)

(*Wyjatek, ktory rzucamy gdy znajdziemy najlepsze rozwiązanie*)
exception Rozwiazanie of int

(*Funkcja która, pozwala określić, czy jestesmy w stanie uzyskac koncowa 
  konfiguracje (Jeden pusty kubeczek lub chociaz jeden pelen)*)
let czy_ok (x, y) =
  x = y || (y = 0 && x <> 0)

(*Warunek konieczny istnienia rozwiązania, tzn wartosci koncowe kubeczkow, musza byc
  podzielne przez nwd pojemnosci*)
let nwd tab =
  let rec nwd_pom a b =
    if b = 0 then a else nwd_pom b (a mod b) 
  in
  let nwd_tab = Array.fold_left (fun a (x, _) -> nwd_pom a x) 0 tab in
  Array.fold_left (fun a (_, y) -> a && y mod nwd_tab = 0) true tab  

(*Tablica hashujaca, ktora przechowuje wszyskie stany ktore zostaly juz odwiedzone*)
let odwiedzone = Hashtbl.create 42000

(*Kolejka na ktorej przechowujemy wszystkie stany z ktorych bedziemy sie poruszac,
  oraz liczbe krokow jakie nalezy wykonać, żeby do nich dojść*)
let q = Queue.create ()

(*Generuje wszystkie mozliwe stany do ktorych jestesmy w stanie dojsc z danego miejsca,
  i wrzuca je na kolejke, po uprzednim sprawdzeniu czy juz wczesniej w nich nie bylismy.
  Jezeli uda nam sie dojsc do prawidlowej konfiugracji, zwracamy wynik poprzez wyjatek,*)
let stany (b_up, kroki) poj komp =
  let tab = Array.copy b_up in
  let n = Array.length tab in
  (*Kontroluje czy dany stan nie jest wynikiem*)
  let sprawdz akt =
    if akt = komp then 
      begin
      Hashtbl.clear odwiedzone;
      Queue.clear q;
      raise (Rozwiazanie (kroki + 1))
      end
    else if not (Hashtbl.mem odwiedzone akt) then
      begin
      Queue.add (Array.copy akt, (kroki + 1)) q;
      Hashtbl.add odwiedzone (Array.copy akt) (kroki); 
      end
  and przywroc x = tab.(x) <- b_up.(x)
  in
  for i = 0 to n - 1 do
    (* Uzupelniamy i-ta szklanke do pelna*)
    tab.(i) <- poj.(i);
    sprawdz tab;
    (*Oprozniamy i-ta szklanke*)
    tab.(i) <- 0;
    sprawdz tab;
    przywroc i;
    (*Przelewamy ze szklanki i-tej do j-tej*)
    for j = 0 to n - 1 do 
      if tab.(i) + tab.(j) > poj.(j) then
        begin
          tab.(i) <- tab.(i) - poj.(j) + tab.(j);
          tab.(j) <- poj.(j);
        end
      else 
        begin
        tab.(j) <- tab.(j) + tab.(i);
        tab.(i) <- 0;
        end;
      sprawdz tab;
      przywroc i;
      przywroc j;
    done;
  done

(*Fuckja przeszukujaca wszerz po kolejnych uzyskiwanych stanach*)
let bfs tab =
  let wynik = Array.map (fun (_, y) -> y) tab
  and poj = Array.map (fun (x, _) -> x) tab 
  and start = Array.make (Array.length tab) 0 in
  if start = wynik then 0
  else if not (nwd tab) then (-1)
  else begin
    Queue.add (start, 0) q;
    Hashtbl.add odwiedzone start 0;
    while not (Queue.is_empty q) do
      stany (Queue.take q) poj wynik;
    done;
  (-1);
  end
  
let przelewanka tab =
  if tab = [||] then 0
  else if (Array.exists czy_ok tab) then
    try bfs tab with
    | Rozwiazanie (x) -> x
  else (-1)