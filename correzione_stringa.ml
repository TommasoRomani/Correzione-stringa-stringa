exception NotFound;;

(* Converte una stringa in una lista di caratteri *)
let string_to_list str = List.of_seq (String.to_seq str);;

(* Converte una lista di caratteri in una stringa *)
let list_to_string lst = String.of_seq (List.to_seq lst);;



(* Genera tutte le stringhe possibili rimuovendo un carattere alla volta,
  generando quindi delle possibili stirghe che andranno aggiunte alla frontiera
  per essere espanse *)
let rec cancella_a len = function
    | [] -> []
    | h :: t -> if len = 0 
                then t 
                else h :: cancella_a (len-1) t

let cancellazioni lst = 
  let rec aux acc len = function 
    | [] -> acc
    | h :: t -> if len = 0 
                then aux acc (len-1) t 
                else aux ((cancella_a (len-1) lst) :: acc) (len-1) t 
in aux [] (List.length lst) lst

(* Genera tutte le stringhe possibili scambiando coppie di caratteri adiacenti,
  generando quindi delle possibili stirghe che andranno aggiunte alla frontiera
  per essere espanse *)
let rec scambia_a len = function
    | [] | [_] -> []
    | a :: b :: t -> if len = 0
                    then b :: a :: t 
                    else a :: scambia_a (len-1) (b :: t)

let scambi lst =
  let rec aux acc len = function
    | [] | [_] -> acc
    | h :: t -> if len = 0
                then aux acc (len-1) t 
                else aux ((scambia_a (len-1) lst) :: acc) (len-1) t
  in
  aux [] (List.length lst -1) lst

(* Espande la frontiera con tutte le possibili stringhe generate,
  e controlla se vengono aggiunte delle stringhe già viste *)
let espandi frontiera x visti =
  List.fold_left (fun acc prossimo -> 
                    if List.mem prossimo visti
                    then acc 
                    else prossimo :: acc) 
  frontiera (cancellazioni x @ scambi x);;

(* BFS per trovare il numero minimo di passi per trasformare x in y *)
let correggi_stringa x y =
  let rec bfs frontiera prossima_frontiera visti passi espansi =
  match frontiera with
  | [] -> 
    (match prossima_frontiera with
     | [] -> 
       Printf.printf "Trasformazione non trovata\n";
       Printf.printf "Numero di nodi espansi: %d\n" espansi;
       raise NotFound
     | _ -> 
       Printf.printf "Cambio di livello di profondità: %d\n" (passi + 1);
       bfs prossima_frontiera [] visti (passi + 1) espansi)
  | h :: t when h = y -> 
    Printf.printf "Nodo finale raggiunto: %s con passi: %d\n" (list_to_string h) passi;
    Printf.printf "Numero di nodi espansi: %d\n" espansi;
    passi
  | h :: t ->
    let nuovi_visti = h :: visti in
    let nuova_frontiera = espandi prossima_frontiera h nuovi_visti in
    bfs t nuova_frontiera nuovi_visti passi (espansi + List.length nuova_frontiera)
  in bfs [x] [] [] 0 1;;



let main () =
  Printf.printf "Inserive il valore di x: ";
  let x_str = read_line () in
  let x = string_to_list x_str in

  Printf.printf "Inserire il valore di y: ";
  let y_str = read_line () in
  let y = string_to_list y_str in

  try
    let steps = correggi_stringa x y in
    Printf.printf "Numero di passi: %d\n" steps
  with
  | NotFound -> Printf.printf "Trasformazione non trovata\n"

let () = main ();;
