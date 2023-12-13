let map_state_to_char c = 
    match c with
    | 1 -> "X"
    | -1 -> "O"
    | _ -> " ";;

let rec reverse_list_inner l acc =
    match l with
    | [] -> acc
    | x :: xs -> reverse_list_inner xs (x :: acc);;

let reverse_list l = reverse_list_inner l [];;

let rec array_slice_inner arr i n acc =
    match i with
    | x when x = n + 1 -> reverse_list acc
    | _ -> array_slice_inner arr (i + 1) n (arr.(i) :: acc);;

let array_slice arr start ending = array_slice_inner arr start ending [];;

let print_line line =
    match line with
    | x :: y :: z :: _ -> print_endline (" " ^ (map_state_to_char x) ^ " | " ^ (map_state_to_char y) ^ " | " ^ (map_state_to_char z) ^ " ")
    | _ -> failwith "line length is not 3";;

let print_board state = 
    print_line (array_slice state 0 2);
    print_endline "-----------";
    print_line (array_slice state 3 5);
    print_endline "-----------";
    print_line (array_slice state 6 8);;

let read_move state = 
    print_endline "Enter your position (1-9):";
    let y = int_of_string (read_line ()) in
        if y < 1 || y > 9 || state.(y - 1) <> 0
        then failwith "Invalid move, you must pick an empty cell!"
        else y - 1;;

let rec list_sum_inner l acc =
    match l with
    | [] -> acc
    | x :: xs -> list_sum_inner xs (acc + x);;

let list_sum l = list_sum_inner l 0;;

let rec array_get_every_third_inner arr i acc = 
    match i with
    | x when x > 8 -> acc
    | _ -> array_get_every_third_inner arr (i + 3) (arr.(i) :: acc);;

let array_get_every_third arr i = array_get_every_third_inner arr i [];;

let check_rows state turn = 
    list_sum (array_slice state 0 2) = 3 * turn ||
    list_sum (array_slice state 3 5) = 3 * turn ||
    list_sum (array_slice state 6 8) = 3 * turn;;

let check_cols state turn = 
    list_sum (array_get_every_third state 0) = 3 * turn ||
    list_sum (array_get_every_third state 1) = 3 * turn ||
    list_sum (array_get_every_third state 2) = 3 * turn;;

let check_diagonals state turn = 
    list_sum [state.(0); state.(4); state.(8)] = 3 * turn ||
    list_sum [state.(2); state.(4); state.(6)] = 3 * turn;;

let check_win state turn = 
    check_rows state turn ||
    check_cols state turn ||
    check_diagonals state turn;;

let check_draw state =
    not (Array.exists (fun x -> x = 0) state);;

let print_win state turn =
    print_board state;
    print_endline ("Player " ^ (map_state_to_char turn) ^ " wins!");;

let print_draw state =
    print_board state;
    print_endline "Draw!";;

let rec play state turn =
    print_board state;
    let move = read_move state in
        print_endline (string_of_int move);
    state.(move) <- turn;
    if check_win state turn
    then print_win state turn
    else if check_draw state
    then print_draw state
    else play state (-turn);;
