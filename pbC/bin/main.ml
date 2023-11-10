(** @autor: Joel Vaz (a48906) *)

(**
  Function that verifies if the value is repeated in the line or column
  @param value : value to verify
  @param line : line to verify
  @param column : column to verify
  @param board : board to verify
  @return Verification of the line and column
*)
let board_verification value line column board =
  (* Ignore if the entry of the board is (0,0) *)
  if line = 0 && column = 0 then (true, true)
  else
    (* Verify the necessary lines *)
    let rec line_f index max result =
      if result = false then false (* Repeated value in board *)
      else if index > max then result (* End of board array *)
      else
        (* Return false if the value is repeated in the line *)
        let condition = not (board.(index).(column) = value) in
        line_f (index + 1) max condition
    in
    (* Verify the necessary columns *)
    let rec column_f index max result =
      if result = false then false (* Repeated value in board *)
      else if index > max then result (* End of board array *)
      else
        (* Return false if the value is repeated in the column *)
        let condition = not (board.(line).(index) = value) in
        column_f (index + 1) max condition
    in
    (line_f 0 (line - 1) true, column_f 0 (column - 1) true)

(**
  Function that verifies if the value is repeated in the line or column
  @param index : index of the restriction
  @param result : result of the restriction verification
  @param restrictions : number of restrictions
  @param restList : array of restrictions
  @param board : board to verify
  @return Verification of the restrictions
*)
let rec restrinction_verification index result restrictions restList board =
  if index = restrictions then result (* End of restrictions array *)
  else
    let x1, y1, x2, y2 = restList.(index) in
    if board.(x1).(y1) = 0 || board.(x2).(y2) = 0 then
      (* No restrictions apply *)
      restrinction_verification (index + 1) result restrictions restList board
    else if board.(x1).(y1) > board.(x2).(y2) then
      (* Respect the restrictions *)
      restrinction_verification (index + 1) true restrictions restList board
    else false (* Do not respect the restrictions *)

(**
  Function that makes backtracking
  @param n : value to put in board
  @param max_n : maximum value of board
  @param line : line to put the value
  @param column : column to put the value
  @param restList : array of restrictions
  @param restrictions : number of restrictions
  @param board : board to put the value
  @return Verification of the restrictions
*)
let rec backtracking n max_n line column restList restrictions board =
  if line = max_n then true (* Board done (line out of board) *)
  else if n > max_n then
    (* Do backtracking ('n' exceeded the maximum value) *)
    if line = 0 && column = 0 then false (* Backtracking is impossible *)
    else
      (* Reset initial value *)
      let () = board.(line).(column) <- 0 in
      if column = 0 then
        (* Decrements the line *)
        let old_value = board.(line - 1).(max_n - 1) in
        backtracking (old_value + 1) max_n (line - 1) (max_n - 1) restList
          restrictions board
      else
        (* Decrements the column *)
        let old_value = board.(line).(column - 1) in
        backtracking (old_value + 1) max_n line (column - 1) restList
          restrictions board
  else
    let line_verification, column_verification =
      board_verification n line column board
    in
    if line_verification = false || column_verification = false then
      (* Value already exists in board *)
      backtracking (n + 1) max_n line column restList restrictions board
    else
      (* Put the supposed correct value in the board *)
      let () = board.(line).(column) <- n in
      let verification =
        restrinction_verification 0 true restrictions restList board
      in
      if verification = true then
        (* Respects the restrictions *)
        if column = max_n - 1 then
          (* Go to the next line *)
          backtracking 1 max_n (line + 1) 0 restList restrictions board
        else
          (* Go to the next column *)
          backtracking 1 max_n line (column + 1) restList restrictions board
      else
        (* Do not respect the restrictions *)
        backtracking (n + 1) max_n line column restList restrictions board

(** Main function *)
let () =
  (* Board size *)
  let n = read_int () in
  if n >= 4 && n <= 6 then
    let board = Array.make_matrix n n 0 in
    (* Number of restrictions *)
    let restrictions = read_int () in
    (* The size of the array has to be greater or equal to 0 *)
    if restrictions >= 0 then
      (* Array that has the restrictions *)
      let restList = Array.make restrictions (0, 0, 0, 0) in
      let () =
        for rest = 0 to restrictions - 1 do
          Scanf.sscanf (read_line ()) "%d %d %d %d" (fun x1 y1 x2 y2 ->
              restList.(rest) <- (x1, y1, x2, y2))
        done
      in
      let result = backtracking 1 n 0 0 restList restrictions board in
      if result = true then
        (* Print final board *)
        for i = 0 to n - 1 do
          for j = 0 to n - 1 do
            if j = n - 1 then Printf.printf "%d" board.(i).(j)
            else Printf.printf "%d " board.(i).(j)
          done;
          print_endline ""
        done
      else print_endline "IMPOSSIBLE"
    else raise (invalid_arg "Invalid_value")
  else raise (invalid_arg "Invalid_value")
