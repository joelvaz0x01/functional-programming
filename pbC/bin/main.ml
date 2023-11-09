let board_verification value line column board =
  if line = 0 && column = 0 then (true, true)
    (* Ignorar se for a entrada do board for (0,0) *)
  else
    let rec line_f index max result =
      (* Verifica as linhas necessárias *)
      if result = false then false (* Valor repetido em board *)
      else if index > max then result (* Fim do array board *)
      else
        let condition = not (board.(index).(column) = value) in
        (* Retorna false caso o valor esteja repetido na linha *)
        line_f (index + 1) max condition
    in
    let rec column_f index max result =
      (* Verifica as colunas necessárias *)
      if result = false then false (* Valor repetido em board *)
      else if index > max then result (* Fim do array board *)
      else
        let condition = not (board.(line).(index) = value) in
        (* Retorna false caso o valor esteja repetido na coluna *)
        column_f (index + 1) max condition
    in
    (line_f 0 (line - 1) true, column_f 0 (column - 1) true)

let rec restrinction_verification index result restrinctions restList board =
  if index = restrinctions then result (* Fim do array das restrinções *)
  else
    let x1, y1, x2, y2 = restList.(index) in
    if board.(x1).(y1) = 0 || board.(x2).(y2) = 0 then
      (* Não se aplicam restrinções *)
      restrinction_verification (index + 1) result restrinctions restList board
    else if board.(x1).(y1) > board.(x2).(y2) then
      (* Respeita as restrinções *)
      restrinction_verification (index + 1) true restrinctions restList board
    else false (* Não respeita as restrinções *)

let rec backtracking n max_n line column restList restrinctions board =
  if line = max_n then true
    (* Board feito com sucesso (linha passa para fora do board) *)
  else if n > max_n then
    (* Fazer backtracking (n exedeu o valor máximo) *)
    if line = 0 && column = 0 then false (* Impossível fazer backtracking *)
    else
      let () = board.(line).(column) <- 0 in
      (* Reposição de valor inicial *)
      if column = 0 then
        (* Decrementa a linha *)
        let old_value = board.(line - 1).(max_n - 1) in
        backtracking (old_value + 1) max_n (line - 1) (max_n - 1) restList
          restrinctions board
      else
        (* Decrementa a coluna *)
        let old_value = board.(line).(column - 1) in
        backtracking (old_value + 1) max_n line (column - 1) restList
          restrinctions board
  else
    let line_verification, column_verification =
      board_verification n line column board
    in
    if line_verification = false || column_verification = false then
      (* Valor já existe em board *)
      backtracking (n + 1) max_n line column restList restrinctions board
    else
      let () = board.(line).(column) <- n in
      (* Coloca no board o suposto valor correto *)
      let verification =
        restrinction_verification 0 true restrinctions restList board
      in
      if verification = true then
        (* Respeita as restrinções *)
        if column = max_n - 1 then
          (* Avança para a próxima linha *)
          backtracking 1 max_n (line + 1) 0 restList restrinctions board
        else
          (* Avança para a próxima coluna *)
          backtracking 1 max_n line (column + 1) restList restrinctions board
      else
        (* Não respeita as restrinções *)
        backtracking (n + 1) max_n line column restList restrinctions board

let () =
  let n = read_int () in
  (* Leitura da dimenção de board *)
  if n >= 4 && n <= 6 then
    let board = Array.make_matrix n n 0 in
    (* Array board do tipo matrix *)
    let restrinctions = read_int () in
    (* Leitura do número de restinções *)
    if restrinctions >= 0 then
      (* O tamanho do array tem que ser maior ou igual a 0 *)
      let restList = Array.make restrinctions (0, 0, 0, 0) in
      (* Array que conterá as restrinções *)
      let () =
        for rest = 0 to restrinctions - 1 do
          Scanf.sscanf (read_line ()) "%d %d %d %d" (fun x1 y1 x2 y2 ->
              restList.(rest) <- (x1, y1, x2, y2))
        done
      in
      let result = backtracking 1 n 0 0 restList restrinctions board in
      if result = true then
        for i = 0 to n - 1 do
          for j = 0 to n - 1 do
            if j = n - 1 then Printf.printf "%d" board.(i).(j)
              (* Print board final *)
            else Printf.printf "%d " board.(i).(j)
          done;
          print_endline ""
        done
      else print_endline "IMPOSSIBLE" (* Resolução impossível *)
    else raise (invalid_arg "Invalid_value") (* Argumento inválido *)
  else raise (invalid_arg "Invalid_value")
(* Argumento inválido *)
