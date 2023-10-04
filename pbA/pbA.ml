(* Declaração de números no módulo Zatith *)
let zero, um, dois, tres, seis = Z.zero, Z.one, Z.of_int 2, Z.of_int 3, Z.of_int 6

(* Declaração das operações no módulo Zatith *)
let ( +! ), ( -! ), ( *! ), ( /! ) = Z.add, Z.sub, Z.mul, Z.div

(*
  Função do somarório da primeira expressão
  Argumentos:
    n -> número de ordem da expressão
    exp -> função first_exp
    count_exp -> contador da função first_exp
    acc -> acumulador das somas do somatório
*)
let rec sum_k_n k n exp count_exp acc =
  if k>n-2 then acc (* Devolução do resultado do somatório (condição de paragem) *)
  else 
    let (inter_exp1,_), (inter_exp2,_)  = exp k (count_exp), exp (n-k-1) (count_exp) in (* Declaração das expressões do somatório *)
    let sum_exp = inter_exp1 *! inter_exp2 in (* Cálculo da expressão do somatório *)
    sum_k_n (k+1) n exp count_exp (acc +! sum_exp)

(*
  Função da primeira expressão (sem momoização) - Verificou-se que o 'n' não era necessário estar no tipo Z.t (módulo Zarith)
  Argumentos:
    n -> número de ordem da expressão
    count -> conta a vezes que a função first_exp é executada
*)
let rec first_exp n count =
  incr count; (* Incrementar variável contador *)
  match n with
  | 0 -> um, count (* Quando 'n' for 0 devolve o resultado 1. O output final é (1,<contador>) *)
  | 1 -> dois, count (* Quando 'n' for 1 devolve o resultado 2. O output final é (2,<contador>) *)
  | _ -> (* Ação que é executada quando 'n' <> 0 e 'n' <> 1 *)
    let inter_exp,_ = first_exp (n-1) (count) in (* Declaração do resultado da expressão recursiva existente na primeira expressão *)
    tres *! inter_exp +! sum_k_n 1 n first_exp count zero, count (* Devolução de resultado na forma (<resutado_da_expressão,<contador>) *)

(*
  Função da segunada expressão (sem momoização) - Verificou-se que o 'n' não era necessário estar no tipo Z.t (módulo Zarith)
  Argumentos:
    n -> número de ordem da expressão
    count -> conta a vezes que a função second_exp é executada
*)
let rec second_exp n count =
  incr count; (* Incrementar variável contador *)
  match n with
  | 0 -> um, count (* Quando 'n' for 0 devolve o resultado 1. O output final é (1,<contador>) *)
  | 1 -> dois, count (* Quando 'n' for 1 devolve o resultado 2. O output final é (2,<contador>) *)
  | _ -> (* Ação que é executada quando 'n' <> 0 e 'n' <> 1 *)
    let (inter_exp1,_), (inter_exp2,_) = second_exp (n-1) (count), second_exp (n-2) (count) in (* Declaração do resultado das expressões recursivas existente na segunda expressão *)
    (seis *! Z.of_int n -! tres) *! inter_exp1 /! Z.of_int (n+1) -! Z.of_int (n-2) *! inter_exp2 /! Z.of_int (n+1), count (* Devolução de resultado na forma (<resutado_da_expressão,<contador>) *)

(*
  Função da segunada expressão (com momoização) - Verificou-se que o 'n' não era necessário estar no tipo Z.t (módulo Zarith)
  Foi selecionada a segunda expressão, pois verificar-se que a execução desta é mais rápida que a primeira expressão
  Argumentos:
    n -> número de ordem da expressão
    h_table -> tabela de hash
*)
let rec memo_apply n h_table =
  if (Hashtbl.mem h_table n) then (Hashtbl.find h_table n) (* Verifica que o resultado de 'n' existe na tabela de hash *)
  else match n with
  | 0 -> Hashtbl.add h_table n um; um (* Quando 'n' for 0 devolve o resultado 1 e adiciona o resultado à tabela de hash. O output final é (1,<contador>) *)
  | 1 -> Hashtbl.add h_table n dois; dois (* Quando 'n' for 1 devolve o resultado 2 e adiciona o resultado à tabela de hash. O output final é (2,<contador>) *)
  | _ -> (* Ação que é executada quando 'n' <> 0 e 'n' <> 1 *)
    let exp = (seis *! Z.of_int n -! tres) *! memo_apply (n-1) h_table /! Z.of_int (n+1) -! Z.of_int (n-2) *! memo_apply (n-2) h_table /! Z.of_int (n+1) in (* Declaração da segunda expressão *)
    Hashtbl.add h_table n exp; exp (* Adiciona o resultado da segunda expressão à tabela de hash *)


exception Invalid_value
let () =
  try
    let a, b = Scanf.scanf "%d %d" (fun first second -> first, second) in
    let hash_table = Hashtbl.create (b+1) in (* Cria uma tabela de hash com b+1 posições pois 'b' inicia no valor 0 *)
    if a<0 || a>20 || b<0 || b>10000 then raise Invalid_value
    else
      let (exp1, first_counter), (exp2, second_counter), fast_exp = first_exp a (ref 0), second_exp a (ref 0), memo_apply b hash_table  in
      Printf.printf "%a %d\n%a %d\n%a\n" (Z.output) exp1 !first_counter (Z.output) exp2 !second_counter (Z.output) fast_exp
  with
  | _ -> raise Invalid_value