type 'a binary_tree =
  | Empty
  | Node of 'a binary_tree * 'a * 'a binary_tree * int

let compare value root =
  if value = root then 0 (* O valor é igual à raiz *)
  else if value < root then -1 (* Escolher a esquerda *)
  else 1 (* Escolher a direita *)

let height = function
  | Empty -> 0 (* Árvore binária não definida *)
  | Node (_, _, _, h) -> h (* Devolução da altura *)

let node l v r = Node (l, v, r, 1 + max (height l) (height r))

let balance l v r =
  let height_l = height l in
  (* Altura do filho esquerdo *)
  let height_r = height r in
  (* Altura do filho direito *)
  if height_l > height_r + 1 then
    (* Corrigir lado esquerdo *)
    match l with
    | Node (ll, lv, lr, _) when height ll >= height lr ->
        node ll lv (node lr v r) (* Rotação para a esquerda *)
    | Node (ll, lv, Node (lrl, lrv, lrr, _), _) ->
        node (node ll lv lrl) lrv (node lrr v r) (* Dupla rotação *)
    | _ -> assert false
  else if height_r > height_l + 1 then
    (* Corrigir lado direito *)
    match r with
    | Node (rl, rv, rr, _) when height rr >= height rl ->
        node (node l v rl) rv rr (* Rotação para a direita *)
    | Node (Node (rll, rlv, rlr, _), rv, rr, _) ->
        node (node l v rll) rlv (node rlr rv rr) (* Dupla rotação *)
    | _ -> assert false
  else node l v r (* Não é necessario rotação *)

let rec add new_val = function
  | Empty ->
      Node (Empty, new_val, Empty, 1)
      (* Se node estiver vazio, adiciona valor *)
  | Node (l, v, r, _) as t ->
      let c = compare new_val v in
      (* Escolha do 'caminho' a seguir *)
      if c = 0 then t (* Devolve o node atual *)
      else if c < 0 then
        balance (add new_val l) v r (* Adiciona node à esquerda *)
      else balance l v (add new_val r)
(* Adiciona node à direita *)

let rec mutation v1 v2 v1_found v2_found mutation_val = function
  | Empty -> (-1, v1_found, v2_found) (* Mutação não existe *)
  | Node (l, v, r, _) ->
      let v1_found_l, v2_found_l =
        ((if v1 = v then true else v1_found), if v2 = v then true else v2_found)
      in
      (* Procura se v1/v2 coincidem com a raiz *)
      let mutation_val_l =
        if mutation_val = -1 && ((v1 >= v && v2 <= v) || (v1 <= v && v2 >= v))
        then v
        else mutation_val
      in
      (* Suposta mutação encontrada *)
      if v1_found_l && v2_found_l then (mutation_val_l, v1_found, v2_found)
        (* A suposta mutação encontrada é verdadeira *)
      else if v1 > v && v2 > v then
        mutation v1 v2 v1_found_l v2_found_l mutation_val_l
          r (* Pesquisar v1 e v2 à direita *)
      else if v1 < v && v2 < v then
        mutation v1 v2 v1_found_l v2_found_l mutation_val_l
          l (* Pesquisar v1 e v2 à esquerda *)
      else if
        ((v1 >= v && v2 <= v) || (v1 <= v && v2 >= v))
        && (v1_found_l || v2_found_l)
      then
        (* Caso um dos valores (v1/v2) já tenha sido encontrado *)
        if v1_found_l then
          if v2 > v then
            mutation v1 v2 v1_found_l v2_found_l mutation_val_l
              r (* Pesquisar v2 à direita *)
          else
            mutation v1 v2 v1_found_l v2_found_l mutation_val_l
              l (* Pesquisar v2 à esquerda *)
        else if v1 > v then
          mutation v1 v2 v1_found_l v2_found_l mutation_val_l
            r (* Pesquisar v1 à direita *)
        else
          mutation v1 v2 v1_found_l v2_found_l mutation_val_l
            l (* Pesquisar v1 à esquerda *)
      else
        (* Verifica se v1/v2 existe na árvore binária caso v1 >= v >= v2 || v1 <= v <= v2 *)
        let _, v1_found_l, v2_found_l =
          mutation v1 v2 v1_found_l v2_found_l mutation_val_l r
        in
        (* Pesquisar v1/v2 à direita *)
        mutation v1 v2 v1_found_l v2_found_l mutation_val_l
          l (* Pesquisar v1/v2 à esquerda *)

let () =
  let tree_num = read_int () in
  (* Número de árvores binárias a criar *)
  let hash_tree = Hashtbl.create tree_num in
  (* Criação de hash table para armazenar as árvores binárias criadas *)
  if tree_num <= 5000 && tree_num > 0 then
    for tree = 0 to tree_num - 1 do
      let num_element = read_int () in
      (* Número de elementos da árvore binária *)
      if num_element <= 10000 && num_element > 0 then
        for _ = 0 to num_element - 1 do
          let value = read_int () in
          (* Valor a guardar na árvore binária *)
          let curr_tree =
            if Hashtbl.mem hash_tree tree then Hashtbl.find hash_tree tree
              (* Árvore binária que se encontra na tabela de hash *)
            else Empty
            (* Cria uma árvore vazia *)
          in
          Hashtbl.replace hash_tree tree (add value curr_tree)
          (* Atualiza a árvore binária na tabela de hash *)
        done
      else raise (invalid_arg "Invalid_value")
    done
  else raise (invalid_arg "Invalid_value");
  Scanf.scanf "%d %d" (fun v1 v2 ->
      let count_fail = ref 0 in
      (* Verificação de falha de mutação *)
      for tree = 0 to tree_num - 1 do
        let bin_tree = Hashtbl.find hash_tree tree in
        (* Árvore binária atual *)
        let tree_mutation, _, _ = mutation v1 v2 false false (-1) bin_tree in
        (* Obtém a árvore binária que está na tabela de hash *)
        if tree_mutation = -1 then incr count_fail (* Mutação não encontrada *)
        else
          Printf.printf "%d\n"
            tree_mutation (* Print da mutação que foi encontrada *)
      done;
      if !count_fail = tree_num then
        print_endline "NO" (* Não existe mutação em todas a árvores *))
