(** @autor: Joel Vaz (a48906) *)

(** Definition of binary tree *)
type 'a binary_tree =
  | Empty
  | Node of 'a binary_tree * 'a * 'a binary_tree * int

(** 
    Binary tree height
    @param tree : binary tree
    @return Height of the binary tree
*)
let height = function
  | Empty -> 0 (*Binary tree not defined *)
  | Node (_, _, _, h) -> h (* Binary tree height *)

(**
    Binary tree node
    @param l : left node
    @param v : value of the node
    @param r : right node
    @return Binary tree node
*)
let node l v r = Node (l, v, r, 1 + max (height l) (height r))

(**
    Binary tree balance
    @param l : left node
    @param v : value of the node
    @param r : right node
    @return Balanced binary tree
*)
let balance l v r =
  (* Height of the left child *)
  let height_l = height l in
  (* Height of the right child *)
  let height_r = height r in
  if height_l > height_r + 1 then
    (* Correct the left side of the binary tree *)
    match l with
    | Node (ll, lv, lr, _) when height ll >= height lr ->
        (* Rotate to the left *)
        node ll lv (node lr v r)
    | Node (ll, lv, Node (lrl, lrv, lrr, _), _) ->
        (* Double rotation *)
        node (node ll lv lrl) lrv (node lrr v r)
    | _ -> assert false
  else if height_r > height_l + 1 then
    (* Correct the right side of the binary tree *)
    match r with
    | Node (rl, rv, rr, _) when height rr >= height rl ->
        (* Rotate to the right *)
        node (node l v rl) rv rr
    | Node (Node (rll, rlv, rlr, _), rv, rr, _) ->
        (* Double rotation *)
        node (node l v rll) rlv (node rlr rv rr)
    | _ -> assert false (* Binary tree is balanced *)
  else node l v r

(** 
    Add value to binary tree
    @param new_val : value to add
    @param tree : binary tree
    @return Binary tree with new value added
*)
let rec add new_val = function
  (* If empty, add value *)
  | Empty -> Node (Empty, new_val, Empty, 1)
  (* If not empty, add value to the left or right of the binary tree *)
  | Node (l, v, r, _) as t ->
      (* Choose the path to follow *)
      let c = compare new_val v in
      if c = 0 then t (* Return the current node *)
      else if c < 0 then
        (* Add value to the left of the binary tree *)
        balance (add new_val l) v r
      else
        (* Add value to the right of the binary tree *)
        balance l v (add new_val r)

(** 
    Find mutation on binary tree
    @param v1 : value 1
    @param v2 : value 2
    @param v1_found : value 1 found
    @param v2_found : value 2 found
    @param mutation_val : mutation value
    @param tree : binary tree
    @return Mutation value
*)
let rec mutation v1 v2 v1_found v2_found mutation_val = function
  | Empty -> (-1, v1_found, v2_found) (* Mutation not found *)
  | Node (l, v, r, _) ->
      (* Search if v1/v2 match the root *)
      let v1_found_l, v2_found_l =
        ((if v1 = v then true else v1_found), if v2 = v then true else v2_found)
      in
      (* Supposed mutation found *)
      let mutation_val_l =
        if mutation_val = -1 && ((v1 >= v && v2 <= v) || (v1 <= v && v2 >= v))
        then v
        else mutation_val
      in
      (* The supposed mutation found is true *)
      if v1_found_l && v2_found_l then (mutation_val_l, v1_found, v2_found)
      else if v1 > v && v2 > v then
        (* Search v1 and v2 to the right *)
        mutation v1 v2 v1_found_l v2_found_l mutation_val_l r
      else if v1 < v && v2 < v then
        (* Search v1 and v2 to the left *)
        mutation v1 v2 v1_found_l v2_found_l mutation_val_l l
      else if
        ((v1 >= v && v2 <= v) || (v1 <= v && v2 >= v))
        && (v1_found_l || v2_found_l)
      then
        (* If v1 or v2 is found *)
        if v1_found_l then
          if v2 > v then
            (* Search v2 to the right *)
            mutation v1 v2 v1_found_l v2_found_l mutation_val_l r
          else
            (* Search v2 to the left *)
            mutation v1 v2 v1_found_l v2_found_l mutation_val_l l
        else if v1 > v then
          (* Search v1 to the right *)
          mutation v1 v2 v1_found_l v2_found_l mutation_val_l r
        else
          (* Search v1 to the left *)
          mutation v1 v2 v1_found_l v2_found_l mutation_val_l l
      else
        (* Verifies if v1/v2 exists in the binary tree if v1 >= v >= v2 || v1 <= v <= v2 *)
        (* Search v1/v2 to the right *)
        let _, v1_found_l, v2_found_l =
          mutation v1 v2 v1_found_l v2_found_l mutation_val_l r
        in
        (* Search v1/v2 to the left *)
        mutation v1 v2 v1_found_l v2_found_l mutation_val_l l

(** Main function *)
let () =
  (* Number of binary trees to create *)
  let tree_num = read_int () in
  (* Hash table creation to store the binary trees *)
  let hash_tree = Hashtbl.create tree_num in
  if tree_num <= 5000 && tree_num > 0 then
    for tree = 0 to tree_num - 1 do
      (* Number of elements of the binary tree *)
      let num_element = read_int () in
      if num_element <= 10000 && num_element > 0 then
        for _ = 0 to num_element - 1 do
          (* Value to add to the binary tree *)
          let value = read_int () in
          let curr_tree =
            if Hashtbl.mem hash_tree tree then
              (* Binary tree that is in the hash table *)
              Hashtbl.find hash_tree tree
            else Empty (* Creates a new binary tree *)
          in
          (* Updates the binary tree in the hash table *)
          Hashtbl.replace hash_tree tree (add value curr_tree)
        done
      else raise (invalid_arg "Invalid_value")
    done
  else raise (invalid_arg "Invalid_value");
  Scanf.scanf "%d %d" (fun v1 v2 ->
      (* Verification of mutation failure *)
      let count_fail = ref 0 in
      for tree = 0 to tree_num - 1 do
        (* Binary tree that is in the hash table *)
        let bin_tree = Hashtbl.find hash_tree tree in
        let tree_mutation, _, _ = mutation v1 v2 false false (-1) bin_tree in
        if tree_mutation = -1 then incr count_fail (* Mutation not found *)
        else (* Print found mutation *)
          Printf.printf "%d\n" tree_mutation
      done;
      if !count_fail = tree_num then
        (* Mutation not found in all binary trees *)
        print_endline "NO")
