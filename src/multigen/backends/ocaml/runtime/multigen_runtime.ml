(* MultiGen OCaml Runtime - Python-like operations using OCaml standard library *)

open Printf

(* String operations module *)
module StrOps = struct
  let upper s = String.uppercase_ascii s

  let lower s = String.lowercase_ascii s

  let strip s =
    let len = String.length s in
    let rec left i =
      if i >= len then i
      else match s.[i] with
        | ' ' | '\t' | '\n' | '\r' -> left (i + 1)
        | _ -> i
    in
    let rec right i =
      if i < 0 then i
      else match s.[i] with
        | ' ' | '\t' | '\n' | '\r' -> right (i - 1)
        | _ -> i
    in
    let start = left 0 in
    let stop = right (len - 1) in
    if start > stop then ""
    else String.sub s start (stop - start + 1)

  let find s sub =
    try
      let pos = String.index s (String.get sub 0) in
      let rec check_match pos =
        if pos + String.length sub > String.length s then -1
        else if String.sub s pos (String.length sub) = sub then pos
        else try
          let next_pos = String.index_from s (pos + 1) (String.get sub 0) in
          check_match next_pos
        with Not_found -> -1
      in
      check_match pos
    with Not_found -> -1

  let replace s old_str new_str =
    let old_len = String.length old_str in
    let new_len = String.length new_str in
    let s_len = String.length s in

    let rec find_all acc pos =
      if pos >= s_len then List.rev acc
      else
        let found = find (String.sub s pos (s_len - pos)) old_str in
        if found = -1 then List.rev acc
        else find_all ((pos + found) :: acc) (pos + found + old_len)
    in

    let positions = find_all [] 0 in
    if positions = [] then s
    else
      let result_len = s_len + List.length positions * (new_len - old_len) in
      let result = Bytes.create result_len in
      let rec replace_at positions src_pos dst_pos =
        match positions with
        | [] ->
          Bytes.blit_string s src_pos result dst_pos (s_len - src_pos)
        | pos :: rest ->
          let before_len = pos - src_pos in
          Bytes.blit_string s src_pos result dst_pos before_len;
          Bytes.blit_string new_str 0 result (dst_pos + before_len) new_len;
          replace_at rest (pos + old_len) (dst_pos + before_len + new_len)
      in
      replace_at positions 0 0;
      Bytes.to_string result

  let split s delimiter =
    let delimiter_len = String.length delimiter in
    let s_len = String.length s in

    let rec find_splits acc start =
      if start >= s_len then List.rev acc
      else
        let found = find (String.sub s start (s_len - start)) delimiter in
        if found = -1 then List.rev (String.sub s start (s_len - start) :: acc)
        else
          let actual_pos = start + found in
          let part = String.sub s start found in
          find_splits (part :: acc) (actual_pos + delimiter_len)
    in
    find_splits [] 0
end

(* Built-in functions module *)
module Builtins = struct
  let abs_int x = abs x
  let abs_float x = abs_float x

  let bool_of_int x = x <> 0
  let bool_of_string s =
    match String.lowercase_ascii s with
    | "true" | "1" | "yes" -> true
    | _ -> false

  let len_string s = String.length s
  let len_list lst = List.length lst
  let len_array arr = Array.length arr

  let min_int x y = min x y
  let min_float x y = min x y

  let max_int x y = max x y
  let max_float x y = max x y

  let sum_int_list lst = List.fold_left (+) 0 lst
  let sum_float_list lst = List.fold_left (+.) 0.0 lst

  let any_bool_list lst = List.exists (fun x -> x) lst
  let all_bool_list lst = List.for_all (fun x -> x) lst

  (* Set constructor - empty set as empty list *)
  let set () = []
end

(* Range operations *)
module Range = struct
  type range = {
    start : int;
    stop : int;
    step : int;
  }

  let create_range stop = { start = 0; stop; step = 1 }
  let create_range2 start stop = { start; stop; step = 1 }
  let create_range3 start stop step = { start; stop; step }

  let to_list range =
    let rec generate acc current =
      if (range.step > 0 && current >= range.stop) ||
         (range.step < 0 && current <= range.stop) then
        List.rev acc
      else
        generate (current :: acc) (current + range.step)
    in
    generate [] range.start
end

(* Comprehensions module *)
module Comprehensions = struct
  (* List comprehensions *)
  let list_comprehension lst transform =
    List.map transform lst

  let list_comprehension_with_filter lst predicate transform =
    List.map transform (List.filter predicate lst)

  (* Dictionary comprehensions using association lists *)
  let dict_comprehension lst key_func value_func =
    List.map (fun x -> (key_func x, value_func x)) lst

  let dict_comprehension_with_filter lst predicate key_func value_func =
    List.map (fun x -> (key_func x, value_func x)) (List.filter predicate lst)

  (* Set comprehensions using lists (OCaml doesn't have built-in sets in this simple form) *)
  let set_comprehension lst transform =
    let rec remove_duplicates = function
      | [] -> []
      | x :: xs -> x :: remove_duplicates (List.filter (fun y -> y <> x) xs)
    in
    remove_duplicates (List.map transform lst)

  let set_comprehension_with_filter lst predicate transform =
    let rec remove_duplicates = function
      | [] -> []
      | x :: xs -> x :: remove_duplicates (List.filter (fun y -> y <> x) xs)
    in
    remove_duplicates (List.map transform (List.filter predicate lst))
end

(* Type conversion utilities *)
module Conversions = struct
  let string_of_bool = function
    | true -> "true"
    | false -> "false"

  let string_of_int_list lst =
    "[" ^ String.concat "; " (List.map string_of_int lst) ^ "]"

  let string_of_string_list lst =
    "[" ^ String.concat "; " (List.map (fun s -> "\"" ^ s ^ "\"") lst) ^ "]"

  let print_value v = print_endline v
  let print_int i = print_endline (string_of_int i)
  let print_float f = print_endline (string_of_float f)
  let print_bool b = print_endline (string_of_bool b)
end

(* Export commonly used functions *)
let range = Range.create_range
let range2 = Range.create_range2
let range3 = Range.create_range3
let range_list r = Range.to_list r

let upper = StrOps.upper
let lower = StrOps.lower
let strip = StrOps.strip
let find = StrOps.find
let replace = StrOps.replace
let split = StrOps.split

let abs' = Builtins.abs_int
let bool' = Builtins.bool_of_int
let len' = Builtins.len_list
let len_array = Builtins.len_array
let min' = Builtins.min_int
let max' = Builtins.max_int
let sum' = Builtins.sum_int_list
let any' = Builtins.any_bool_list
let all' = Builtins.all_bool_list
let set = Builtins.set

let list_comprehension = Comprehensions.list_comprehension
let list_comprehension_with_filter = Comprehensions.list_comprehension_with_filter
let dict_comprehension = Comprehensions.dict_comprehension
let dict_comprehension_with_filter = Comprehensions.dict_comprehension_with_filter
let set_comprehension = Comprehensions.set_comprehension
let set_comprehension_with_filter = Comprehensions.set_comprehension_with_filter

(* Type-specific conversion functions *)
let to_string_int = string_of_int
let to_string_float = string_of_float
let to_string_bool = Conversions.string_of_bool
let to_string x = x  (* For strings, identity function *)

let print_value = Conversions.print_value

(* Array operations for mutable sequences *)
module ArrayOps = struct
  (* Update array element *)
  let update_array arr idx value =
    arr.(idx) <- value;
    arr

  (* Update association list (dict) *)
  let update_assoc_list lst key value =
    let rec update acc = function
      | [] -> List.rev ((key, value) :: acc)
      | (k, v) :: rest ->
        if k = key then
          List.rev_append acc ((key, value) :: rest)
        else
          update ((k, v) :: acc) rest
    in
    update [] lst

  (* Convert list to array *)
  let list_to_array lst = Array.of_list lst

  (* Convert array to list *)
  let array_to_list arr = Array.to_list arr

  (* Append to list (returns new list) *)
  let list_append lst value =
    lst @ [value]

  (* Append to array (creates new array with increased size) *)
  let array_append arr value =
    let len = Array.length arr in
    let new_arr = Array.make (len + 1) value in
    Array.blit arr 0 new_arr 0 len;
    new_arr
end

let update_array = ArrayOps.update_array
let update_assoc_list = ArrayOps.update_assoc_list
let list_to_array = ArrayOps.list_to_array
let array_to_list = ArrayOps.array_to_list
let list_append = ArrayOps.list_append
let array_append = ArrayOps.array_append