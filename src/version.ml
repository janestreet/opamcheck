(* version.ml
   Copyright 2017 Inria
   author: Damien Doligez
*)

let name_version (p, v) = Printf.sprintf "%s.%s" p v

let split_name_version s =
  try
    let i = String.index s '.' in
    (String.sub s 0 i, Some (String.sub s (i+1) (String.length s - i - 1)))
  with Not_found -> (s, None)

let get_number s i j =
  if i = j then 0 else int_of_string (String.sub s i (j-i))

let split v =
  let rec nondigit accu i j =
    if j = String.length v then
      if i = j then
        List.rev accu
      else
        digit accu (String.sub v i (j-i)) j j
    else begin
      match v.[j] with
      | '0' .. '9' -> digit accu (String.sub v i (j-i)) j j
      | _ -> nondigit accu i (j+1)
    end
  and digit accu s i j =
    if j = String.length v then
      nondigit ((s, get_number v i j) :: accu) j j
    else begin
      match v.[j] with
      | '0' .. '9' -> digit accu s i (j+1)
      | _ -> nondigit ((s, get_number v i j) :: accu) j j
    end
  in
  nondigit [] 0 0

let compare_block s1 s2 n1 n2 k a1 a2 =
  let comp_cont a b = if a = b then k a1 a2 else Pervasives.compare a b in
  if s1 = s2 then comp_cont n1 n2
  else begin
    let s1 = s1 ^ "0" and s2 = s2 ^ "0" in
    let rec loop i =
      match s1.[i], s2.[i] with
      | c1, c2 when c1 = c2 -> loop (i+1)
      | '~', _ -> -1
      | _, '~' -> 1
      | '0', _ -> -1
      | _, '0' -> 1
      | ('A' .. 'Z' | 'a' .. 'z'), ('A' .. 'Z' | 'a' .. 'z') ->
         comp_cont s1.[i] s2.[i]
      | ('A' .. 'Z' | 'a' .. 'z'), _ -> -1
      | _, ('A' .. 'Z' | 'a' .. 'z') -> 1
      | _ -> comp_cont s1.[i] s2.[i]
    in loop 0
  end

let compare v1 v2 =
  let l1 = split v1 in
  let l2 = split v2 in
  let rec loop l1 l2 =
    match l1, l2 with
    | [], [] -> 0
    | (s1, n1) :: t1, [] -> compare_block s1 "" n1 0 loop t1 []
    | [], (s2, n2) :: t2 -> compare_block "" s2 0 n2 loop [] t2
    | (s1, n1) :: t1, (s2, n2) :: t2 ->
       compare_block s1 s2 n1 n2 loop t1 t2
  in
  loop l1 l2
