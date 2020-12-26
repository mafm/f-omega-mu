let to_uchar_array str =
  let buffer = Array.make (Stdlib.String.length str) Uchar.min in
  let n =
    str
    |> Uutf.String.fold_utf_8
         (fun i _ -> function
           | `Uchar c ->
             buffer.(i) <- c;
             i + 1
           | `Malformed _ -> Exn.failwithf "Malformed utf-8 at char index %d" i)
         0
  in
  if n <> Array.length buffer then
    Array.sub buffer 0 n
  else
    buffer
