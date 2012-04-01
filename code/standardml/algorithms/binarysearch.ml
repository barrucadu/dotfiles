(* Implementation of binary search on a list *)

local
    (* Get the first N elements of a list, removing the rest *)
    fun firstn _ [] = []
      | firstn len (m :: ns) = if len = 0
                               then []
                               else m :: firstn (len - 1) ns;

    (* Divide a list, returning two equal halves *)
    fun divide [] = ([], [])
      | divide [m] = ([], [m])
      | divide l =
        let
            val lena = (length l) div 2;
            val lenb = (length l) - lena;
        in
            (firstn lena l, rev (firstn lenb (rev l)))
        end;

    (* Search for a term *)
    fun binsearch eq gt l t pos =
        let
            val (first, m :: ns) = divide l;
            val p = (length l) div 2 + pos;
        in
            if eq (m, t)
            then p + 1
            else
                if gt (m, t)
                then binsearch eq gt first t 0
                else binsearch eq gt (m :: ns) t p
        end;
in
    (* Search for a term *)
    fun binarysearch eq gt l t = binsearch eq gt l t 0;
end;
