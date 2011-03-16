(* Implementation of bubblesort using higher-order functions on polymorphic lists *)

local
    (* Go down the list, swapping out-of-place elements *)
    fun bubblepass _ [] = []
      | bubblepass _ [m] = [m]
      | bubblepass f (m :: n :: ns) = if f (m, n)
                                      then m :: bubblepass f (n :: ns)
                                      else n :: bubblepass f (m :: ns);

in
    (* Sort the list *)
    fun bubblesort f l = if (l = bubblepass f l)
                         then l
                         else bubblesort f (bubblepass f l);
end;
