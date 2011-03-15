(* Implementation of some list functions. Yay, pattern matching :D *)

(* Count the length of a list *)
fun llen l = 
    let
        fun lleniter ([], c) = c
          | lleniter (m :: ns, c) = lleniter (ns, c + 1)
    in lleniter (l, 0)
    end;

(* Sum of a list *)
fun list_sum [m] = m
  | list_sum (m :: ns) = m + list_sum ns;

(* Product of a list *)
fun list_prod [] = 1
  | list_prod (m :: ns) = m * list_prod ns;

(* Maximum of a list *)
fun list_max [m] = m
  | list_max (m :: n :: ns) = if m > n
                              then m
                              else list_max (n :: ns);

(* Minimum of a list *)
fun list_min [m] = m
  | list_min (m :: n :: ns) = if m < n
                              then m
                              else list_min (n :: ns);

(* Nth item of a list *)
fun nth (m :: _) 0  = m
  | nth (_ :: ns) n = nth ns n - 1;

(* Average of a list *)
fun list_avg l  = list_sum l div llen l;

(* Generate a list from a to b *)
fun upto a b = if a = b
               then [b]
               else a :: upto (a + 1) b;

(* Factorial *)
fun fact n = list_prod (upto 1 n);

(* Fibonacci *)
local
    fun fib n =
        let fun fibiter a _ 0 = a
              | fibiter a b N = fibiter b (a + b) (N - 1)
        in fibiter 0 1 n
        end;

in fun fiblist n = 
        let fun fibliter l N = if N = n
                               then l
                               else fibliter (l @ [fib N]) (N + 1)
        in fibliter [] 0
        end;
end;

(* Pair the elements in two equal-length lists *)
fun list_pair [] [] = []
  | list_pair (m :: ns) (M :: NS) = (m, M) :: list_pair ns NS;

(* Unpair the elements, producing a pair of lists *)
fun list_unpair [] = ([], [])
  | list_unpair ((a, b) :: pairs) = 
    let val (x, y) = list_unpair pairs
    in (a :: x, b :: y)
    end;

(* Take the cartesian product of two lists *)
fun list_cart _ [] = []
  | list_cart [] _ = []
  | list_cart [a] (m :: ns) = (a, m) :: list_cart [a] ns
  | list_cart (m :: ns) l = (list_cart [m] l) @ (list_cart ns l);
