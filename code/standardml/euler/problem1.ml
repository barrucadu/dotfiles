(* Project Euler Problem 1
 * 
 * If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
 * 
 * Find the sum of all the multiples of 3 or 5 below 1000.
 *)
open List;
use "/home/barrucadu/code/standardml/lists.ml";

local
    (* Check a value has at least one of the factors in a list *)
    fun hasfactors _ [] = false
      | hasfactors a (m :: ns) = a mod m = 0 orelse hasfactors a ns;
        
    (* 'Remove' all values in a list without given factors *)
    fun reduce l factors =
        let fun chkf x = if hasfactors x factors then x else 0
        in map chkf l
        end;  
in    
    (* Solve Euler 1 *)
    fun euler1 n = list_sum (reduce (upto 1 (n - 1)) [3, 5]);
end;
