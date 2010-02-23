program Euler1;

Var
   n, i : Integer;

begin
   n := 0;
   
   for i := 1 to 999 do
   begin
      if (i mod 3 = 0) or (i mod 5 = 0) then
	 n := n + i;
   end;

   writeln('Sum of the multiples of 3 or 5 below 1000 = ', n)
end.