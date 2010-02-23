program fib;

Var
   fib1, fib2, temp, i, n : longint;

begin
   fib1 := 1;
   fib2 := 1;

   write('Enter the number of Fibonacci numbers to calculate: ');
   read(n);
   
   for i := 1 to n do
   begin
      write(fib1, ' ');
      temp := fib2;
      fib2 := fib1 + fib2;
      fib1 := temp;
   end;
end.