program die;

Var
   diestr	    : String;
   a, i, err	    : Integer;
   looping	    : Boolean;
   rolls, size, sum : Integer;
   mean		    : Real;

begin
   randomize;

   write('Enter the die string (eg: 1d6): ');
   readln(diestr);

   a       := 1;
   i       := 1;
   rolls   := -1;
   size    := -1;
   looping := true;

   while looping do
   begin
      i := i + 1;
      if rolls = -1 then
      begin
	 if diestr[i] = 'd' then
	    begin
	       val(copy(diestr, a, i - a), rolls, err);
	       if err <> 0 then
	       begin
		  writeln('An error occurred at ', err, ' (rolls)');
		  exit
	       end
	       else
	       begin
		  val(copy(diestr, i + 1, Length(diestr)), size, err);
		  if err <> 0 then
		  begin
		     writeln('An error occurred at ', err, ' (size)');
		     exit
		  end;
		  looping := false;
	       end
	    end
      end
   end;

   sum := 0;

   for i := 1 to rolls do
   begin
      a := random(size) + 1;
      writeln('Roll ', i, ' = ', a);
      sum := sum + a;
   end;

   mean := sum / rolls;
   writeln('Sum:  ', sum);
   writeln('Mean: ', mean:2:2);
end.