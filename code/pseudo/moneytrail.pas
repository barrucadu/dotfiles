program MoneyTrail;

Const
   pupilsnum = 5; { The number of pupils; defined as a constant as it's not likely to change often }

Var
   pupils : array [1..pupilsnum] of String; { Pupils and money stored as arrays, using pupilsnum.
					    i is a counter, err stores any error value,
					    t stores the total money, and temp stores the temporary string storing money }
   money  : array [1..pupilsnum] of Real;
   i, err : Integer;
   temp	  : String;
   t	  : Real;

begin
   t := 0; { Initialise the t varuable }
   
   for i := 1 to pupilsnum do { 
   begin
      err := 0; { Initialise the err variable }
      
      write('Enter the name of the pupil: ');
      readln(pupils[i]);
      
      write('Enter the money collected:   ');
      readln(temp);
      val(temp, money[i], err);

      while err <> 0 do
      begin
	 write('Please enter a numerical amount: ');
	 readln(temp);
	 val(temp, money[i], err);
      end;

      t := t + money[i];
   end;
   
   writeln();
   
   for i := 1 to pupilsnum do
   begin
      writeln('Pupil: ', pupils[i]);
      writeln('       £', money[i]:2:2);
   end;

   writeln();
   writeln('Total: £', t:2:2);
end.
  