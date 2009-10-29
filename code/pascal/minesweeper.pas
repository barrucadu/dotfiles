program minesweeper;

const
   width  = 9;
   height = 9;

var
   field, visible : array [0..height, 0..width] of Boolean; { field: T = mine, F = clean. visible: T = visible, F = invisible }
   turns, x, y	  : Integer;
   playing	  : Boolean;
   result	  : Integer;

Procedure initboard ();
var
   mines, x, y : Integer;
begin
   mines := random(42) + 33; { 33 to 75 mines }
   
   while mines > 0 do
   begin
      x := random(width) + 1;
      y := random(height) + 1;
      
      if field[y, x] = false then
      begin
	 field[y, x] := true;
	 mines := mines - 1;
      end;
   end;
end; { initboard }

Procedure printboard ();
var
   count, x, y : Integer;
begin
   write('   ');
   for count := 0 to width do
      write(count:2);
   writeln();
   
   write('   ');
   for count := 0 to width + 1 do
      write('__');
   
   writeln();
   
   for y := 0 to height do
   begin
      write(height - y:2, ' |');
      for x := 0 to width do
      begin
	 if visible[y, x] then
	    if field[y, x] then
	       write('M')
	    else
	    begin
	       count := 0;
	       
	       if field[y, x - 1]     then count := count + 1;
	       if field[y, x + 1]     then count := count + 1;
	       if field[y - 1, x]     then count := count + 1;
	       if field[y + 1, x]     then count := count + 1;
	       if field[y - 1, x - 1] then count := count + 1;
	       if field[y + 1, x + 1] then count := count + 1;
	       if field[y - 1, x + 1] then count := count + 1;
	       if field[y + 1, x - 1] then count := count + 1;
	       
	       if count <> 0 then
		  write(count:2)
	       else
		  write('  ');
	    end
         else
	    write('--');
      end;
      writeln('|');
   end;

   write('   |');
   for count := 0 to width do
      write('__');
   
   writeln('|');
end; { printboard }

Function makemove (x, y : Integer) : Integer;
begin
   y := height - y; { user perspective is from bottom left, we work from top left. }
   if visible[y, x] then
      if field[y, x] then
	 makemove := 2 { hit a mine }
      else
      begin
	 
	 makemove := 0 { good move}
      end
   else
      makemove := 1; { already hit that square }
end; { makemove }

begin
   randomize();
   initboard();
   
   turns := 0;
   playing := True;
   
   while playing do
   begin
      writeln('Minesweeper (turn ', turns, '):');
      printboard();

      write('Enter the coordinates of your move: ');
      read(x);
      read(y);

      result := makemove(x, y);

      if result = 0 then { Successful }
	 turns := turns + 1
      else
	 if result = 2 then { Hit a mine (1 = already clicked that square) }
	 begin
	    playing := false;
	    endgame();
	 end;
   end;
end.