Program Hanoi;

{ A Towers of Hanoi solver:
  * Generates a tower of random height between 3 and 10 blocks
  * Saves solution to "soln.txt"
  * Prints info to STDOUT }

Const
   SOLNFILE  = 'soln.txt';

Type
   Tower = Array [0..10] of Integer;

Var 
   disks : Integer;
   moves : Integer;
   ltr	 : Boolean;

Function pow(base, exponent : Integer) : Integer; { Raise base to power exponent }
begin
   pow := Round(exp(exponent * ln(base)));
end; { pow }

Procedure printinfo(); { Print information about the puzzle generated }
begin
   writeln('Towers of Hanoi Solver.');
   writeln('    ', disks, ' disks.');
   writeln('    ', moves, ' moves required.');

   if ltr then
      writeln('    L->R move direction.')
   else
      writeln('    R->L move direction.')
end; { printinfo }

Procedure inittower(Var tow : Tower; ndisks : Integer); { Initialises the tower variables }
var
   count : Integer;
begin
   for count := 0 to 10 do
   begin
      if ndisks - count >= 0 then
	 tow[count] := ndisks - count
      else
	 tow[count] := 0
   end;
end; { inittower }

Procedure makemove(Var towa, towb : Tower); { Move the top disk from one tower to another }
var
   i, j	: Integer;
begin
   { Find the indexes of the last nonzero value in the towers }
   i := 0;
   j := 0;
   
   while towa[i + 1] <> 0 do
      i := i + 1;

   while towb[j + 1] <> 0 do
      j := j + 1;

   towb[j + 1] := towa[i];
   towa[i] := 0;
end; { makemove }

Procedure getmoveorder(towa, towb, diska, diskb : Integer; Var towerbegin, towerend : Integer); { Get the move between two towers }
begin
   if (diska < diskb) or (diskb = 0) then
   begin
      towerbegin := towa;
      towerend   := towb;
   end
   else
   begin
      towerbegin := towb;
      towerend   := towa;
   end;
end; { getmoveorder }

Procedure findmove(Var towa, towb, towc	: Tower; Var towerbegin, towerend : Integer; smallest : Boolean); { Find the next move that either does or does not involve moving the smallest disk }
var
   i, j, k : Integer;
   dir	   : Integer;
begin
   { Find the indexes of the last nonzero value in the towers }
   i := 0;
   j := 0;
   k := 0;
   
   while towa[i + 1] <> 0 do
      i := i + 1;

   while towb[j + 1] <> 0 do
      j := j + 1;

   while towc[k + 1] <> 0 do
      k := k + 1;

   if smallest then
   begin
      if ltr then
	 dir := 1
      else
	 dir := -1;

      if towa[i] = 1 then
	 towerbegin := 0
      else
	 if towb[j] = 1 then
	    towerbegin := 1
	 else
	    towerbegin := 2;
      
      towerend := towerbegin + dir;

      if towerend = 3 then
	 towerend := 0
      else
	 if towerend = -1 then
	    towerend := 2;
   end
   else
   begin
      if towa[i] = 1 then
	 getmoveorder(1, 2, towb[j], towc[k], towerbegin, towerend)
      else
	 if towb[j] = 1 then
	    getmoveorder(0, 2, towa[i], towc[k], towerbegin, towerend)
	 else
	    getmoveorder(0, 1, towa[i], towb[j], towerbegin, towerend);
   end;
end; { findmove }

Procedure solve(); { Solve the puzzle }
var
   fh	      : TextFile;
   move	      : Integer;
   towa	      : Tower;
   towb	      : Tower;
   towc	      : Tower;
   towerbegin : Integer;
   towerend   : Integer;
begin
   { Open solution file for writing }
   Assign(fh, SOLNFILE);
   Rewrite(fh);

   move := 0;
   writeln(fh, move:3, ': Build a tower of ', disks, ' disks.');

   { Keep track of which tower has what }
   inittower(towa, disks);
   inittower(towb, 0);
   inittower(towc, 0);
   
   for move := 1 to moves do
   begin
      if (move - 1) mod 2 = 0 then
	 { Move smallest disk }
	 findmove(towa, towb, towc, towerbegin, towerend, True)
      else
	 { Move other disk }
	 findmove(towa, towb, towc, towerbegin, towerend, False);

      if towerbegin = 0 then
      begin
	 if towerend = 1 then
	    makemove(towa, towb)
	 else
	    makemove(towa, towc)
      end
      else
	 if towerbegin = 1 then
	 begin
	    if towerend = 0 then
	       makemove(towb, towa)
	    else
	       makemove(towb, towc)
	 end
	 else
	 begin
	    if towerend = 1 then
	       makemove(towc, towb)
	    else
	       makemove(towc, towa)
	 end;

      writeln(fh, move:3, ': Move disk from tower ', towerbegin, ' to tower ', towerend, '.');
   end;
   
   close(fh);
end; { solve }

begin
   { Do some set up }
   Randomize;
   disks := 3 + Random(8); { Generate random number between 3 and 10 }
   
   moves := pow(2, disks) - 1; { Figure out the number of moves required, and the move direction. }
   ltr := (disks mod 2 = 0);

   { Information about the puzzle generated }
   printinfo;
   
   { Now solve }
   solve;
end.