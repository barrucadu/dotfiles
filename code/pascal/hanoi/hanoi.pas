Program HanoiSolver;

{ A Towers of Hanoi solver:
  * Generates a tower of random height between 3 and 10 blocks
  * Saves solution to "soln.txt"
  * Prints info to STDOUT }

{ Include maths lib }
Uses Math;

Type
   Tower = Array [0..10] of Integer;
   Hanoi = Array [0 .. 2] of Tower;

Var
   disks : LongInt;

Procedure printinfo(disks : LongInt); { Print information about the puzzle generated }
begin
   writeln('Towers of Hanoi Solver.');
   writeln('    ', disks, ' disks.');
   writeln('    ', round(power(2, disks) - 1.0), ' moves required.');

   if disks mod 2 = 0 then
      writeln('    L->R move direction.')
   else
      writeln('    R->L move direction.')
end; { printinfo }

Procedure inithanoi(Var towers : Hanoi; disks : Integer); { Initialises the tower variables }
var
   tow	 : Integer;
   count : Integer;
begin
   for tow := 0 to 2 do
      for count := 0 to 10 do
      begin
	 if (disks - count >= 0) and (tow = 0) then
	    towers[tow][count] := disks - count
	 else
	    towers[tow][count] := 0
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

Procedure findmove(disks : Integer; Var towers : Hanoi; Var towerbegin, towerend : Integer; smallest : Boolean); { Find the next move that either does or does not involve moving the smallest disk }
var
   i, j, k : Integer;
   dir	   : Integer;
begin
   { Find the indexes of the last nonzero value in the towers }
   i := 0;
   j := 0;
   k := 0;

   while towers[0][i + 1] <> 0 do
      i := i + 1;

   while towers[1][j + 1] <> 0 do
      j := j + 1;

   while towers[2][k + 1] <> 0 do
      k := k + 1;

   if smallest then
   begin
      if disks mod 2 = 0 then
	 dir := 1
      else
	 dir := -1;

      if towers[0][i] = 1 then
	 towerbegin := 0
      else
	 if towers[1][j] = 1 then
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
      if towers[0][i] = 1 then
	 getmoveorder(1, 2, towers[1][j], towers[2][k], towerbegin, towerend)
      else
	 if towers[1][j] = 1 then
	    getmoveorder(0, 2, towers[0][i], towers[2][k], towerbegin, towerend)
	 else
	    getmoveorder(0, 1, towers[0][i], towers[1][j], towerbegin, towerend);
   end;
end; { findmove }

Procedure solve(disks : Integer); { Solve the puzzle }
var
   moves      : LongInt;
   move	      : LongInt;
   towerbegin : Integer;
   towerend   : Integer;
   towers     : Hanoi;
begin
   moves := round(power(2, disks) - 1.0); { Calculate the number of moves }

   move := 0;
   writeln(move:3, ': Build a tower of ', disks, ' disks.');

   { Keep track of which tower has what }
   inithanoi(towers, disks);

   for move := 1 to moves do
   begin
      { Get the next move }
      findmove(disks, towers, towerbegin, towerend, ((move - 1) mod 2 = 0));

      { And make it }
      makemove(towers[towerbegin], towers[towerend]);

      writeln(move:3, ': Move disk from tower ', towerbegin, ' to tower ', towerend, '.');
   end;
end; { solve }

begin
   { Get the number of disks }
   write('Enter the number of disks: ');
   readln(disks);
   writeln;

   { Information about the puzzle generated }
   printinfo(disks);
   writeln;

   { Now solve }
   solve(disks);
end.