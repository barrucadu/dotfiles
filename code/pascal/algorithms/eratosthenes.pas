program Eratosthenes;
{ An implementation of the Sieve of Eratosthenes to find all primes up to, and including, PRIME_MAX. }

const
   PRIME_MAX  = 8000; { No prime gets bigger than this. This is the size of the sieve. 1000th prime is 7919, 8000 is a nice round number. }
   NUM_PRIMES = 1000; { The maximum number of primes to calculate. }

var
   nth	      : Integer; { The Nth prime to find }
   st, en     : Integer; { Start and end of range }
   instr      : String;  { Temporary string for grabbing input }

Function readint (question : String; min, max : Integer) : Integer; { readint(question, min, max) - ask for an integer between min and max, asking question. }
var
   str	       : String;  { Temporary string for grabbing input }
   tmpint, err : Integer; { Integer value of str, and error code (val function) }

begin
   err := 1;

   while err <> 0 do
   begin
      { Print question and value range, reading the input and generating an integer }
      write(question, ' [', min, '-', max, ']: ');
      readln(str);
      val(str, tmpint, err);

      { Check if the user did something stupid }
      if err <> 0 then
	 writeln('Something bad happened. Shall we try that again?');

      { Check if the user actually paid any attention whatsoever to the range }
      if (tmpint > max) or (tmpint < min) then
	 writeln('Value out of range [', min, '-', max, ']');
   end;

   readint := tmpint;
end; { readint }

Procedure eratosthenes (nth, st, en : Integer); { eratosthenes(nth, st, en) - perform the Sieve of Eratosthenes, printing either the nth prime }
var                                             { or all primes between st and en. }
   sieve      : Array [0 .. PRIME_MAX] of Boolean; { The sieve. Each entry represents a number. }
   i, j, p, n : Integer; { Counting variable: i, j: counters. p: current prime. n: number of primes. }
begin
   p   := 0; { Setting initial values is good. }
   n   := 0;

   { Mark all numbers as available. }
   for i := 0 to PRIME_MAX do
   begin
      sieve[i] := FALSE;
   end;

   { Mark 0 and 1 as not available. }
   sieve[0] := TRUE;
   sieve[1] := TRUE;

   for i := 0 to NUM_PRIMES do
   begin
      { Find the next allowed number (ie: sieve[x] = FALSE). }
      while sieve[p] = TRUE do
      begin
	 p := p + 1;
      end;

      { p is now prime. Mark all multiples of it unavailable. }
      j := 0;
      while j * p < PRIME_MAX do
      begin
	 sieve[j * p] := TRUE;
	 j := j + 1;
      end;

      { Increment the prime counter. }
      n := n + 1;

      if n = nth then { If asked to find the nth, and n = nth, print p. }
	 writeln(p)
      else
	 if (n >= st) and (n <= en) then { If asked to find st to en, and st <= n <= en, print p. }
	    writeln(p)
   end
end; { eratosthenes }

begin
   nth   := 0; { We'll use zero in eratosthenes() to check what to do }
   st    := 0;
   en    := 0;
   instr := '';

   while (instr <> 'R') and (instr <> 'I') do { A loop because some fool is guaranteed to enter an invalid value. }
   begin
      write('Do you want to find a range of prime numbers or an individual number? (R/I): ');
      readln(instr);
   end;

   { Now grab all the numbers we're using }
   if instr = 'I' then
      nth := readint('Which prime do you want to find?', 1, NUM_PRIMES)
   else
   begin
      st := readint('Enter start number.', 1, NUM_PRIMES);
      en := readint('Enter end number.', 1, NUM_PRIMES);

      if (st > en) then
      begin
	 write('Start value too big (', st, ' > ', en, '). '); { Fool. }
	 writeln('I shall assume you meant to have the numbers the other way around. Do not do it again.');

	 { Swap st and en without using a third variable as a temporary store. }
	 en := en + st;
	 st := en - st;
	 en := en - st;
      end;
   end;

   { Sieve numbers }
   eratosthenes(nth, st, en);
end.