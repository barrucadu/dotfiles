program LinearSearch;
{ An implementation of the Linear Search algorithm to find a character in a string. }

var
   searchstr : String;  { The string entered by the user. }
   searchchr : Char;    { The character to search for. }
   pos	     : Integer; { The position of the character. }

Function linsearch (str : String; chr : Char) : Integer; { linsearch(str, chr) - find the first occurrence of chr in str, return position. }
var
   pos : Integer; { The position }
begin
   pos := 1;

   { Increment pos until we find chr. }
   while str[pos] <> chr do
      pos := pos + 1;

   linsearch := pos - 1; { String indexes count from 1 in Pascal. In everything else, they count from 0. }
end;

begin
   { Get a string to search. }
   write('Enter a string: ');
   readln(searchstr);

   { Get a character to search for. }
   write('Enter a character: ');
   readln(searchchr);

   pos := linsearch(searchstr, searchchr);

   if pos > Length(searchstr) then
      writeln(searchchr, ' not found.')
   else
      writeln(searchchr, ' found at position ', pos)
end.
