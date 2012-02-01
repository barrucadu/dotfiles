program BubbleSort;
{ An implementation of Bubble Sort to sort a string by ASCII code }

var
   orderstr : String; { String to order }

Function sorted (str :  String) : Boolean; { sorted(str) - return whether string is sorted or not. }
var
   issort : Boolean; { Whether the string is sorted or not }
   pos	  : Integer; { Position in the string. }
begin
   issort := TRUE;
   pos    := 1;

   { Compare each pair of characters, see if they are sorted }
   while pos < Length(str) do
   begin
      if issort then { Don't overwrite a false value! }
	 issort := ord(str[pos]) <= ord(str[pos + 1]);

      pos := pos + 1;
   end;

   sorted := issort;
end; { sorted }

Function bubblepass (str : String) : String; { bubblepass(str) - perform one pass of bubble sort on str. }
var
   tmp	  : String;  { Temporary working string }
   pos	  : Integer; { Position in the string. }
   tmpchr : Char;    { temporary character (used for swapping). }
begin
   tmp    := str;
   pos    := 1;
   tmpchr := '0';

   { Check every pair of characters in the string. }
   while pos < Length(str) do
   begin
      { And swap if they're the wrong way around. }
      if (ord(tmp[pos]) > ord(tmp[pos + 1])) then
      begin
	 tmpchr       := tmp[pos];
	 tmp[pos]     := tmp[pos + 1];
	 tmp[pos + 1] := tmpchr;
      end;

      pos := pos + 1;
   end;

   bubblepass := tmp;
end; { bubblepass }

Function bubblesort (str :  String) : String; { bubblesort(str) - perform bubblepas(str) until sorted(str) = TRUE. }
begin
   while sorted(str) = FALSE do
   begin
      str := bubblepass (str);
   end;

   bubblesort := str;
end;

begin
   { Get a string. }
   write('Enter a string to sort: ');
   readln(orderstr);

   { Sort and display it. }
   orderstr := bubblesort(orderstr);
   writeln(orderstr);
end.
