program Beginner9;

{ Your program should be able to open a text file and read its contents.
  The file will consist only of LOWERCASE letters and numbers 0-9.
  The file will be formatted such that there is only one alphanumeric character per line.

  Your program must read each line and store the data in a data structure of your choice. Parse through your structure and print out the following information:
    - The sum of all the individual digits.
    - How many times each character appeared in the file

  http://ubuntuforums.org/showthread.php?t=1407897

  Attempt by Barrucadu <mike@barrucadu.co.uk>. }

Uses SysUtils;

const
   FILESIZE = 1000000;         { Works for a file of up to 1,000,000 lines. Todo: figure out a way to dynamically grow the lines arrays. }
   FILENAME = 'beginner9.txt'; { Filename of data file. }

type
   carr	 = Array [0 .. FILESIZE] of Char;
   pcarr =  ^carr;

var
   lines : carr; { Data structure for read file ("read each line and store the data") }

Procedure readfile (fname : String; lines : pcarr); { readfile(fname) - read filename into an array. Pascal doesn't allow returning an array, so pointers. }
var
   fh	 : TextFile; { File handler }
   i	 : Integer;  { Line counter }
begin
   i := 0;
   
   AssignFile(fh, fname); { Assign the file handler with the file }
   reset(fh);             { Open file in read mode }

   { While we still have stuff to read }
   while not eof(fh) do
   begin
      if i <= FILESIZE then
      begin
	 { Read a line and increment the counter. }
	 readln(fh, lines^[i]);
	 i := i + 1;
      end
      else
      begin
	 { Todo: figure a way around this. }
	 writeln('File too big, ignoring rest of lines.');
	 exit;
      end;
   end;

   CloseFile(fh); { Close the file handler }
end; { readfile }

Function countchar (pfile : carr; ascii : Char) : Integer; { countchar(pfile, ascii) - return the number of occurrences of ascii in pfile. }
var
   count, i : Integer; { Counting variable, and loop counter. }
begin
   count := 0;
   
   for i := 0 to FILESIZE do
   begin
      if pfile[i] = ascii then
      begin
	 count := count + 1;
      end;
   end;

   countchar := count;
end; { countchar }

Function sumtotal (pfile : carr) : Integer; { sumtotal(pfile) - return the total of all integers in pfile. }
var
   sum, i : Integer; { Sum variable, and loop counter. }
begin
   sum := 0;

   { For every line do }
   for i := 0 to FILESIZE do
   begin
      if (pfile[i] >= '0') and (pfile[i] <= '9') then { It's a number :D }
	 sum := sum + ord(pfile[i]) - 48;
   end;

   sumtotal := sum;
end;

Procedure showchars (pfile : carr); { showchars(pfile) - show char counts. }
var
   ascii, count	: Integer; { Current ascii code, and character count. }
begin
   for ascii := 32 to 126 do { The printing characters }
   begin
      count := countchar(pfile, chr(ascii)); { Get the character count. }

      if count > 0 then
	 writeln(chr(ascii), ' (', ascii, '): ', count); { Display character, ASCII code, and count }
   end;
end;

Procedure showsum (pfile : carr); { showsum(pfile) - show the sum of all digits. }
begin
   writeln('Sum total of all digits: ', sumtotal(pfile));
end; { showsum }

begin
   readfile(FILENAME, @lines); { Read/parse the file. }
   showchars(lines);           { Show character counts. }
   showsum(lines);             { Show sum total. }
end.
