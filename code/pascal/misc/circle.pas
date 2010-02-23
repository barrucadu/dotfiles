program circle;

const
   pi =  3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679;

var
   radius : Real;
   circumference, area : Integer;

begin
   Write('Enter the circle radius: ');
   Readln(radius);

   circumference := round(2 * pi * radius);
   area          := round(radius * pi * pi);
   
   Writeln('The circumference is ', circumference, ', and the area is ', area);
end.