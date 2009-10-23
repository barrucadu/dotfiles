program angle;

const
      pi = 3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679;

var
   radians, degrees, sine, cosine, tangent, secant, cosecant, cotangent	:  Real;

Function tan (theta : Real) : Real;
begin
   tan := sin(theta) / cos(theta)
end; { tan }

Function sec (theta : Real) : Real;
begin
   sec := 1 / cos(theta)
end; { sec }

Function csc (theta : Real) : Real;
begin
   csc := 1 / sin(theta)
end; { csc }

Function cot (theta : Real) : Real;
begin
   cot := 1 / tan(theta)
end; { cot }

begin
   write('Enter an angle in degrees: ');
   read(degrees);

   radians   := (degrees / 180) * pi;
   sine      := sin(radians);
   cosine    := cos(radians);
   tangent   := tan(radians);
   secant    := sec(radians);
   cosecant  := csc(radians);
   cotangent := cot(radians);

   writeln();
   writeln('Radians: ', radians:2:10);
   writeln('sin(', degrees:2:2, ') = ', sine:1:5);
   writeln('cos(', degrees:2:2, ') = ', cosine:1:5);
   writeln('tan(', degrees:2:2, ') = ', tangent:1:5);
   writeln('sec(', degrees:2:2, ') = ', secant:1:5);
   writeln('csc(', degrees:2:2, ') = ', cosecant:1:5);
   writeln('cot(', degrees:2:2, ') = ', cotangent:1:5);
end.