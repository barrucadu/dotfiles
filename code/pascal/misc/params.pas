program params;

var
   i : Integer;

begin
   for i := 0 to ParamCount do
      writeln(ParamStr(i)); { ParamStr() is a function, not an array. }
end.