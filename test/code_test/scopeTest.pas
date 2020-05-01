program scopeTest;
var
   a, b, c: integer;

procedure scopeTest(a,b,c: integer);
begin
   a := 10;
   b := 20;
   c := a + b;

   writeln('In function scope');
   writeln('value of a = ', a , ' b =  ',  b, ' and c = ', c);
end;

begin
   a:= 100;
   b:= 200;
   c:= a + b;

   writeln('In global scope');
   writeln('value of a = ', a , ' b =  ',  b, ' and c = ', c);
   scopeTest(a, b, c);

  writeln('Back in global scope');
  writeln('value of a = ', a , ' b =  ',  b, ' and c = ', c);
end.