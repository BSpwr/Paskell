program tester
const
PI = 3.141592654;
GTT = 'sack';
type
SUMMER = (April, May, June, July, September);
var gee : string = 'st' ++ 'ring';;;;;;;
dee : string;;;;;;
ADC : integer = 5;
a,b,c,i : real;
d,e : boolean;
number : integer = 10;
sum : integer = 0;
grade: string;
eddh: SUMMER = April;
function hiddenNumber(grade2, gee2: string; sum2: integer): integer;
begin
    writeln('hii');
    sum2 := sum2 + 42;
    hiddenNumber := sum2;
end;
procedure sayHello;
begin
     writeln('hello world!');
end;
//
//
begin
grade := 'C';
writeln(eddh);
a := 3 + 5;;;;;;;;;;;
b := 6.54 + 434343;
c := a + b;
d := false || ((false && false) || true);
dee := gee ++ 'aaaa';;
//readln(a, b);;
writeln(grade);
for i:= 1 to 4 do writeln(i);
repeat
   sum := sum + number;
   break;
   number := number - 2;
until number = 0;
writeln(sum);
writeln(PI);
sayHello;
hiddenNumber(grade, gee, sum);
writeln(hiddenNumber(grade, gee, sum));
number := 10;
writeln(GTT);
while number>0 do
begin
   if sum = 28 then begin sum := sum + number; continue; end;
   if sum < 100 then writeln(sum);
   if sum = 34 then break;
   sum := sum + number;
   number := number - 2
end;
writeln(sum);
case (grade) of
    'A' : writeln('Excellent!' );
    'B', 'C': writeln('Well done' );
    'D' : writeln('You passed' );
    'F' : writeln('Better try again' );
    else
        writeln('junk!');
end;  
if 'aa' = 'aa' then
    begin
    c := 21;
    writeln('hahaha');
    end
else if 'aa' = 'ab' then
    writeln('nooo')
else writeln('fin');
writeln(c);
writeln('nut' = 'nut');
writeln('lol ', 5 + 2, true && false, a, b, c, d, ' ' ++ gee);;;;
end
.