program calculator;

type meme = (cat, nyan, pepe);

var F,L:real;
    b: boolean;
    i,j,n:integer;
    s:string;
    x:array[1..10] of real;
    y:array[1..10] of real;
    m:meme;
    d:meme;
    number,sum:integer;
    taco_loop:integer;

const colMax = 76;
      rowMax = 22;
      dr = colMax+2;
      cDelay = 20;
      letterA = 'A';

function double (t: integer): integer;
begin
writeln(t);
double := t * 2;
end;

function triple (g: integer): integer;
var
    result: integer;
begin
writeln(g);
result := g * 3;
end;

procedure ree (bee, d: boolean; st: string);
begin
writeln(st);
writeln(bee, ' and ', d);
end;

function fib(a:integer): integer;
begin
    case a of
        0: fib := 0;
        1: fib := 1;
        else
            fib := fib(a-1) + fib(a-2);
    end;
end;

procedure print_msg();
var
    str: string = 'look ma, no args!';
begin
writeln(str);
break
end;

begin
  b:=false;
  d:=cat;

  writeln(double(456));
  writeln(triple(1234));
  ree(true, false, 'wow!');

  (*Nested Ifs*)
  writeln('Testing Nested Ifs');
  if (b <> true) then
  begin
    m:=pepe;
    b:=true;
    i:=0;
    j:=10;
    n:= j*10;
    F:= 30.0;

    (*Functions*)
    writeln('Testing functions');
    writeln('Value to take the sine of: ');
    writeln(F);
    writeln(' degrees');
    L:= sin(F*(PI/180.0));
    writeln('Sin: ');
    writeln(L);
    L:= 45.0;
    F:= cos(L*(PI/180.0));
    writeln('Cos: ');
    writeln(F);
    L:= sqrt(F);
    writeln('Sqrt: ');
    writeln(L);
    L:=20.0;
    writeln('Value to take the natural log of: ');
    writeln(F);
    F:= ln(L);
    writeln('Ln: ');
    writeln(F);
    L:= exp(F);
    writeln('Exp: ');
    writeln(L);

    (*Case*)
    writeln('Testing Cases');
    writeln('Case 1: ');
    case letterA of
        'C': writeln('Letter was C');
        'B': writeln('Letter was B');
    end;
    writeln('Case 2: ');
    case letterA of
        'A': writeln('Letter was A');
        'B': writeln('Letter was B');
    end;
    writeln('Case 3: ');
    case letterA of
        'C': writeln('Letter was B');
        'B': writeln('Letter was B');
        else
            writeln('Letter was not in case!');
    end;
  end
  else
  begin

    (*Readln*)
  //  writeln('Testing readln');
   // writeln('Enter a value:');
   // readln(s,b);
  //  writeln('My value was: ',s,' and the value of b is ',b);
    (*This is a comment, this does not affect the code at all!*)
  //  if(b = true) then
   //     writeln('b was true!');
  end;
 // writeln('Enter a value:');
 // readln(s,b);
//  writeln('My value was: ',s,' and the value of b is ',b);
  (*This is a comment, this does not affect the code at all!*)
  if(b = true) then
      writeln('b was true!');

  (*Enums*)
  writeln('Testing Enum Equivalence');
  if(m <> d) then
      writeln('enum works!');

  (*Comparisons*)
  writeln('Testing Comparisons');
  if(i > n) then
      writeln('i was greater than n')
  else
      writeln('i was less than n');

   print_msg();

   number := 8;
   while number>0 do
   begin
       number := number - 1;
       if (number = 6) then break;
       writeln(number);
   end;

   writeln('--');

  number := 8;
  while number>0 do
  begin
     number := number - 1;
     if (number = 4) then continue;
     writeln(number);
  end;

  writeln('--');
  for taco_loop:= 1 to 10 do
  begin
  if (taco_loop = 6) then continue
  else if (taco_loop = 8) then break;
  writeln(taco_loop);
  end;

  writeln('--');
  writeln(taco_loop);
  writeln('--');

    i := 0;

    repeat
    if (i = 6) then
    begin
        i := i + 1;
    continue
    end
    else if (i = 8) then break;
        writeln(i);
        i := i + 1;
    until i = 10;

  writeln('All tests completed!');
end
.