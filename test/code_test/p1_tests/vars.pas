program VarsTest;
var
   grade: string = 'A';
   good: boolean = false;
   junk, junk2: string;
   val: real = 5;
   b: real = 5;
   s, a: real;
begin

    grade := 'Z';
    junk := 'A';
    junk2 := 'B';
    s := 5;
    a := 10;
    b := b * b * b;

    writeln(grade, ' ', good);
    writeln(junk, ' ', junk2);
    writeln(s);
    writeln(a);
    writeln(b);

end.