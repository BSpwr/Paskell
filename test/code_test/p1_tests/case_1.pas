program Case1Test;
var
    grade: string;
    b: real = 5;
    val: real = 10;
begin

    grade := 'A';

   case (grade) of
      'A': writeln('Excellent!' );
      'B': writeln('Well done' );
      'Z' : writeln(val, exp(sqrt(val + b)));
      'F' : writeln('Better try again' );
    else
      writeln('Bad!');
   end;

   val := 4;

    case (val = 4) of
      true: writeln('Well done' );
      false : writeln('Better try again');
    else
      writeln('Should never run');
   end;

end.