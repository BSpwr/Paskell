program Case2Test;
var
    val: real;
begin

   val := 19;

    case (val < 20) of
      true: writeln('Yep, 19 is < 20' );
      false : writeln('Better try again' );
    else
      writeln('Should never run');
   end;

end.