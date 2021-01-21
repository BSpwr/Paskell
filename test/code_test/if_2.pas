program If2Test;
var
   grade: string = 'A';
begin

    grade := 'F';

   if grade = 'B' then
      begin
      writeln('good');
      writeln('second statement of compound')
      end
   else if grade = 'A' then
      writeln('WOW A!')
   else if grade <> 'F' then
      writeln('Not Fail!')
   else begin
        writeln('Compound statement 1');
        writeln('Compound statement 2')
   end;

end.