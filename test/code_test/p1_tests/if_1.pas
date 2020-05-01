program If1Test;
var
   grade: string = 'A';
   good: boolean = true;
   junk, junk2: string;
   val: real = 5;
   b: real = 5;
   s, a: real;
begin

    grade := 'B';

   if grade = 'B' then
      begin
      writeln('good');
      writeln('second statement of compound')
      end
   else if grade = 'A' then
      writeln('WOW A!')
   else if grade <> 'F' then
      writeln('Not Fail!')
   else writeln('Bad job, its an F');
   
   val := 7;
   b := 7;
   writeln(val + ((b+1) * 2));
   writeln('Your grade is an: ', grade);

end.