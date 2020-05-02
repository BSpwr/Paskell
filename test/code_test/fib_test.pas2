program fib_test;
  function fib_case(a:integer): integer;
  begin
      case a of
          0: fib_case := 0;
          1: fib_case := 1;
          else
              fib_case := fib_case(a-1) + fib_case(a-2);
      end;
  end;

  function fib_if(a:integer): integer;
  begin
      if (a = 0) then fib_if := 0
      else if (a = 1) then fib_if := 1
      else fib_if := fib_if(a-1) + fib_if(a-2);
  end;

  begin
  if (fib_case(20) <> fib_if(20)) then
      writeln('FIB ERROR');
  writeln(fib_case(20));
  writeln(fib_if(20));
  end.