program iter_test;
  function fib_while(a:integer): integer;
  var idx : integer = 1;
  var i : integer = 0;
  var n : integer = 1;
  begin
    while idx<a do
      begin
        n := i + n;
        i := n - i;
        idx := idx + 1;
        end;
    fib_while := n;
  end;

  function fib_for(a:integer): integer;
  var idx : integer;
  var i : integer = 0;
  var n : integer = 1;
  begin
    for idx := 1 to a-1 do
      begin
        n := i + n;
        i := n - i;
        end;
    fib_for := n;
  end;

  begin
    if (fib_while(20) <> fib_for(20)) then
      writeln('FIB ERROR');
    writeln(fib_while(42));
    writeln(fib_for(42));
  end.