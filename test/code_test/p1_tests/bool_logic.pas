program BoolLogicTest;
var
    val: boolean = true;
    val2: boolean = false;
begin

    writeln('Should be true: ', val or val2);
    writeln('Should be false: ', val and val2);

end.