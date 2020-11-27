uses
  Akko,
  SysUtils;

function WorkingFunc(_: AkkoUnitTest): Boolean;
begin
  WriteLn('> hello from a working function!');
  WorkingFunc := true
end;

function BrokenFunc(_: AkkoUnitTest): Boolean;
begin
  WriteLn('> hello from a broken function!');
  raise Exception.Create('Uh-oh')
end;

procedure TestAkko(test: AkkoUnitTest);
begin
  test.Assert('this is a successful test', 5 = 5);
  test.Assert('this is a failed test', 5 = 6);
  test.Assert('calling a working function', @WorkingFunc);
  test.Assert('calling a broken function', @BrokenFunc)
end;

procedure NestedTest(test: AkkoUnitTest);
begin
  test.Assert('this assertion will run', true);
  raise Exception.Create('Some error');
  test.Assert('this one won''t', not true);
end;

var
  test: AkkoUnitTest;
begin
  test := AkkoUnitTest.Create('akko itself', [
    AkkoUnitTest.Create('nested test', [], @NestedTest)
  ], @TestAkko);
  test.Run
end.
