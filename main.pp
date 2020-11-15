uses
  Akko,
  Colors;

procedure TestLogic(test: AkkoUnitTest);
begin
  test.Should('check equality of two numbers correctly', 5 = 5);
  test.Should('check equality of two strings correctly', 'HelLo' = 'HelLo');
  test.Should('check disequality of two numbers correctly', 5 <> 6);
  test.Should('check disequality of two strings correctly', 'HelLo' <> 'hello')
end;

var
  test: AkkoUnitTest;
begin
  test := AkkoUnitTest.Create('pascal itself', [
    AkkoUnitTest.Create('logic', [], @TestLogic)
  ]);

  test.Run;
  test.Report
end.
