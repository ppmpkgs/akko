unit Akko;

{$mode objfpc}
{$h+}

interface

  uses
    SysUtils,
    Colors;

  const
    INDENTATION_STR = '    ';
    SUCCESS_CHAR: Char = '+';
    FAILURE_CHAR: Char = 'X';

  type
    AkkoUnitTest = class;

    AkkoRunRoutine = procedure(test: AkkoUnitTest);

    AkkoAssertionResultPtr = ^AkkoAssertionResult;
    AkkoAssertionResult = record
      title: String;
      result: Boolean;
    end;

    AkkoUnitTests = Array of AkkoUnitTest;
    AkkoAssertionResults = Array of AkkoAssertionResult;

    AkkoUnitTest = class
      title: String;
      tests: AkkoUnitTests;
      runRoutine: AkkoRunRoutine;
      successful: Boolean;
      assertionResults: AkkoAssertionResults;
      thrownException: Exception;

      constructor Create(ATitle: String; ATests: AkkoUnitTests; ARun: AkkoRunRoutine = nil);
      procedure Run;
      procedure Should(what: String; condition: Boolean);
      procedure Report(depth: LongWord = 0);
    end;

implementation

  uses StrUtils;

  constructor AkkoUnitTest.Create(ATitle: String; ATests: AkkoUnitTests; ARun: AkkoRunRoutine = nil);
  begin
    title := ATitle;
    tests := ATests;
    runRoutine := ARun
  end;

  procedure AkkoUnitTest.Run;
  var
    i: LongWord;
  begin
    if Assigned(tests) and (Length(tests) <> 0) then
      for i := Low(tests) to High(tests) do
        tests[i].Run;

    if Assigned(runRoutine) then
      try
        runRoutine(self)
      except
        on e: Exception do begin
          successful := false;
          thrownException := e
        end
      end
  end;

  procedure AkkoUnitTest.Should(what: String; condition: Boolean);
  begin
    successful := successful and condition;
    SetLength(assertionResults, Length(assertionResults) + 1);

    with assertionResults[High(assertionResults)] do begin
      title := what;
      result := condition
    end
  end;

  procedure AkkoUnitTest.Report(depth: LongWord = 0);
  var
    indent: String;
    i: LongWord;
  begin
    if depth = 0 then WriteLn;

    indent := DupeString(INDENTATION_STR, depth);

    WriteLn(indent + title);
    WriteLn(indent + DupeString('-', Length(title)));
    WriteLn;
    indent += INDENTATION_STR;

    if Assigned(tests) and (Length(tests) <> 0) then begin
      for i := Low(tests) to High(tests) do
        tests[i].Report(depth + 1);
      WriteLn
    end;

    if Assigned(assertionResults) and (Length(assertionResults) <> 0) then
      for i := Low(assertionResults) to High(assertionResults) do
        WriteLn(indent +
          BoolToStr(assertionResults[i].result, COLOR_GREEN.S, COLOR_RED.S) +
          '[' +
          BoolToStr(assertionResults[i].result, SUCCESS_CHAR, FAILURE_CHAR) +
          ']' + '  should ' + assertionResults[i].title + COLOR_RESET.S);

    if depth = 0 then WriteLn
  end;

end.
