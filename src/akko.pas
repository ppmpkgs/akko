unit Akko;

{$mode objfpc}
{$h+}

interface

  uses
    SysUtils,
    ConsoleColors;

  const
    DISCONNECTED_INDENTATION_STR: String = ' |   ';
    CONNECTED_INDENTATION_STR: String    = ' |-  ';
    SUCCESS_CHAR: Char                   = '+';
    FAILURE_CHAR: Char                   = 'x';
    EXCEPTION_PREFIX: String             = 'ERR: ';

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
    AkkoAssertionResultPtrs = Array of AkkoAssertionResultPtr;
    AkkoAssertionRoutine = function(test: AkkoUnitTest): Boolean;

    AkkoUnitTest = class
      title: String;
      tests: AkkoUnitTests;
      runRoutine: AkkoRunRoutine;
      successful: Boolean;
      assertionResults: AkkoAssertionResults;
      successfulAssertionResults: AkkoAssertionResultPtrs;
      thrownException: Exception;

      constructor Create(ATitle: String; ATests: AkkoUnitTests; ARun: AkkoRunRoutine = nil);
      procedure Run(AReport: Boolean = true);

      procedure Assert(what: String; condition: Boolean);
      procedure Assert(what: String; routine: AkkoAssertionRoutine);

      procedure Should(what: String; condition: Boolean);
      procedure Should(what: String; routine: AkkoAssertionRoutine);

      procedure Report(depth: LongWord = 0);
    end;

implementation

  uses StrUtils;

  constructor AkkoUnitTest.Create(ATitle: String; ATests: AkkoUnitTests; ARun: AkkoRunRoutine = nil);
  begin
    title := ATitle;
    tests := ATests;
    runRoutine := ARun;
    thrownException := nil
  end;

  procedure AkkoUnitTest.Run(AReport: Boolean = true);
  var
    i: LongWord;
  begin
    if Assigned(tests) and (Length(tests) <> 0) then
      for i := Low(tests) to High(tests) do
        tests[i].Run(false);

    if Assigned(runRoutine) then
      try
        runRoutine(self)
      except
        on e: Exception do begin
          successful := false;
          thrownException := Exception.Create(e.Message)
        end
      end;

    if AReport then Report
  end;

  procedure AkkoUnitTest.Assert(what: String; condition: Boolean);
  begin
    successful := successful and condition;
    SetLength(assertionResults, Length(assertionResults) + 1);

    with assertionResults[High(assertionResults)] do begin
      title := what;
      result := condition
    end;

    if condition then begin
      SetLength(successfulAssertionResults, Length(successfulAssertionResults) + 1);
      successfulAssertionResults[High(successfulAssertionResults)] :=
        @assertionResults[High(assertionResults)]
    end
  end;

  procedure AkkoUnitTest.Assert(what: String; routine: AkkoAssertionRoutine);
  var
    condition: Boolean = false;
  begin
    try
      condition := routine(self)
    except
      on e: Exception do begin
        condition := false;
        successful := false;
        thrownException := Exception.Create(e.Message)
      end
    end;

    Assert(what, condition)
  end;

  procedure AkkoUnitTest.Should(what: String; condition: Boolean);
  begin
    Assert('should ' + what, condition)
  end;

  procedure AkkoUnitTest.Should(what: String; routine: AkkoAssertionRoutine);
  begin
    Assert('should ' + what, routine)
  end;

  procedure AkkoUnitTest.Report(depth: LongWord = 0);
  var
    indent: String;
    i: LongWord;
  begin
    if depth = 0 then WriteLn;

    if depth = 0 then
      indent := ''
    else if depth = 1 then
      indent := CONNECTED_INDENTATION_STR
    else
      indent := DupeString(DISCONNECTED_INDENTATION_STR, depth - 1) + CONNECTED_INDENTATION_STR;

    // Print the test header
    WriteLn(indent + COLOR_BLUE.S + title + COLOR_RESET.S + ' (' +
      COLOR_GREEN.S +
      IntToStr(Length(successfulAssertionResults)) +
      COLOR_RESET.S +
      '/' +
      COLOR_BLUE.S +
      BoolToStr(
        Assigned(thrownException), '?',
        IntToStr(Length(assertionResults))) +
      COLOR_RESET.S +
      ')'
    );

    Inc(depth);

    if depth = 1 then
      indent := CONNECTED_INDENTATION_STR
    else
      indent := DupeString(DISCONNECTED_INDENTATION_STR, depth - 1) + CONNECTED_INDENTATION_STR;

    if Assigned(tests) and (Length(tests) <> 0) then
      for i := Low(tests) to High(tests) do
        tests[i].Report(depth);

    if Assigned(assertionResults) and (Length(assertionResults) <> 0) then
      for i := Low(assertionResults) to High(assertionResults) do
        WriteLn(indent +
          BoolToStr(assertionResults[i].result, COLOR_GREEN.S, COLOR_RED.S) +
          '[' +
          BoolToStr(assertionResults[i].result, SUCCESS_CHAR, FAILURE_CHAR) +
          ']' + '  ' + assertionResults[i].title + COLOR_RESET.S);

    if Assigned(thrownException) then
      WriteLn(indent + COLOR_RED.S + EXCEPTION_PREFIX + thrownException.Message + COLOR_RESET.S);

    if depth = 0 then WriteLn
  end;

end.
