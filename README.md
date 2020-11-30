# Akko

This is a testing framework for Pascal. The name is a reference to absolutely nothing whatsoever.

To install Akko, run this command:

~~~
ppm install ppmpkgs/akko
~~~

To use Akko in code, type this:

~~~
uses Akko;
~~~

## Usage

Suppose you had the following unit called `mymath.pas` in your package:

~~~ pascal
unit MyMath;

interface

  function Factorial(n: Integer): Integer;

implementation

  function Factorial(n: Integer): Integer;
  begin
    if n = 0 then
      Factorial := 0
    else
      Factorial := n + Factorial(n - 1)
  end;

end.
~~~

The way to test it is to create a test file (usually called `test.pp`), that looks a little something like this:

~~~ pascal
uses Akko, MyMath;

procedure TestFactorial(&unit: AkkoUnitTest);
begin
  &unit.Should('correctly calculate the factorial of 1', Factorial(1) = 1);
  &unit.Should('correctly calculate the factorial of 3', Factorial(3) = 3 + 2 + 1)
end;

var
  myMathTest: AkkoUnitTest;
begin
  myMathTest := AkkoUnitTest.Create('MyMath', [
    AkkoUnitTest.Create('Factorial', [], @TestFactorial)
  ]);
  myMathTest.Run
end.
~~~
