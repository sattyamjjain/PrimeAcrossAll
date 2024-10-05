program PrimeCheck;

function IsPrime(n: Integer): Boolean;
var
  i: Integer;
begin
  if n <= 1 then
    IsPrime := False
  else if n = 2 then
    IsPrime := True
  else if n mod 2 = 0 then
    IsPrime := False
  else
  begin
    IsPrime := True;
    i := 3;
    while i * i <= n do
    begin
      if n mod i = 0 then
      begin
        IsPrime := False;
        Break;
      end;
      i := i + 2;
    end;
  end;
end;

var
  number: Integer;
begin
  ReadLn(number);
  if IsPrime(number) then
    WriteLn('true')
  else
    WriteLn('false');
end.
