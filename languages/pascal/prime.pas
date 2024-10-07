program PrimeCheck;

uses sysutils;

var
   n: LongInt;
   isPrime: Boolean;
   i: LongInt;
begin
   Write('Enter a number to check if it''s prime: ');
   ReadLn(n);
   isPrime := True;

   if n <= 1 then
      isPrime := False
   else if n <= 3 then
      isPrime := True
   else if (n mod 2 = 0) or (n mod 3 = 0) then
      isPrime := False
   else
   begin
      i := 5;
      while i * i <= n do
      begin
         if (n mod i = 0) or (n mod (i + 2) = 0) then
         begin
            isPrime := False;
            Break;
         end;
         i := i + 6;
      end;
   end;

   if isPrime then
      WriteLn(n, ' is a prime number.')
   else
      WriteLn(n, ' is not a prime number.');
end.
