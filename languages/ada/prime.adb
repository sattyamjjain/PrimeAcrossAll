with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Prime is
   Number : Integer;
   Is_Prime : Boolean := True;
begin
   Put("Enter a number: ");
   Get(Number);

   if Number < 2 then
      Is_Prime := False;
   else
      for I in 2 .. Number / 2 loop
         if Number mod I = 0 then
            Is_Prime := False;
            exit;
         end if;
      end loop;
   end if;

   if Is_Prime then
      Put_Line("The number is prime.");
   else
      Put_Line("The number is not prime.");
   end if;
end Prime;
