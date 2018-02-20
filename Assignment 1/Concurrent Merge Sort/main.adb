with Text_Io;
with Msort;

use Text_Io;
use Msort;

procedure Main is

   package Int_Io is new Integer_Io(Integer);
   use Int_Io;

   next : integer;
   total : integer := 0;

   task Reader is
      entry start;
   end Reader;

   task Sum is
      entry start;
   end Sum;

   task Print is
      entry start;
      entry printSum;
   end Print;

   task body Reader is
   begin
      accept start do
         for i in 1 .. inputArray'length loop
            get(next);
            inputArray(i) := next;
         end loop;
      end start;
   end Reader;

   task body Sum is
   begin
      accept start;
      for i in 1 .. inputArray'length loop
         total := total + inputArray(i);
      end loop;
      Print.printSum;
   end Sum;

   task body Print is
   begin
      accept start;
      Sum.start;
      for i in 1 .. inputArray'length loop
         Put(inputArray(i));
         New_Line;
      end loop;
      accept printSum;
      New_Line;
      Put(total);
   end Print;

begin
   Reader.start;
   sort(inputArray);
   Print.start;
end Main;

