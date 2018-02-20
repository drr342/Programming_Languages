package Msort is

   subtype myInteger is integer range -300 .. 300;
   type myArray is array(integer range <>) of myInteger;
   LENGTH : constant integer := 40;
   inputArray : myArray(1 .. LENGTH);
   
   procedure Sort (inArray : in out myArray);
   
end Msort;
