with Text_Io;

use Text_Io;

package body Msort is

   package Int_Io is new Integer_Io(Integer);
   use Int_Io;

   function merge (leftArray, rightArray : myArray) return myArray is
      mergedArray : myArray(leftArray'first .. rightArray'last);
      leftIndex : integer := leftArray'first;
      rightIndex : integer := rightArray'first;
      mergedIndex : integer := mergedArray'first;
   begin
      while leftIndex <= leftArray'last and rightIndex <= rightArray'last loop
         if leftArray(leftIndex) <= rightArray(rightIndex) then
            mergedArray(mergedIndex) := leftArray(leftIndex);
            leftIndex := leftIndex + 1;
         else
            mergedArray(mergedIndex) := rightArray(rightIndex);
            rightIndex := rightIndex + 1;
         end if;
         mergedIndex := mergedIndex + 1;
      end loop;
      if leftIndex <= leftArray'last then
         mergedArray(mergedIndex..mergedArray'last) := leftArray(leftIndex..leftArray'last);
      end if;
      if rightIndex <= rightArray'last then
         mergedArray(mergedIndex..mergedArray'last) := rightArray(rightIndex..rightArray'last);
      end if;
      return mergedArray;
   end Merge;

   procedure sort (inArray : in out myArray) is
      mid : integer;
   begin
      if inArray'Length <= 1 then
         return;
      else
         mid :=  (inArray'length / 2) + inArray'first;
         declare
            left  : myArray(inArray'first .. mid - 1);
            right : myArray(mid .. inArray'last);
         begin
            for i in left'range loop
               left(i) := inArray(i);
            end loop;
            for i in right'range loop
               right(i) := inArray(i);
            end loop;
            sort(left);
            sort(right);
            inArray := merge(left, right);
         end;
      end if;
   end sort;

end Msort;
