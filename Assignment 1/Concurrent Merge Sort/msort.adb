with Text_Io;
use Text_Io;

package body Msort is

   package Int_Io is new Integer_Io(Integer);
   use Int_Io;

   function merge (left, right : myArray) return myArray is
      merged : myArray(left'first .. right'last);
      leftI : integer := left'first;
      rightI : integer := right'first;
      mergedI : integer := merged'first;
   begin
      while leftI <= left'last and rightI <= right'last loop
         if left(leftI) <= right(rightI) then
            merged(mergedI) := left(leftI);
            leftI := leftI + 1;
         else
            merged(mergedI) := right(rightI);
            rightI := rightI + 1;
         end if;
         mergedI := mergedI + 1;
      end loop;
      if leftI <= left'last then
         merged(mergedI..merged'last) := left(leftI..left'last);
      end if;
      if rightI <= right'last then
         merged(mergedI..merged'last) := right(rightI..right'last);
      end if;
      return merged;
   end merge;

   function fSort (fArray : in out myArray) return myArray is
      mid : integer :=  (fArray'length / 2) + fArray'first;
      left  : myArray(fArray'first .. mid - 1);
      right : myArray(mid .. fArray'last);

      task sortLeft is
         entry start;
      end sortLeft;
      task sortRight is
         entry start;
      end sortRight;

      task body sortLeft is
      begin
         accept start;
         if (left'Length > 1) then
            left := fSort(left);
         end if;
      end sortLeft;

      task body sortRight is
      begin
         accept start;
         if (right'Length > 1) then
            right := fSort(right);
         end if;
      end sortRight;

   begin
      for i in left'range loop
         left(i) := fArray(i);
      end loop;
      for i in right'range loop
         right(i) := fArray(i);
      end loop;
      sortLeft.start;
      sortRight.start;
      while (not sortLeft'Terminated or not sortRight'Terminated) loop
         null;
      end loop;
      fArray := merge(left, right);
      return fArray;
   end fSort;

   procedure sort(inArray : in out myArray) is
   begin
      inArray := fSort(inArray);
   end sort;

end Msort;
