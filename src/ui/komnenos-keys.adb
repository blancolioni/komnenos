package body Komnenos.Keys is

   ------------
   -- Modify --
   ------------

   function Modify (Key      : Komnenos_Key;
                    Modifier : Modifier_Keys)
                    return Komnenos_Key
   is
      Result : Komnenos_Key := Key;
   begin
      for M in Modifier'Range loop
         if Modifier (M) then
            Result := Result + Modifier_Masks (M);
         end if;
      end loop;
      return Result;
   end Modify;

end Komnenos.Keys;
