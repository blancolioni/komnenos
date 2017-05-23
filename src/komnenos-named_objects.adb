package body Komnenos.Named_Objects is

   --------------
   -- Set_Name --
   --------------

   procedure Set_Name
     (Item : in out Root_Named_Object'Class;
      Name : String)
   is
   begin
      Item.Object_Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Set_Name;

end Komnenos.Named_Objects;
