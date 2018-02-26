with WL.String_Maps;

package body Komnenos.Entities.Tables is

   package Table_Maps is
     new WL.String_Maps (Entity_Table_Access);

   Table_Map : Table_Maps.Map;

   ---------------
   -- Set_Table --
   ---------------

   procedure Set_Table
     (Path  : String;
      Table : not null access Entity_Table_Interface'Class)
   is
   begin
      if Table_Map.Contains (Path) then
         raise Constraint_Error with
           "komnenos: duplicate table path: " & Path;
      else
         Table_Map.Insert (Path, Entity_Table_Access (Table));
      end if;
   end Set_Table;

   -----------
   -- Table --
   -----------

   function Table
     (Path : String)
      return access Entity_Table_Interface'Class
   is
   begin
      if Table_Map.Contains (Path) then
         return Table_Map.Element (Path);
      else
         raise Constraint_Error with
           "komenenos: no such table: " & Path;
      end if;
   end Table;

end Komnenos.Entities.Tables;
