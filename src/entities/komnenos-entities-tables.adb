with WL.String_Maps;

package body Komnenos.Entities.Tables is

   package Table_Maps is
     new WL.String_Maps (Entity_Table_Access);

   Table_Map          : Table_Maps.Map;
   Local_Active_Table : Entity_Table_Access;

   ------------------
   -- Active_Table --
   ------------------

   function Active_Table
     return access Entity_Table_Interface'Class
   is
   begin
      return Local_Active_Table;
   end Active_Table;

   ---------------
   -- New_Table --
   ---------------

   procedure New_Table
     (Name  : String;
      Store : not null access Program_Store_Interface'Class)
   is
      Rec : Entity_Table;
      Table : Entity_Table_Access;
   begin
      Rec.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Rec.Store := Program_Store_Access (Store);
      Table := new Entity_Table'(Rec);
      Set_Table (Name, Table);
   end New_Table;

   ----------------------
   -- Set_Active_Table --
   ----------------------

   procedure Set_Active_Table
     (Table : not null access Entity_Table_Interface'Class)
   is
   begin
      Local_Active_Table := Entity_Table_Access (Table);
   end Set_Active_Table;

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
      Local_Active_Table := Entity_Table_Access (Table);
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
