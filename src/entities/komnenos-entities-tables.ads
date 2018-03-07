package Komnenos.Entities.Tables is

   function Table
     (Path : String)
      return access Entity_Table_Interface'Class;

   procedure Set_Table
     (Path  : String;
      Table : not null access Entity_Table_Interface'Class);

   procedure New_Table
     (Name  : String;
      Store : not null access Program_Store_Interface'Class);

   function Active_Table
     return access Entity_Table_Interface'Class;

   procedure Set_Active_Table
     (Table : not null access Entity_Table_Interface'Class);

end Komnenos.Entities.Tables;
