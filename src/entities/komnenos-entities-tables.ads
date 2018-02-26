package Komnenos.Entities.Tables is

   function Table
     (Path : String)
      return access Entity_Table_Interface'Class;

   procedure Set_Table
     (Path  : String;
      Table : not null access Entity_Table_Interface'Class);

end Komnenos.Entities.Tables;
