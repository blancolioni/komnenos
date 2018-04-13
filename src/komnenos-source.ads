package Komnenos.Source is

   type Source_Tree_Interface is interface;

   function Source_File_Name
     (Source : Source_Tree_Interface)
      return String
      is abstract;

   function Source_Line
     (Source : Source_Tree_Interface)
      return Line_Number
      is abstract;

   function Source_Column
     (Source : Source_Tree_Interface)
      return Column_Number
      is abstract;

   function Source_Position
     (Source : Source_Tree_Interface)
      return Text_Position
      is abstract;

   function Source_Root
     (Source : not null access Source_Tree_Interface)
      return access Source_Tree_Interface'Class
      is abstract;

   type Source_Tree is access all Source_Tree_Interface'Class;

end Komnenos.Source;
