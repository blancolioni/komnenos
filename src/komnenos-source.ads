with Aqua;
with Aqua.Execution;

package Komnenos.Source is

   type Source_Tree_Interface is interface
     and Aqua.External_Object_Interface;

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

   function Source_Root
     (Source : not null access Source_Tree_Interface)
      return access Source_Tree_Interface'Class
      is abstract;

   type Source_Tree is access all Source_Tree_Interface'Class;

   function Get
     (Context : in out Aqua.Execution.Execution_Interface'Class;
      Value   : Aqua.Word)
      return Source_Tree
   is (if Aqua.Is_External_Reference (Value)
       then Source_Tree (Context.To_External_Object (Value))
       else null);

end Komnenos.Source;
