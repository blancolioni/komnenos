with Aqua.Objects;

package Komnenos.Entities.Aqua_Entities is

   type Root_Aqua_Object is
     new Aqua.External_Object_Interface
     and Aqua.Objects.Object_Interface
   with private;

   overriding function Get_Property
     (Object : in out Root_Aqua_Object;
      Name   : in String)
      return Aqua.Word;

   overriding function Name
     (Object : Root_Aqua_Object)
      return String
   is ("[UI]");

   overriding function Text
     (Object : Root_Aqua_Object)
      return String
   is ("[UI]");

   overriding function Show
     (Object         : Root_Aqua_Object;
      Recursive_Show : access
        function (Value : Aqua.Word) return String)
      return String
   is ("[UI]");

--     procedure Define
--       (Object       : in out Root_Aqua_Object'Class;
--        Declaration  : Source_Tree;
--        Defined_Name : Source_Tree);

   type Komnenos_Aqua_Object is access all Root_Aqua_Object'Class;

   procedure Create_Aqua_Object
     (Table : access Entity_Table_Interface'Class);

   function Get_Aqua_Object
     return Komnenos_Aqua_Object;

private

   type Root_Aqua_Object is
     new Aqua.Objects.Root_Object_Type with
      record
         Table : access Entity_Table_Interface'Class;
      end record;

end Komnenos.Entities.Aqua_Entities;
