with Komnenos.Source;

package Komnenos.Entities.Source is

   type Root_Source_Entity_Reference is
     abstract new Root_Entity_Reference with private;

   procedure Initialize
     (Entity           : in out Root_Source_Entity_Reference'Class;
      Table            : not null access Entity_Table_Interface'Class;
      Name             : String;
      File_Name        : String;
      Class            : String;
      Line             : Line_Number;
      Column           : Column_Number;
      Top_Level        : Boolean;
      Compilation_Unit : not null access
        Komnenos.Source.Source_Tree_Interface'Class;
      Entity_Spec      : not null access
        Komnenos.Source.Source_Tree_Interface'Class;
      Entity_Body      : access
        Komnenos.Source.Source_Tree_Interface'Class);

   procedure Set_Entity_Body
     (Entity      : Entity_Reference;
      Entity_Body : not null access
        Komnenos.Source.Source_Tree_Interface'Class);

   function Syntax_Entity
     (Table  : not null access Entity_Table_Interface'Class;
      Entity : Entity_Reference)
      return Entity_Reference;

private

   type Root_Source_Entity_Reference is
     abstract new Root_Entity_Reference with
      record
         Name      : Ada.Strings.Unbounded.Unbounded_String;
         File_Name : Ada.Strings.Unbounded.Unbounded_String;
         Line      : Natural;
         Column    : Natural;
      end record;

   overriding procedure Select_Entity
     (Entity : not null access Root_Source_Entity_Reference;
      Table  : access Entity_Table_Interface'Class;
      Parent : access Entity_Visual'Class;
      Visual : access Entity_Visual'Class;
      Offset : Natural);

   overriding procedure Render
     (Entity : not null access Root_Source_Entity_Reference;
      Visual : not null access Entity_Visual'Class);

end Komnenos.Entities.Source;
