package Komnenos.Entities.Text_Entities is

   type Root_Text_Entity_Reference is
     new Root_Entity_Reference with private;

   type Text_Entity_Reference is
     access all Root_Text_Entity_Reference'Class;

   function Create_Text_Entity
     (Name      : String;
      File_Name : String;
      Contents  : String)
      return Entity_Reference;

private

   type Root_Text_Entity_Reference is
     new Root_Entity_Reference with
      record
         Name      : Ada.Strings.Unbounded.Unbounded_String;
         File_Name : Ada.Strings.Unbounded.Unbounded_String;
         Contents  : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding procedure Select_Entity
     (Entity : not null access Root_Text_Entity_Reference;
      Table  : access Entity_Table_Interface'Class;
      Parent : access Entity_Visual'Class;
      Visual : access Entity_Visual'Class;
      Offset : Natural);

   overriding procedure Render
     (Entity : not null access Root_Text_Entity_Reference;
      Visual : not null access Entity_Visual'Class);

end Komnenos.Entities.Text_Entities;
