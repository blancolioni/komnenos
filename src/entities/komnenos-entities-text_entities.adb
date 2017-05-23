with Komnenos.Entities.Visuals;
with Komnenos.Fragments.Notes;

with Komnenos.Themes;
with Komnenos.UI;

package body Komnenos.Entities.Text_Entities is

   ------------------------
   -- Create_Text_Entity --
   ------------------------

   function Create_Text_Entity
     (Name      : String;
      File_Name : String;
      Contents  : String)
      return Entity_Reference
   is
      use Ada.Strings.Unbounded;
      Entity : Root_Text_Entity_Reference;
      Result : Entity_Reference;
   begin
      Entity.Create
        (Identifier   => Name,
         Class_Name   => "text",
         Display_Text => Name,
         Description  => "Text entity");
      Entity.File_Name := To_Unbounded_String (File_Name);
      Entity.Contents := To_Unbounded_String (Contents);
      Result := new Root_Text_Entity_Reference'(Entity);
      return Result;
   end Create_Text_Entity;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Entity : not null access Root_Text_Entity_Reference;
      Visual : not null access Entity_Visual'Class)
   is
   begin
      Visual.Put_Line
        (Text     => Ada.Strings.Unbounded.To_String (Entity.Contents),
         Style    => Komnenos.Themes.Active_Theme.Default_Style);
   end Render;

   -------------------
   -- Select_Entity --
   -------------------

   overriding procedure Select_Entity
     (Entity : not null access Root_Text_Entity_Reference;
      Table  : access Entity_Table_Interface'Class;
      Parent : access Entity_Visual'Class;
      Visual : access Entity_Visual'Class;
      Offset : Natural)
   is
      use Ada.Strings.Unbounded;
      Fragment : constant Komnenos.Fragments.Fragment_Type :=
                   (if Visual = null
                    then Komnenos.Fragments.Notes.New_Note_Fragment
                      (Komnenos.Entities.Entity_Reference (Entity))
                    else Komnenos.Fragments.Fragment_Type (Visual));
   begin
      Entity.Table := Entity_Table_Access (Table);
      Fragment.Set_Content (Entity);
      Komnenos.Entities.Visuals.Bind_Visual (Fragment, Entity);

      Root_Text_Entity_Reference'Class (Entity.all).Render (Fragment);

      if Visual = null then
         Komnenos.UI.Current_UI.Place_Fragment
           (Parent, Pixel_Position (Offset), Fragment);
      end if;

      Fragment.Rendered;

   end Select_Entity;

end Komnenos.Entities.Text_Entities;
