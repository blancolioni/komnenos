package Komnenos.Entities.Visuals is

   procedure Bind_Visual
     (Visual : not null access Entity_Visual'Class;
      Entity : not null access Root_Entity_Reference'Class);

   procedure Unbind_Visual
     (Visual : not null access Entity_Visual'Class);

   procedure Invalidate_Visuals
     (Entity : in out Root_Entity_Reference'Class);

   procedure Update_Cursor
     (Entity   : in out Root_Entity_Reference'Class;
      Cursor   : Cursor_Type;
      Position : Text_Position);

   procedure Insert_At_Cursor
     (Entity : in out Root_Entity_Reference'Class;
      Cursor : Cursor_Type;
      Text   : String);

   procedure Delete_At_Cursor
     (Entity : in out Root_Entity_Reference'Class;
      Cursor : Cursor_Type;
      Offset : Text_Offset);

end Komnenos.Entities.Visuals;
