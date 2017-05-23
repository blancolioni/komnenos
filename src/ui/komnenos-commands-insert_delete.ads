package Komnenos.Commands.Insert_Delete is

   function Insert_Character_Command
     (Ch : Character)
      return Root_Komnenos_Command'Class;

   function Delete_Text_Command
     (From, To : Text_Position)
      return Root_Komnenos_Command'Class;

   function Delete_Text_At_Cursor
     (Forward : Boolean;
      Count   : Text_Offset_Range)
      return Root_Komnenos_Command'Class;

end Komnenos.Commands.Insert_Delete;
