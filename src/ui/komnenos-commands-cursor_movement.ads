package Komnenos.Commands.Cursor_Movement is

   function Move_By_Line_Command
     (Offset : Text_Offset)
      return Root_Komnenos_Command'Class;

   function Move_By_Character_Command
     (Offset : Text_Offset)
      return Root_Komnenos_Command'Class;

   function Move_To_Position_Command
     (New_Position : Text_Position)
      return Root_Komnenos_Command'Class;

end Komnenos.Commands.Cursor_Movement;
