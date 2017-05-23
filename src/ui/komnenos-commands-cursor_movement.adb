package body Komnenos.Commands.Cursor_Movement is

   type Root_Move_Cursor_Command is
     abstract new Root_Komnenos_Command with
      record
         Old_Position : Text_Position;
      end record;

   overriding function Is_Change
     (Command : Root_Move_Cursor_Command)
      return Boolean
   is (False);

   overriding procedure Undo
     (Command : in out Root_Move_Cursor_Command);

   type Move_Cursor_Relative_Command is
     new Root_Move_Cursor_Command with
      record
         Movement : Text_Movement;
      end record;

   overriding procedure Execute
     (Command : in out Move_Cursor_Relative_Command);

   overriding function Show
     (Command : Move_Cursor_Relative_Command) return String
   is ("[]");

   type Move_Cursor_Absolute_Command is
     new Root_Move_Cursor_Command with
      record
         New_Position : Text_Position;
      end record;

   overriding procedure Execute
     (Command : in out Move_Cursor_Absolute_Command);

   overriding function Show
     (Command : Move_Cursor_Absolute_Command) return String
   is ("[]");

   function Move_Command
     (Movement   : Text_Movement)
      return Root_Komnenos_Command'Class;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command : in out Move_Cursor_Relative_Command)
   is
   begin
      Command.Entity.Move_Cursor
        (Cursor => Point,
         Movement => Command.Movement);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command : in out Move_Cursor_Absolute_Command)
   is
   begin
      Command.Entity.Set_Cursor (Point,
                                 Command.New_Position);
   end Execute;

   -------------------------------
   -- Move_By_Character_Command --
   -------------------------------

   function Move_By_Character_Command
     (Offset : Text_Offset)
      return Root_Komnenos_Command'Class
   is
   begin
      return Move_Command ((Character_Unit, Offset));
   end Move_By_Character_Command;

   --------------------------
   -- Move_By_Line_Command --
   --------------------------

   function Move_By_Line_Command
     (Offset : Text_Offset)
      return Root_Komnenos_Command'Class
   is
   begin
      return Move_Command
        ((Line_Unit, Offset));
   end Move_By_Line_Command;

   ------------------
   -- Move_Command --
   ------------------

   function Move_Command
     (Movement   : Text_Movement)
      return Root_Komnenos_Command'Class
   is
   begin
      return Result : Move_Cursor_Relative_Command do
         Result.Movement := Movement;
      end return;
   end Move_Command;

   ------------------------------
   -- Move_To_Position_Command --
   ------------------------------

   function Move_To_Position_Command
     (New_Position : Text_Position)
      return Root_Komnenos_Command'Class
   is
   begin
      return Result : Move_Cursor_Absolute_Command do
         Result.New_Position := New_Position;
      end return;
   end Move_To_Position_Command;

   ----------
   -- Undo --
   ----------

   overriding procedure Undo
     (Command : in out Root_Move_Cursor_Command)
   is
   begin
      Command.Entity.Set_Cursor
        (Point, Command.Old_Position);
   end Undo;

end Komnenos.Commands.Cursor_Movement;
