with Ada.Strings.Unbounded;

package body Komnenos.Commands.Insert_Delete is

   type Root_Delete_Command is
     abstract new Root_Komnenos_Command with
      record
         Deleted_Text : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   type Delete_From_Cursor_Command is
     new Root_Delete_Command with
      record
         Movement : Text_Movement;
      end record;

   overriding procedure Execute
     (Command : in out Delete_From_Cursor_Command);

   overriding procedure Undo
     (Command : in out Delete_From_Cursor_Command) is null;

   type Delete_Region_Command is
     new Root_Delete_Command with
      record
         From, To : Text_Position;
      end record;

   overriding procedure Execute
     (Command : in out Delete_Region_Command) is null;

   overriding procedure Undo
     (Command : in out Delete_Region_Command) is null;

   type Root_Insert_Command is
     new Root_Komnenos_Command with
      record
         Text : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding procedure Execute
     (Command : in out Root_Insert_Command);

   overriding procedure Undo
     (Command : in out Root_Insert_Command) is null;

   ---------------------------
   -- Delete_Text_At_Cursor --
   ---------------------------

   function Delete_Text_At_Cursor
     (Forward : Boolean;
      Count   : Text_Offset)
      return Root_Komnenos_Command'Class
   is
   begin
      return Result : Delete_From_Cursor_Command do
         Result.Movement.Unit := Character_Unit;
         if Forward then
            Result.Movement.Offset := Count;
         else
            Result.Movement.Offset := -Count;
         end if;
      end return;
   end Delete_Text_At_Cursor;

   -------------------------
   -- Delete_Text_Command --
   -------------------------

   function Delete_Text_Command
     (From, To : Text_Position)
      return Root_Komnenos_Command'Class
   is
   begin
      return Result : Delete_Region_Command do
         Result.From := From;
         Result.To := To;
      end return;
   end Delete_Text_Command;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command : in out Delete_From_Cursor_Command)
   is
      use Komnenos.Entities;
      Old_Mark : constant Text_Position := Command.Entity.Get_Cursor (Mark);
      New_Mark : constant Text_Position :=
                   Text_Position'Max
                     (Command.Entity.Get_Cursor (Point)
                      + Command.Movement.Offset,
                      0);
   begin
      Command.Entity.Set_Cursor (Mark, New_Mark);
      Command.Entity.Delete_Region;
      Command.Entity.Set_Cursor (Mark, Old_Mark);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Command : in out Root_Insert_Command)
   is
      use Ada.Strings.Unbounded;
   begin
      Command.Entity.Insert_Text
        (To_String (Command.Text));
   end Execute;

   ------------------------------
   -- Insert_Character_Command --
   ------------------------------

   function Insert_Character_Command
     (Ch : Character)
      return Root_Komnenos_Command'Class
   is
   begin
      return Result : Root_Insert_Command do
         Result.Text := Ada.Strings.Unbounded.To_Unbounded_String ((1 => Ch));
      end return;
   end Insert_Character_Command;

end Komnenos.Commands.Insert_Delete;
