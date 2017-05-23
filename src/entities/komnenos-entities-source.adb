with Ada.Text_IO;

with Komnenos.Source;
with Komnenos.Themes;

with Komnenos.Entities.Visuals;
with Komnenos.Fragments.Rendering;
with Komnenos.Fragments.Source;
with Komnenos.UI;

package body Komnenos.Entities.Source is

   type Source_Cursor_Array is
     array (Cursor_Type) of Text_Position;

   type Root_Komnenos_Source_Entity is
     new Root_Source_Entity_Reference with
      record
         Top_Level              : Boolean;
         Changed                : Boolean;
         Compilation_Unit       : Source_Tree;
         Entity_Spec            : Source_Tree;
         Entity_Body            : Source_Tree;
         Entity_Tree            : Source_Tree;
         Update_Tree            : Source_Tree;
         Cursors                : Source_Cursor_Array := (others => 0);
         Edit_Tree              : Source_Tree;
         Left_Of_Tree           : Boolean := True;
         Buffer_Offset          : Text_Position_Offset := 0;
         Edit_Buffer            : Ada.Strings.Unbounded.Unbounded_String;
         Parse_Context          : Komnenos.Programs.Parser.Parse_Context;
         Buffer_Changed         : Boolean := False;
         Invalidated            : Boolean := False;
         New_Line_Before_Buffer : Boolean := False;
      end record;

   overriding function Top_Level
     (Entity : Root_Komnenos_Source_Entity)
      return Boolean
   is (Entity.Top_Level);

   overriding procedure Select_Entity
     (Entity : not null access Root_Komnenos_Source_Entity;
      Table  : access Entity_Table_Interface'Class;
      Parent : access Entity_Visual'Class;
      Visual : access Entity_Visual'Class;
      Offset : Natural);

   overriding procedure Render
     (Entity : not null access Root_Komnenos_Source_Entity;
      Visual : not null access Entity_Visual'Class);

   overriding function Get_Cursor
     (Entity : Root_Komnenos_Source_Entity;
      Cursor : Cursor_Type)
      return Text_Position;

   overriding procedure Set_Cursor
     (Entity   : in out Root_Komnenos_Source_Entity;
      Cursor   : Cursor_Type;
      Position : Text_Position);

   overriding function Get_Line
     (Entity   : Root_Komnenos_Source_Entity;
      Position : Text_Position)
      return Line_Number;

   overriding function Get_Column
     (Entity   : Root_Komnenos_Source_Entity;
      Position : Text_Position)
      return Column_Number;

   overriding function Get_Start_Of_Line
     (Entity : Root_Komnenos_Source_Entity;
      Line   : Line_Number)
      return Text_Position;

   overriding procedure Move_Cursor
     (Item     : in out Root_Komnenos_Source_Entity;
      Cursor   : Cursor_Type;
      Movement : Cursor_Movement_Type;
      Offset   : Text_Position_Offset);

   overriding procedure Insert_Text
     (Item     : in out Root_Komnenos_Source_Entity;
      Text     : String);

   overriding procedure Delete_Region
     (Entity     : in out Root_Komnenos_Source_Entity);

   overriding function Get_Region_Text
     (Entity : Root_Komnenos_Source_Entity;
      End_1  : Cursor_Type;
      End_2  : Cursor_Type)
      return String
   is ("");

   procedure Forward_Character
     (Item   : in out Root_Komnenos_Source_Entity'Class);

   procedure Backward_Character
     (Entity   : in out Root_Komnenos_Source_Entity'Class);

   procedure Insert_Character
     (Entity : in out Root_Komnenos_Source_Entity'Class;
      Ch     : Character);

   procedure Insert_New_Line
     (Entity : in out Root_Komnenos_Source_Entity'Class);

   --     overriding procedure Execute_Command
   --       (Item    : not null access Root_Komnenos_Source_Entity;
   --        Command : Komnenos.Commands.Komnenos_Command);

   procedure Finish_Edit
     (Item : not null access Root_Komnenos_Source_Entity'Class)
   is null;

   procedure Check_Token
     (Entity : not null access Root_Komnenos_Source_Entity'Class;
      Force  : in     Boolean;
      Echo   : out Boolean);

   procedure Scan_Token
     (Entity : not null access Root_Komnenos_Source_Entity'Class;
      Force  : in     Boolean;
      Length :    out Natural;
      Tok    :    out Komnenos.Tokens.Token);

   --     overriding procedure Insert_Character
   --       (Item    : in out Root_Komnenos_Source_Entity;
   --        Value   : Character;
   --        Updated : out Boolean);

   function Get_Key (File_Name : String;
                     Line      : Natural;
                     Column    : Natural;
                     Name      : String)
                     return String;

   procedure Log_Tree (Tree : Source_Tree)
     with Unreferenced;

   function Show_Buffer
     (Entity : Root_Komnenos_Source_Entity'Class)
      return String;

   ------------------------
   -- Backward_Character --
   ------------------------

   procedure Backward_Character
     (Entity : in out Root_Komnenos_Source_Entity'Class)
   is
      use Ada.Strings.Unbounded;
      use Komnenos.Layout;
      New_Position : Position renames Entity.Cursors (Point);
      Offset       : Position_Offset renames Entity.Buffer_Offset;
   begin

      if Offset > 0 then
         Offset := Offset - 1;
      else
         if Entity.Buffer_Changed then
            Entity.Finish_Edit;
         end if;

         declare
            use Komnenos.Programs;
            Next_Terminal : constant Program_Tree :=
                              Entity.Edit_Tree.Scan_Terminal (-1);
         begin
            if Next_Terminal = null or else not Next_Terminal.Is_Terminal then
               Offset := 0;
            else
               Entity.Edit_Tree := Next_Terminal;
               Entity.Left_Of_Tree := True;
               Offset := New_Position - Next_Terminal.Layout_Start_Position;
            end if;
         end;
      end if;
   end Backward_Character;

   -----------------
   -- Check_Token --
   -----------------

   procedure Check_Token
     (Entity : not null access Root_Komnenos_Source_Entity'Class;
      Force  : in     Boolean;
      Echo   : out Boolean)
   is
      use Ada.Strings.Unbounded;
      use Komnenos.Programs;
      use Komnenos.Programs.Parser;
      use Komnenos.Layout;

      Tok       : Komnenos.Tokens.Token;
      Last      : Natural;
      Got_Token : Boolean := False;
      Start     : Natural :=
                    Index_Non_Blank (Entity.Edit_Buffer);
      Offset    : Text_Position_Offset renames
                    Entity.Buffer_Offset;
   begin
      if Start = 0 then
         --           Entity.Edit_Buffer := Null_Unbounded_String;
         Echo := True;
         return;
      end if;

      Echo := True;

      while Length (Entity.Edit_Buffer) > 0 loop
         Scan_Token (Entity, Force, Last, Tok);
         exit when Last = 0;

         declare
            use type Komnenos.Tokens.Token;
            Text : constant String := Slice (Entity.Edit_Buffer, Start, Last);
         begin

            if Entity.Update_Tree /= null then
               if Tok = Entity.Update_Tree.Get_Token then
                  --  replace the text of the existing token
                  Ada.Text_IO.Put_Line
                    ("Replace: ["
                     & Entity.Update_Tree.Text
                     & "] -> ["
                     & Text
                     & "]");
                  Entity.Update_Tree.Fill (Text);
                  Entity.Update_Tree := null;
                  --  TODO: update semantics
               else
                  --  we edited a token and changed it to something else
                  --  right now we're fucked
                  null;
               end if;
            else
               --  creating a new token

               --                 Ada.Text_IO.Put_Line
               --                   ("Parsing into: "
               --                    & Komnenos.Trees.Cursors.Image
               --                      (Komnenos.Programs.Parser.Get_Cursor
               --                         (Entity.Parse_Context)));

               if Token_OK
                 (Tok, Komnenos.Source.No_Source_Position,
                  Entity.Parse_Context)
               then
                  Parse_Token
                    (Tok, Komnenos.Source.No_Source_Position,
                     Text, Entity.Parse_Context);
                  --                    Ada.Text_IO.Put_Line
                  --                      ("After parse: "
                  --                       & Komnenos.Trees.Cursors.Image
                  --                         (Komnenos.Programs.Parser.Get_Cursor
                  --                              (Entity.Parse_Context)));
                  Entity.Edit_Tree :=
                    Source_Tree
                      (Komnenos.Trees.Cursors.Get_Left_Tree
                         (Komnenos.Programs.Parser.Get_Cursor
                            (Entity.Parse_Context)));
                  Entity.Left_Of_Tree := False;
                  Entity.New_Line_Before_Buffer := False;
                  Got_Token := True;
               else
                  Ada.Text_IO.Put_Line ("parse failed");
                  Delete (Entity.Edit_Buffer,
                          Length (Entity.Edit_Buffer),
                          Length (Entity.Edit_Buffer));
                  Offset := Offset - 1;
                  Echo := False;
                  exit;
               end if;
            end if;

            --  remove the processed text from our buffer
            Ada.Strings.Unbounded.Delete (Entity.Edit_Buffer, 1, Last);
            Offset := Offset - Position_Offset (Last);
            Start := 1;
         end;
      end loop;

      if Got_Token and then
        not Komnenos.Programs.Parser.Is_Ambiguous
          (Entity.Parse_Context)
      then
         Komnenos.Entities.Visuals.Invalidate_Visuals (Entity.all);
         Echo := False;
         Ada.Text_IO.Put_Line ("after check_token: " & Entity.Show_Buffer);

      end if;

   end Check_Token;

   -----------------------------------
   -- Create_Komnenos_Source_Entity --
   -----------------------------------

   function Create_Komnenos_Source_Entity
     (Table            : not null access Entity_Table_Interface'Class;
      Name             : String;
      File_Name        : String;
      Class            : String;
      Line             : Natural;
      Column           : Natural;
      Top_Level        : Boolean;
      Compilation_Unit : Source_Tree;
      Entity_Spec      : Source_Tree;
      Entity_Body      : Source_Tree)
      return Entity_Reference
   is
      Key : constant String := Get_Key (File_Name, Line, Column, Name);
   begin
      if not Table.Exists (Key) then
         declare
            use type Source_Tree;
            Entity : Root_Komnenos_Source_Entity;
            Result : Entity_Reference;
         begin
            Entity.Create (Name, File_Name, Class, Line, Column);
            Entity.Top_Level := Top_Level;
            Entity.Compilation_Unit := Compilation_Unit;
            Entity.Grammar :=
              Komnenos.Trees.Properties.Get_Grammar (Compilation_Unit);
            Entity.Entity_Spec := Entity_Spec;
            Entity.Entity_Body := Entity_Body;
            Entity.Entity_Tree :=
              (if Entity_Body = null then Entity_Spec else Entity_Body);
            Komnenos.Programs.Parser.Initialise_Parse_Context
              (Context     => Entity.Parse_Context,
               Grammar     => Entity.Grammar,
               Root        => Entity.Entity_Tree,
               Interactive => True,
               Run_Actions => False);
            Komnenos.Programs.Parser.Set_Cursor
              (Entity.Parse_Context,
               Komnenos.Trees.Cursors.Left_Of_Tree (Entity.Entity_Tree));

            Result := new Root_Komnenos_Source_Entity'(Entity);
            Table.Add_Entity (Key, Result);
            return Result;
         end;
      else
         return Table.Get (Key);
      end if;
   end Create_Komnenos_Source_Entity;

   -------------------
   -- Delete_Region --
   -------------------

   overriding procedure Delete_Region
     (Entity     : in out Root_Komnenos_Source_Entity)
   is
      use Komnenos.Layout;
      Mark_Position  : constant Position :=
                         Entity.Get_Cursor (Mark);
      Point_Position : constant Position :=
                         Entity.Get_Cursor (Point);
      Start          : constant Position :=
                         Position'Min (Mark_Position, Point_Position);
      Finish         : constant Position :=
                         Position'Max (Mark_Position, Point_Position);
      Length         : constant Position := Finish - Start;
      Remaining      : Position := Length;
      Backward       : constant Boolean := Mark_Position < Point_Position;
      Needs_Render   : Boolean := False;
   begin

      Ada.Text_IO.Put_Line
        ("delete: start ="
         & Start'Img
         & "; finish ="
         & Finish'Img
         & "; length ="
         & Length'Img
         & "; buffer = "
         & Entity.Show_Buffer);

      if Backward then

         while Remaining > 0
           and then Remaining >= Entity.Buffer_Offset
         loop
            declare
               use Komnenos.Programs;
               New_Edit_Tree : constant Program_Tree :=
                                 Entity.Edit_Tree.Scan_Terminal (-1);
               New_Offset    : constant Position_Offset :=
                                 Entity.Edit_Tree.Layout_Start_Position
                                   - New_Edit_Tree.Layout_Start_Position;
               Ancestor      : Komnenos.Trees.Tree;
               Left, Right   : Komnenos.Trees.Tree;
               Deleted_Tree  : Program_Tree;
            begin
               New_Edit_Tree.Common_Ancestor
                 (Right          => Entity.Edit_Tree,
                  Ancestor       => Ancestor,
                  Left_Ancestor  => Left,
                  Right_Ancestor => Right);
               Deleted_Tree := Program_Tree (Right);
               Ada.Text_IO.Put_Line ("Deleting: " & Deleted_Tree.Image);
               Ancestor.Remove_Child (Right);
               Remaining := Remaining - Entity.Buffer_Offset;

               declare
                  New_Buffer : constant String :=
                                 New_Edit_Tree.Text;
                  Num_Spaces : constant Natural :=
                                 Natural
                                   (Entity.Edit_Tree.Layout_Start_Position
                                    - New_Edit_Tree.Layout_End_Position
                                    - 1);
                  Spaces     : constant String (1 .. Num_Spaces) :=
                                 (others => ' ');
               begin
                  Entity.Edit_Buffer :=
                    Ada.Strings.Unbounded.To_Unbounded_String
                      (New_Buffer & Spaces);
               end;

               Entity.Edit_Tree := New_Edit_Tree;
               Entity.Update_Tree := Entity.Edit_Tree;
               Entity.Buffer_Offset := New_Offset;

               Komnenos.Programs.Parser.Set_Cursor
                 (Entity.Parse_Context,
                  Komnenos.Trees.Cursors.Right_Of_Tree
                    (Entity.Edit_Tree));

               Ada.Text_IO.Put_Line
                 ("new buffer: " & Show_Buffer (Entity));

               Free (Deleted_Tree);

               Needs_Render := True;
            end;
         end loop;

         if Remaining > 0 then

            Ada.Strings.Unbounded.Delete
              (Entity.Edit_Buffer,
               Positive (Entity.Buffer_Offset - Remaining + 1),
               Natural (Entity.Buffer_Offset));
            Entity.Buffer_Offset := Entity.Buffer_Offset - Remaining;
            Entity.Update_Tree := Entity.Edit_Tree;

            Entity.Buffer_Changed := True;

            Ada.Text_IO.Put_Line
              ("after delete: "
               & Entity.Show_Buffer);
         end if;

         Entity.Cursors (Point) :=
           Entity.Cursors (Point) - Length;

         if Needs_Render then
            Komnenos.Entities.Visuals.Invalidate_Visuals (Entity);
         else
            Komnenos.Entities.Visuals.Delete_At_Cursor
              (Entity, Point, -Length);
         end if;

      end if;
   end Delete_Region;

   ---------------------
   -- Execute_Command --
   ---------------------

   --     overriding procedure Execute_Command
   --       (Item    : not null access Root_Komnenos_Source_Entity;
   --        Command : Komnenos.Commands.Komnenos_Command)
   --     is
   --        use Komnenos.Commands;
   --     begin
   --        case Command.Command is
   --           when No_Command =>
   --              null;
   --           when Move_Cursor_Command =>
   --              case Command.Units is
   --                 when By_Character =>
   --                    if Command.Offset > 0 then
   --                       for I in 1 .. Command.Offset loop
   --                          Item.Forward_Character;
   --                       end loop;
   --                    elsif Command.Offset < 0 then
   --                       for I in 1 .. -Command.Offset loop
   --                          Item.Backward_Character;
   --                       end loop;
   --                    else
   --                       null;
   --                    end if;
   --                 when By_Token =>
   --                    null;
   --                 when By_Line =>
   --                    null;
   --                 when By_Fragment =>
   --                    null;
   --              end case;
   --
   --           when Set_Cursor_Command =>
   --              Set_Cursor (Item, Command.New_Position);
   --
   --           when Insert_Character_Command =>
   --              Item.Insert_Character (Command.Ch);
   --
   --           when New_Line_Command =>
   --              Item.Insert_New_Line;
   --
   --        end case;
   --     end Execute_Command;

   ----------------------------
   -- Find_Entity_Containing --
   ----------------------------

   --     function Find_Entity_Containing
   --       (Table     : not null access Entity_Table_Interface'Class;
   --        Location  : File_Location)
   --        return Entity_Reference
   --     is
   --     begin
   --        return Table.Get_Reference (Location);
   --     end Find_Entity_Containing;

   -----------------------
   -- Forward_Character --
   -----------------------

   procedure Forward_Character
     (Item   : in out Root_Komnenos_Source_Entity'Class)
   is
      use Ada.Strings.Unbounded;
      use Komnenos.Layout;
      Tree         : Source_Tree renames
                       Item.Edit_Tree;
      Offset       : Position_Offset renames Item.Buffer_Offset;
      New_Position : constant Position := Item.Cursors (Point);
      Leaving      : Boolean := False;
      Token_Length : constant Natural :=
                       Length (Item.Edit_Buffer);
   begin
      Offset := Offset + 1;

      if Offset > Position (Token_Length) then
         Leaving := True;
      elsif Offset = Position (Token_Length)
        and then Tree.Is_Reserved_Terminal
      then
         Leaving := True;
      end if;

      if Leaving then
         if Item.Buffer_Changed then
            Item.Finish_Edit;
         end if;

         declare
            use Komnenos.Programs;
            Next_Terminal : constant Program_Tree :=
                              Tree.Scan_Terminal (1);
         begin
            if Next_Terminal.Layout_Start_Position >= New_Position then
               Item.Edit_Tree := Next_Terminal;
               Item.Left_Of_Tree := True;
               Item.Buffer_Offset :=
                 New_Position - Next_Terminal.Layout_Start_Position;
            end if;
         end;
      end if;

   end Forward_Character;

   ----------------
   -- Get_Column --
   ----------------

   overriding function Get_Column
     (Entity   : Root_Komnenos_Source_Entity;
      Position : Text_Position)
      return Column_Number
   is
      use Komnenos.Layout;
      use type Source_Tree;
      Terminal : constant Source_Tree :=
                   Entity.Entity_Tree.Find_Local_Node_At (Position);
   begin
      if Terminal = null then
         return 1;
      else
         return Terminal.Layout_Start_Column
           + Column_Offset (Position - Terminal.Layout_Start_Position);
      end if;
   end Get_Column;

   ----------------
   -- Get_Cursor --
   ----------------

   overriding function Get_Cursor
     (Entity : Root_Komnenos_Source_Entity;
      Cursor : Cursor_Type)
      return Text_Position
   is
   begin
      return Entity.Cursors (Cursor);
   end Get_Cursor;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (File_Name : String;
                     Line      : Natural;
                     Column    : Natural;
                     Name      : String)
                     return String
   is
   begin
      return File_Name & Integer'Image (-Line)
        & Integer'Image (-Column)
        & "-" & Name;
   end Get_Key;

   --------------
   -- Get_Line --
   --------------

   overriding function Get_Line
     (Entity   : Root_Komnenos_Source_Entity;
      Position : Text_Position)
      return Line_Number
   is
      use type Source_Tree;
      Terminal : constant Source_Tree :=
                   Entity.Entity_Tree.Find_Local_Node_At (Position);
   begin
      if Terminal = null then
         return 1;
      else
         return Terminal.Layout_Line;
      end if;
   end Get_Line;

   -----------------------
   -- Get_Start_Of_Line --
   -----------------------

   overriding function Get_Start_Of_Line
     (Entity : Root_Komnenos_Source_Entity;
      Line   : Line_Number)
      return Text_Position
   is
      use Komnenos.Layout;
      use type Source_Tree;
      Terminal : constant Source_Tree :=
                   Entity.Entity_Tree.Find_Local_First_Node_At_Line
                     (Line);
   begin
      if Terminal = null then
         return 1;
      else
         return Terminal.Layout_Start_Position
           - Position_Offset (Terminal.Layout_Start_Column) + 1;
      end if;
   end Get_Start_Of_Line;

   ----------------------
   -- Insert_Character --
   ----------------------

   procedure Insert_Character
     (Entity : in out Root_Komnenos_Source_Entity'Class;
      Ch     : Character)
   is
      use Komnenos.Layout;
      Tree           : Source_Tree renames
                         Entity.Edit_Tree;
      Offset         : Position_Offset renames Entity.Buffer_Offset;
      Buffer         : constant String :=
                         Ada.Strings.Unbounded.To_String (Entity.Edit_Buffer);
      New_Buffer     : constant String :=
                         Buffer (1 .. Natural (Offset))
                         & Ch
                         & Buffer (Natural (Offset) + 1 .. Buffer'Last);
      Complete       : Boolean;
      Have_Class     : Boolean;
      Unique         : Boolean;
      Class          : Komnenos.Tokens.Token_Class;
      Tok            : Komnenos.Tokens.Token;
      First          : Positive := 1;
      Last           : Natural;
   begin

      if Offset > 0 and then not Entity.Buffer_Changed then
         --  check to see if we can join this token to the active one
         Komnenos.Tokens.Scan
           (Frame      => Entity.Grammar.Frame,
            Text       => New_Buffer,
            Partial    => False,
            Complete   => Complete,
            Have_Class => Have_Class,
            Unique     => Unique,
            Class      => Class,
            Tok        => Tok,
            First      => First,
            Last       => Last,
            Token_OK   => null);

         if Last = New_Buffer'Last then
            --  we can update the current token
            Ada.Text_IO.Put_Line
              ("Editor: updating terminal "
               & Tree.Text);
            Entity.Update_Tree := Tree;
         else
            Entity.Update_Tree := null;
            if Last = Buffer'Last
              and then Natural (Offset) = Buffer'Last
            then
               --  starting a new token
               Entity.Edit_Buffer :=
                 Ada.Strings.Unbounded.Null_Unbounded_String;
               Offset := 0;
               Komnenos.Programs.Parser.Set_Cursor
                 (Entity.Parse_Context,
                  Komnenos.Trees.Cursors.Right_Of_Tree (Tree));
               Entity.Insert_Character (Ch);
               return;
            end if;
         end if;
      end if;

      --        Ada.Text_IO.Put_Line
      --          ("edit: [" & New_Buffer & "] at "
      --           & Komnenos.Trees.Cursors.Image
      --             (Komnenos.Programs.Parser.Get_Cursor
      --                  (Item.Parse_Context)));

      Entity.Edit_Buffer :=
        Ada.Strings.Unbounded.To_Unbounded_String (New_Buffer);
      Offset := Offset + 1;

      declare
         Echo : Boolean;
      begin
         Entity.Check_Token (False, Echo);
         if Echo then
            Komnenos.Entities.Visuals.Insert_At_Cursor
              (Entity, Point, (1 => Ch));

            if Ch = Character'Val (10) then
               declare
                  use Komnenos.Programs;
                  use Ada.Strings.Unbounded;
                  Edit_Tree                        : constant Source_Tree :=
                                                       Entity.Edit_Tree;
                  Start_Line                       : constant Program_Tree :=
                                                       Edit_Tree.Start_Of_Line;
                  Next_Line                        : constant Program_Tree :=
                                                       Edit_Tree.Scan_Terminal (1);
                  Before_Indent                    : constant Column_Number :=
                                                       Start_Line.Layout_Start_Column;
                  After_Indent                     : constant Column_Number :=
                                                       Next_Line.Layout_Start_Column;
                  New_Indent                       : constant Column_Number :=
                                                       Column_Number'Max
                                                         (Before_Indent, After_Indent);
                  Spaces                           : constant String
                    (1 .. Positive (New_Indent) - 1) :=
                                                       (others => ' ');
               begin
                  Ada.Text_IO.Put_Line
                    ("NL: before indent =" & Before_Indent'Img
                     & "; after indent =" & After_Indent'Img);

                  Entity.Buffer_Offset := Position_Offset (New_Indent);
                  Entity.Edit_Buffer :=
                    Entity.Edit_Buffer & Spaces;
                  Komnenos.Entities.Visuals.Insert_At_Cursor
                    (Entity, Point, Spaces);
               end;
            end if;
         end if;
      end;

      Entity.Buffer_Changed := True;

      Komnenos.UI.Current_UI.Program_Store.On_Edit
        (Entity.Compilation_Unit);

   end Insert_Character;

   ---------------------
   -- Insert_New_Line --
   ---------------------

   procedure Insert_New_Line
     (Entity : in out Root_Komnenos_Source_Entity'Class)
   is
      Echo : Boolean;
      pragma Unreferenced (Echo);
   begin

      --        if Entity.Buffer_Changed then
      --           --  FIXME
      --           Entity.Insert_Character (' ');
      --        end if;

      Entity.Update_Tree := null;
      Entity.Buffer_Changed := True;
      Entity.Edit_Buffer := Ada.Strings.Unbounded.Null_Unbounded_String;
      Entity.Buffer_Offset := 0;
      Entity.Left_Of_Tree := False;

      Entity.New_Line_Before_Buffer := True;

      declare
         use Komnenos.Layout;
         Edit_Tree     : constant Source_Tree :=
                           Entity.Edit_Tree;
         Before_Indent : constant Column_Number :=
                           Edit_Tree.Start_Of_Line.Layout_Start_Column;
         After_Indent  : constant Column_Number :=
                           Edit_Tree.Scan_Terminal (1).Layout_Start_Column;
         New_Indent    : constant Column_Number :=
                           Column_Number'Max (Before_Indent, After_Indent);
         Spaces        : constant String (1 .. Positive (New_Indent) - 1) :=
                           (others => ' ');
      begin
         Ada.Text_IO.Put_Line
           ("NL: before indent =" & Before_Indent'Img
            & "; after indent =" & After_Indent'Img);

         Entity.Buffer_Offset := Position_Offset (New_Indent);
         Entity.Edit_Buffer :=
           Ada.Strings.Unbounded.To_Unbounded_String
             (' ' & Spaces);
         Komnenos.Entities.Visuals.Insert_At_Cursor
           (Entity, Point, Character'Val (10) & Spaces);
      end;

      --        declare
      --           Cursor : constant Komnenos.Trees.Cursors.Cursor :=
      --                      Komnenos.Programs.Parser.Get_Cursor
      --                        (Item.Parse_Context);
      --           Program : Source_Tree;
      --        begin
      --           if not Komnenos.Trees.Cursors.Is_Off_Right (Cursor) then
      --              Program :=
      --                Source_Tree
      --                  (Komnenos.Trees.Cursors.Get_Right_Tree (Cursor));
      --              Program.Set_Vertical_Gap_Before (1);
      --              Komnenos.Entities.Visuals.Invalidate_Visuals (Item);
      --           end if;
      --        end;
   end Insert_New_Line;

   -----------------
   -- Insert_Text --
   -----------------

   overriding procedure Insert_Text
     (Item     : in out Root_Komnenos_Source_Entity;
      Text     : String)
   is
   begin
      for Ch of Text loop
         if Character'Pos (Ch) = 10 then
            Item.Insert_New_Line;
         else
            Root_Komnenos_Source_Entity'Class (Item).Insert_Character (Ch);
         end if;
      end loop;
   end Insert_Text;

   --------------
   -- Log_Tree --
   --------------

   procedure Log_Tree (Tree : Source_Tree) is

      procedure Log (T     : Source_Tree;
                     Start : Ada.Text_IO.Count);

      ---------
      -- Log --
      ---------

      procedure Log (T     : Source_Tree;
                     Start : Ada.Text_IO.Count)
      is
         use Ada.Text_IO;
         New_Start : Count := Start;
      begin
         if T.Name /= "" then
            Set_Col (Start);
            Put_Line (T.Image);
            New_Start := Start + 2;
         end if;
         for I in 1 .. T.Child_Count loop
            Log (T.Program_Child (I), New_Start);
         end loop;
      end Log;

   begin
      Log (Tree, 1);
   end Log_Tree;

   -----------------
   -- Move_Cursor --
   -----------------

   overriding procedure Move_Cursor
     (Item     : in out Root_Komnenos_Source_Entity;
      Cursor   : Cursor_Type;
      Movement : Cursor_Movement_Type;
      Offset   : Text_Position_Offset)
   is
      use Komnenos.Layout;
      Forward : constant Boolean := Offset >= 0;
      Count   : constant Count :=
                  Count (abs Offset);
      Current : Position renames Item.Cursors (Cursor);
   begin
      case Movement is
         when By_Character =>
            for I in 1 .. Count loop
               if Forward then
                  Current := Current + 1;
                  if Cursor = Point then
                     Item.Forward_Character;
                  end if;
               else
                  if Current > 0 then
                     Current := Current - 1;
                     if Cursor = Point then
                        Item.Backward_Character;
                     end if;
                  end if;
               end if;
            end loop;
         when By_Word =>
            null;
         when By_Line =>
            null;
         when By_Page =>
            null;
      end case;
   end Move_Cursor;

   ------------
   -- Render --
   ------------

   overriding procedure Render
     (Entity : not null access Root_Komnenos_Source_Entity;
      Visual : not null access Entity_Visual'Class)
   is
      use Komnenos.Layout;
      use type Source_Tree;
      Renderer : Komnenos.Rendering.Komnenos_Renderer :=
                   Komnenos.Fragments.Rendering.Fragment_Renderer
                     (Komnenos.Fragments.Fragment_Type (Visual),
                      Entity.Table);
      Program  : constant Source_Tree := Entity.Entity_Tree;
      Tree_Cursor : constant Komnenos.Trees.Cursors.Cursor :=
                      Komnenos.Programs.Parser.Get_Cursor
                        (Entity.Parse_Context);
      Edit_Line : Line_Number;
      Edit_Column : Column_Number;
      Partial  : constant String :=
                   Ada.Strings.Unbounded.To_String (Entity.Edit_Buffer);
      Edit_Tree : constant Source_Tree :=
                    Entity.Edit_Tree;
      Offset    : constant Position_Offset := Entity.Buffer_Offset;
      Left_Of_Tree : constant Boolean := Entity.Left_Of_Tree;
   begin
      Komnenos.Programs.Arrangements.Arrange
        (Item             => Program,
         Point            => Tree_Cursor,
         Partial          => Partial,
         Updating         => Entity.Update_Tree /= null,
         Partial_Line     => Edit_Line,
         Partial_Column   => Edit_Column,
         Line_Length      => Visual.Width / 8);

      Renderer.Set_Theme (Komnenos.Themes.Active_Theme);

      Visual.Clear;

      Komnenos.Programs.Arrangements.Render
        (Program          => Program,
         Renderer         => Renderer,
         Point            => Tree_Cursor,
         Partial          => Partial,
         Updating         => Entity.Update_Tree /= null,
         Partial_Line     => Edit_Line,
         Partial_Column   => Edit_Column);

      if Edit_Tree /= null then
         Ada.Text_IO.Put_Line
           ("Set cursor: edit-tree = " & Edit_Tree.Text
            & Position_Offset'Image (Edit_Tree.Layout_Start_Position)
            & " .."
            & Position_Offset'Image (Edit_Tree.Layout_End_Position)
            & "; side = "
            & (if Left_Of_Tree then "left" else "right")
            & "; offset ="
            & Offset'Img);

         if Left_Of_Tree then
            Visual.Set_Cursor
              (Point, Edit_Tree.Layout_Start_Position + Offset);
         else
            Visual.Set_Cursor
              (Point, Edit_Tree.Layout_End_Position + 1 + Offset);
         end if;
      end if;

   end Render;

   ----------------
   -- Scan_Token --
   ----------------

   procedure Scan_Token
     (Entity : not null access Root_Komnenos_Source_Entity'Class;
      Force  : in     Boolean;
      Length :    out Natural;
      Tok    :    out Komnenos.Tokens.Token)
   is
      use Ada.Strings.Unbounded;
      Complete    : Boolean;
      Have_Class  : Boolean;
      Unique      : Boolean;
      Class       : Komnenos.Tokens.Token_Class;
      First, Next : Natural := 1;
      Buffer      : String :=
                      To_String (Entity.Edit_Buffer);
   begin
      for I in Buffer'Range loop
         if Buffer (I) = Character'Val (10) then
            Buffer (I) := ' ';
         end if;
      end loop;

      Komnenos.Tokens.Scan
        (Frame      => Entity.Grammar.Frame,
         Text       => Buffer,
         Partial    => True,
         Complete   => Complete,
         Have_Class => Have_Class,
         Unique     => Unique,
         Class      => Class,
         Tok        => Tok,
         First      => First,
         Last       => Next,
         Token_OK   => null);

      if (Force and then Next > 0) or else
        (Have_Class and then Complete
         and then Next <= Ada.Strings.Unbounded.Length (Entity.Edit_Buffer))
      then
         Length := Next;
      else
         Length := 0;
      end if;

   end Scan_Token;

   -------------------
   -- Select_Entity --
   -------------------

   overriding procedure Select_Entity
     (Entity : not null access Root_Komnenos_Source_Entity;
      Table  : access Entity_Table_Interface'Class;
      Parent : access Entity_Visual'Class;
      Visual : access Entity_Visual'Class;
      Offset : Natural)
   is
      use Ada.Strings.Unbounded;
      use type Source_Tree;
      Fragment : constant Komnenos.Fragments.Fragment_Type :=
                   (if Visual = null
                    then Komnenos.Fragments.Source.New_Source_Fragment
                      (Title => To_String (Entity.Description),
                       Path  => To_String (Entity.File_Name))
                    else Komnenos.Fragments.Fragment_Type (Visual));
   begin
      Entity.Table := Entity_Table_Access (Table);
      Fragment.Set_Entity_Key (Key (Entity.all));
      Fragment.Set_Content (Entity);
      Komnenos.Entities.Visuals.Bind_Visual (Fragment, Entity);

      Root_Komnenos_Source_Entity'Class (Entity.all).Render (Fragment);

      if Visual = null then
         Komnenos.UI.Current_UI.Place_Fragment
           (Parent, Offset, Fragment);
      end if;

      Fragment.Rendered;

   end Select_Entity;

   ----------------
   -- Set_Cursor --
   ----------------

   overriding procedure Set_Cursor
     (Entity   : in out Root_Komnenos_Source_Entity;
      Cursor   : Cursor_Type;
      Position : Text_Position)
   is
   begin
      Entity.Cursors (Cursor) := Position;

      if Cursor = Point then
         declare
            use Ada.Strings.Unbounded;
            use Komnenos.Layout;
            use Komnenos.Programs;
            Terminal     : constant Program_Tree :=
                             Entity.Entity_Tree.Find_Local_Node_At
                               (Location => Position);
            Tree_Cursor  : Komnenos.Trees.Cursors.Cursor;
         begin
            if Terminal /= null
              and then Terminal /= Entity.Edit_Tree
            then
               Entity.Left_Of_Tree := True;
               if Terminal.Layout_End_Position < Position then
                  Entity.Edit_Buffer := Null_Unbounded_String;
                  Tree_Cursor :=
                    Komnenos.Trees.Cursors.Right_Of_Tree (Terminal);
                  Entity.Edit_Tree := Terminal;

                  if Terminal.Layout_End_Position = Position - 1 then
                     Entity.Edit_Buffer := To_Unbounded_String (Terminal.Text);
                     Entity.Buffer_Offset :=
                       Position_Offset (Length (Entity.Edit_Buffer));
                  else
                     Entity.Buffer_Offset := 0;
                  end if;
               else
                  Entity.Edit_Tree := Terminal;
                  Tree_Cursor :=
                    Komnenos.Trees.Cursors.Left_Of_Tree (Terminal);
                  Entity.Edit_Buffer := To_Unbounded_String (Terminal.Text);
                  Entity.Update_Tree := Entity.Edit_Tree;
                  Entity.Buffer_Offset :=
                    Position - Terminal.Layout_Start_Position;
               end if;

               Komnenos.Programs.Parser.Set_Cursor
                 (Entity.Parse_Context, Tree_Cursor);

            end if;
         end;
      end if;
   end Set_Cursor;

   ---------------------
   -- Set_Entity_Body --
   ---------------------

   procedure Set_Entity_Body
     (Entity      : Entity_Reference;
      Entity_Body : Source_Tree)
   is
   begin
      Root_Komnenos_Source_Entity (Entity.all).Entity_Body := Entity_Body;
   end Set_Entity_Body;

   -----------------
   -- Show_Buffer --
   -----------------

   function Show_Buffer
     (Entity : Root_Komnenos_Source_Entity'Class)
      return String
   is
      use Ada.Strings.Unbounded;
      Buffer : constant String := To_String (Entity.Edit_Buffer);
      Offset : constant Natural :=
                 Natural (Entity.Buffer_Offset);
   begin
      return "["
        & Buffer (1 .. Offset)
        & "|"
        & Buffer (Offset + 1 .. Buffer'Last)
        & "]";
   end Show_Buffer;

   -------------------
   -- Syntax_Entity --
   -------------------

   function Syntax_Entity
     (Table  : not null access Entity_Table_Interface'Class;
      Entity : Entity_Reference)
      return Entity_Reference
   is
      Program : constant Source_Tree :=
                  Root_Komnenos_Source_Entity (Entity.all).Compilation_Unit;
      Grammar : constant Komnenos.Grammars.Komnenos_Grammar :=
                  Komnenos.Trees.Properties.Get_Grammar (Program);
      Syntax  : constant Source_Tree :=
                  Grammar.Get_EBNF_Tree;
   begin
      return Create_Komnenos_Source_Entity
        (Table            => Table,
         Name             => Grammar.Name,
         File_Name        => Syntax.Source_File_Name,
         Class            => "syntax",
         Line             => 1,
         Column           => 1,
         Top_Level        => False,
         Compilation_Unit => Syntax,
         Entity_Spec      => Syntax,
         Entity_Body      => null);
   end Syntax_Entity;

end Komnenos.Entities.Source;
