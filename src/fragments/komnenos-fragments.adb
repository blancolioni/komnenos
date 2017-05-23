with Ada.Characters.Latin_1;
with Ada.Directories;

with Komnenos.Commands.Cursor_Movement;
with Komnenos.Themes;

package body Komnenos.Fragments is

   type Null_Text_Display is
     new Text_Editor_Display with null record;

   overriding procedure Insert_At_Cursor
     (Null_Display : in out Null_Text_Display;
      Text         : in     String)
   is null;

   overriding procedure Delete_From_Cursor
     (Null_Display : in out Null_Text_Display;
      Offset       : in     Text_Offset)
   is null;

   overriding procedure Set_Cursor
     (Null_Display : in out Null_Text_Display;
      New_Position : Text_Position)
   is null;

   overriding procedure Set_Content
     (Null_Display : in out Null_Text_Display;
      New_Content  : String)
   is null;

   overriding procedure Render_Fragment
     (Null_Display : in out Null_Text_Display;
      Fragment     : not null access Fragments.Root_Fragment_Type'Class)
   is null;

   Local_Null_Text_Display : aliased Null_Text_Display;

   function Get_Style_Info
     (Fragment : Root_Fragment_Type;
      Offset   : Positive)
      return Style_Info;

   function New_Fragment
     return access Komnenos.Session_Objects.Session_Object_Interface'Class;

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Fragment : in out Root_Fragment_Type) is
      pragma Unreferenced (Fragment);
   begin
      null;
   end Adjust;

   -----------------------
   -- Background_Colour --
   -----------------------

   function Background_Colour
     (Fragment : Root_Fragment_Type)
      return Komnenos.Colours.Komnenos_Colour
   is
   begin
      return Fragment.Background_Colour;
   end Background_Colour;

   -------------------
   -- Border_Colour --
   -------------------

   function Border_Colour
     (Fragment : Root_Fragment_Type)
      return Komnenos.Colours.Komnenos_Colour
   is
   begin
      return Fragment.Border_Colour;
   end Border_Colour;

   -----------
   -- Clear --
   -----------

   overriding procedure Clear (Fragment : in out Root_Fragment_Type) is
   begin
      Fragment.Lines.Clear;
      Fragment.Display.Set_Content ("");
      Fragment.Lines.Append (new Line_Info);
   end Clear;

   ------------------------
   -- Delete_From_Cursor --
   ------------------------

   overriding procedure Delete_From_Cursor
     (Fragment  : in out Root_Fragment_Type;
      Offset    : Text_Offset)
   is
   begin
      Fragment.Display.Delete_From_Cursor (Offset);
   end Delete_From_Cursor;

   --------------
   -- Editable --
   --------------

   function Editable
     (Fragment : Root_Fragment_Type)
      return Boolean
   is
   begin
      return Fragment.Editable;
   end Editable;

   ----------------
   -- Entity_Key --
   ----------------

   function Entity_Key (Fragment : Root_Fragment_Type'Class)
                        return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Fragment.Key);
   end Entity_Key;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Fragment : in out Root_Fragment_Type'Class;
      Command  : in out Komnenos.Commands.Root_Komnenos_Command'Class)
   is
   begin
      Fragment.Commands.Execute
        (Command, Fragment.Content);
   end Execute;

   ---------------
   -- File_Name --
   ---------------

   function File_Name
     (Fragment : Root_Fragment_Type'Class)
      return String
   is
      Path : constant String :=
               Ada.Strings.Unbounded.To_String (Fragment.Path);
   begin
      return Ada.Directories.Simple_Name (Path);
   end File_Name;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Fragment : in out Root_Fragment_Type) is
      pragma Unreferenced (Fragment);
   begin
      null;
   end Finalize;

   -----------------------
   -- Foreground_Colour --
   -----------------------

   function Foreground_Colour
     (Fragment : Root_Fragment_Type)
      return Komnenos.Colours.Komnenos_Colour
   is
   begin
      return Fragment.Foreground_Colour;
   end Foreground_Colour;

   -----------------
   -- From_Config --
   -----------------

   overriding procedure From_Config
     (Fragment : not null access Root_Fragment_Type;
      Config   : Tropos.Configuration)
   is
   begin
      Fragment.Default_Style :=
        Komnenos.Themes.Active_Theme.Style (Config.Get ("default_style"));
      Fragment.Layout_Rec := From_Config (Config.Child ("rectangle"));
      Fragment.Path :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (String'(Config.Get ("path")));
      Fragment.Title :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (String'(Config.Get ("title")));
      Fragment.Key :=
        Ada.Strings.Unbounded.To_Unbounded_String
          (String'(Config.Get ("key")));
      Fragment.Editable := Config.Get ("editable");
      Fragment.Background_Colour :=
        Komnenos.Colours.From_String (Config.Get ("background"));
      Fragment.Foreground_Colour :=
        Komnenos.Colours.From_String (Config.Get ("foreground"));
      Fragment.Border_Colour :=
        Komnenos.Colours.From_String (Config.Get ("border"));
   end From_Config;

   --------------
   -- Get_Link --
   --------------

   function Get_Link
     (Fragment : Root_Fragment_Type;
      Offset   : Positive)
      return Komnenos.Entities.Entity_Reference
   is
   begin
      return Get_Style_Info (Fragment, Offset).Reference;
   end Get_Link;

   ---------------
   -- Get_Style --
   ---------------

   function Get_Style
     (Fragment : Root_Fragment_Type;
      State    : Element_State;
      Offset   : Positive)
      return Komnenos.Styles.Komnenos_Style
   is
      Style : Komnenos.Styles.Komnenos_Style;
      Start, Finish : Natural;
   begin
      Root_Fragment_Type'Class (Fragment).Get_Style
        (State, Offset, Style, Start, Finish);
      return Style;
   end Get_Style;

   ---------------
   -- Get_Style --
   ---------------

   procedure Get_Style
     (Fragment : Root_Fragment_Type;
      State    : Element_State;
      Offset   : Positive;
      Style    : out Komnenos.Styles.Komnenos_Style;
      Start    : out Natural;
      Finish   : out Natural)
   is
      use Ada.Strings.Unbounded;
      use Komnenos.Styles;
      Line       : Positive := 1;
      Line_Start : Positive := 1;
      Last_Line  : constant Natural :=
                     Natural (Fragment.Lines.Length);
   begin
      while Line <= Last_Line loop
         declare
            This_Length : constant Natural :=
                            Length (Fragment.Lines (Line).Text) + 1;
         begin
            exit when This_Length + Line_Start > Offset;
            Line_Start := Line_Start + This_Length;
         end;
         Line := Line + 1;
      end loop;

      if Line <= Last_Line then
         declare
            use Komnenos.Themes;
            Line_Offset  : Natural := 0;
            Style_Offset : constant Natural := Offset - Line_Start;
         begin
            for Info of Fragment.Lines (Line).Styles loop
               if Line_Offset + Info.Length > Style_Offset then
                  Start := Line_Start + Line_Offset;
                  Finish := Line_Start + Line_Offset + Info.Length;
                  Style := Info.Styles (State);
                  if State /= Normal and then Style = Null_Style then
                     Style := Info.Styles (Normal);
                  end if;
                  if Style = Null_Style then
                     Style := Fragment.Default_Style;
                  end if;
                  return;
               end if;
               Line_Offset := Line_Offset + Info.Length;
            end loop;
         end;
      end if;

      Style := Komnenos.Styles.Null_Style;
      Start := 0;
      Finish := 0;

   end Get_Style;

   --------------------
   -- Get_Style_Info --
   --------------------

   function Get_Style_Info
     (Fragment : Root_Fragment_Type;
      Offset   : Positive)
      return Style_Info
   is
      use Ada.Strings.Unbounded;
      Line       : Positive := 1;
      Line_Start : Positive := 1;
      Last_Line  : constant Natural :=
                     Natural (Fragment.Lines.Length);
   begin
      while Line <= Last_Line loop
         declare
            This_Length : constant Natural :=
                            Length (Fragment.Lines (Line).Text) + 1;
         begin
            exit when This_Length + Line_Start > Offset;
            Line_Start := Line_Start + This_Length;
         end;
         Line := Line + 1;
      end loop;

      if Line <= Last_Line then
         declare
            Line_Offset  : Natural := 0;
            Style_Offset : constant Natural := Offset - Line_Start;
         begin
            for Info of Fragment.Lines (Line).Styles loop
               Line_Offset := Line_Offset + Info.Length;
               if Line_Offset > Style_Offset then
                  return Info;
               end if;
            end loop;
         end;
      end if;

      return (0, (others => Fragment.Default_Style), null,
              Ada.Strings.Unbounded.Null_Unbounded_String);
   end Get_Style_Info;

   ------------------
   -- Get_Tool_Tip --
   ------------------

   function Get_Tool_Tip
     (Fragment : Root_Fragment_Type;
      Position : Text_Position)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String
        (Get_Style_Info (Fragment, Natural (Position) + 1).Tool_Tip);
   end Get_Tool_Tip;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (Fragment : in out Root_Fragment_Type) is
   begin
      Fragment.Display := Local_Null_Text_Display'Access;
      Fragment.Layout_Rec := (0, 0, 350, 400);
      Fragment.Lines.Append (new Line_Info);
      Fragment.Default_Style := Komnenos.Themes.Active_Theme.Default_Style;
      Fragment.Bindings.Default_Bindings;
   end Initialize;

   ----------------------
   -- Insert_At_Cursor --
   ----------------------

   overriding procedure Insert_At_Cursor
     (Fragment : in out Root_Fragment_Type;
      Text     : String)
   is
   begin
      Fragment.Display.Insert_At_Cursor (Text);
   end Insert_At_Cursor;

   ----------------
   -- Invalidate --
   ----------------

   overriding procedure Invalidate
     (Fragment : not null access Root_Fragment_Type)
   is
   begin
      Fragment.Display.Render_Fragment (Fragment);
      Fragment.Display.Set_Cursor (Fragment.Point);
   end Invalidate;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Fragment : Root_Fragment_Type;
      Put      : not null access
        procedure (Text : String;
                   Style : Komnenos.Styles.Komnenos_Style;
                   Tool_Tip : String;
                   Link  : Komnenos.Entities.Entity_Reference);
      New_Line : not null access procedure)
   is
      use Ada.Strings.Unbounded;
   begin
      for Line of Fragment.Lines loop
         declare
            Current : Positive := 1;
         begin
            for Style of Line.Styles loop
               declare
                  use Komnenos.Styles;
                  use Komnenos.Entities;
                  Normal_Style : constant Komnenos.Styles.Komnenos_Style :=
                                   Style.Styles (Normal);
                  This_Style : constant Komnenos.Styles.Komnenos_Style :=
                                 (if Normal_Style = Null_Style
                                  then Fragment.Default_Style
                                  else Normal_Style);
                  This_Tool_Tip : constant String :=
                                    Ada.Strings.Unbounded.To_String
                                      (Style.Tool_Tip);
                  This_Link  : constant Komnenos.Entities.Entity_Reference :=
                                 Style.Reference;
                  This_Text  : constant String :=
                                 Slice (Line.Text, Current,
                                        Current + Style.Length - 1);
               begin
                  Put (This_Text, This_Style, This_Tool_Tip, This_Link);
                  Current := Current + Style.Length;
               end;
            end loop;

            if Current <= Length (Line.Text) then
               Put
                 (Slice (Line.Text, Current, Length (Line.Text)),
                  Fragment.Default_Style, "", null);
            end if;
         end;
         New_Line.all;
      end loop;
   end Iterate;

   ---------
   -- Key --
   ---------

   function Key
     (Fragment : Root_Fragment_Type'Class)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Fragment.Key);
   end Key;

   ------------------
   -- Needs_Render --
   ------------------

   function Needs_Render
     (Fragment : Root_Fragment_Type)
      return Boolean
   is
   begin
      return Fragment.Needs_Render;
   end Needs_Render;

   ------------------
   -- New_Fragment --
   ------------------

   function New_Fragment
     return access Komnenos.Session_Objects.Session_Object_Interface'Class
   is
      Result :  constant Fragment_Type := new Root_Fragment_Type;
   begin
      return Result;
   end New_Fragment;

   --------------
   -- New_Line --
   --------------

   overriding procedure New_Line (Fragment : in out Root_Fragment_Type) is
   begin
      Fragment.Lines.Append (new Line_Info);
   end New_Line;

   --------------------
   -- On_Cursor_Move --
   --------------------

   procedure On_Cursor_Move
     (Fragment     : in out Root_Fragment_Type;
      New_Position : Text_Position)
   is
      Move : Komnenos.Commands.Root_Komnenos_Command'Class :=
               Komnenos.Commands.Cursor_Movement.Move_To_Position_Command
                 (New_Position);
   begin
      Fragment.Commands.Execute (Move, Fragment.Content);
   end On_Cursor_Move;

   ------------------
   -- On_Key_Press --
   ------------------

   procedure On_Key_Press
     (Fragment : in out Root_Fragment_Type;
      Key      : Komnenos.Keys.Komnenos_Key)
   is
      use Komnenos.Keys, Komnenos.Keys.Sequences;
      Incomplete, Match : Boolean;
      Command           : Komnenos.Commands.Komnenos_Command;
   begin
      Add_Key (Fragment.Key_Sequence, Key);
      Fragment.Bindings.Get_Binding
        (Fragment.Key_Sequence, Incomplete, Match, Command);
      if Incomplete then
         null;
      else
         if Match then
            Fragment.Commands.Execute
              (Command.all, Fragment.Content);
         end if;
         Clear (Fragment.Key_Sequence);
      end if;
   end On_Key_Press;

   ----------
   -- Path --
   ----------

   function Path
     (Fragment : Root_Fragment_Type'Class)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Fragment.Path);
   end Path;

   ---------
   -- Put --
   ---------

   overriding procedure Put
     (Fragment : in out Root_Fragment_Type;
      Text     : in     String;
      Style    : in     Komnenos.Styles.Komnenos_Style;
      Tool_Tip : in     String;
      Link     : access Komnenos.Entities.Root_Entity_Reference'Class)
   is
      use type Komnenos.Styles.Komnenos_Style;
      use type Komnenos.Entities.Entity_Reference;
      Line : constant Line_Info_Access := Fragment.Lines.Last_Element;
      Line_Style : Style_Info :=
                     (Length    => Text'Length,
                      Styles    => (Normal => Style,
                                    others => null),
                      Tool_Tip  =>
                        Ada.Strings.Unbounded.To_Unbounded_String
                          (Tool_Tip),
                      Reference => Komnenos.Entities.Entity_Reference (Link));
   begin
      if Link /= null then
         Line_Style.Styles (Hover) :=
           Komnenos.Themes.Active_Theme.Default_Link_Style;
      end if;

      Line.Styles.Append (Line_Style);
      Ada.Strings.Unbounded.Append (Line.Text, Text);
   end Put;

   ---------------
   -- Rectangle --
   ---------------

   function Rectangle
     (Fragment : Root_Fragment_Type'Class)
      return Layout_Rectangle
   is
   begin
      return Fragment.Layout_Rec;
   end Rectangle;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Komnenos.Session_Objects.Register_Session_Object
        ("fragment", New_Fragment'Access);
   end Register;

   --------------
   -- Rendered --
   --------------

   procedure Rendered
     (Fragment : in out Root_Fragment_Type)
   is
   begin
      Fragment.Needs_Render := False;
   end Rendered;

   -----------------
   -- Set_Content --
   -----------------

   overriding procedure Set_Content
     (Fragment : in out Root_Fragment_Type;
      Content  : access Komnenos.Entities.Root_Entity_Reference'Class)
   is
   begin
      Fragment.Content := Komnenos.Entities.Entity_Reference (Content);
   end Set_Content;

   ----------------
   -- Set_Cursor --
   ----------------

   overriding procedure Set_Cursor
     (Fragment : in out Root_Fragment_Type;
      Cursor   : Komnenos.Entities.Cursor_Type;
      Position : Text_Position)
   is
      use all type Komnenos.Entities.Cursor_Type;
   begin
      case Cursor is
         when Point =>
            Fragment.Point := Position;
         when Mark =>
            null;
         when Selection_Start =>
            null;
         when Selection_End =>
            null;
      end case;
   end Set_Cursor;

   --------------------
   -- Set_Entity_Key --
   --------------------

   procedure Set_Entity_Key
     (Fragment : in out Root_Fragment_Type'Class;
      Key      : String)
   is
   begin
      Fragment.Key := Ada.Strings.Unbounded.To_Unbounded_String (Key);
   end Set_Entity_Key;

   ------------------
   -- Set_Position --
   ------------------

   procedure Set_Position
     (Fragment : in out Root_Fragment_Type'Class;
      X, Y     : Pixel_Position)
   is
   begin
      Fragment.Layout_Rec.X := X;
      Fragment.Layout_Rec.Y := Y;
   end Set_Position;

   --------------
   -- Set_Size --
   --------------

   procedure Set_Size
     (Fragment : in out Root_Fragment_Type'Class;
      Width    : Pixel_Length;
      Height   : Pixel_Length)
   is
   begin
      Fragment.Layout_Rec.Width := Width;
      Fragment.Layout_Rec.Height := Height;
   end Set_Size;

   ----------------------
   -- Set_Text_Display --
   ----------------------

   procedure Set_Text_Display
     (Fragment : in out Root_Fragment_Type;
      Display  : access Text_Editor_Display'Class)
   is
   begin
      if Display = null then
         Fragment.Display := Local_Null_Text_Display'Access;
      else
         Fragment.Display := Display;
      end if;
   end Set_Text_Display;

   -------------------
   -- Text_Contents --
   -------------------

   function Text_Contents
     (Fragment : Root_Fragment_Type)
      return String
   is
      use Ada.Strings.Unbounded;
      Result : Unbounded_String;
   begin
      for Line of Fragment.Lines loop
         Result := Result & Line.Text;
         Result := Result & Ada.Characters.Latin_1.LF;
      end loop;
      return To_String (Result);
   end Text_Contents;

   -----------
   -- Title --
   -----------

   function Title
     (Fragment : Root_Fragment_Type'Class)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Fragment.Title);
   end Title;

   ---------------
   -- To_Config --
   ---------------

   overriding procedure To_Config
     (Fragment : Root_Fragment_Type;
      Config   : in out Tropos.Configuration)
   is
   begin
      Config.Add ("default_style", Fragment.Default_Style.Name);
      Config.Add ("link_style", Fragment.Default_Style.Name);
      Config.Add (To_Config (Fragment.Layout_Rec));
      Config.Add ("path", Ada.Strings.Unbounded.To_String (Fragment.Path));
      Config.Add ("title", Ada.Strings.Unbounded.To_String (Fragment.Title));
      Config.Add ("editable", (if Fragment.Editable then "yes" else "no"));
      Config.Add ("background",
                  Komnenos.Colours.To_String (Fragment.Background_Colour));
      Config.Add ("foreground",
                  Komnenos.Colours.To_String (Fragment.Foreground_Colour));
      Config.Add ("border",
                  Komnenos.Colours.To_String (Fragment.Border_Colour));
      Config.Add ("key", Fragment.Entity_Key);
   end To_Config;

end Komnenos.Fragments;
