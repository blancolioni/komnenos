private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;
private with Ada.Finalization;
private with Ada.Strings.Unbounded;

with Tropos;

with Css;

with Komnenos.Colours;
with Komnenos.Keys.Sequences;
with Komnenos.Styles;

with Komnenos.Displays;
with Komnenos.Entities;
with Komnenos.Session_Objects;

with Komnenos.Commands.Manager;
private with Komnenos.Commands.Bindings;

package Komnenos.Fragments is

   type Fragment_Display is interface;

   procedure On_Fragment_Resized
     (Display : in out Fragment_Display)
   is abstract;

   type Text_Editor_Display is interface and Fragment_Display;

   procedure Insert_At_Cursor
     (Editor : in out Text_Editor_Display;
      Text   : in     String)
   is abstract;

   procedure Delete_From_Cursor
     (Editor : in out Text_Editor_Display;
      Movement : Text_Movement)
   is abstract;

   procedure Set_Cursor
     (Editor       : in out Text_Editor_Display;
      New_Position : Text_Position)
   is abstract;

   procedure Set_Content
     (Editor       : in out Text_Editor_Display;
      New_Content  : String)
   is abstract;

   type Root_Fragment_Type is
     abstract new Komnenos.Entities.Entity_Visual
     and Komnenos.Session_Objects.Session_Object_Interface
     and Css.Css_Element_Interface
   with private;

   procedure Render_Fragment
     (Editor   : in out Text_Editor_Display;
      Fragment : not null access Root_Fragment_Type'Class)
   is abstract;

   overriding function Config_Name
     (Fragment : Root_Fragment_Type)
      return String
   is ("fragment");

   overriding procedure Create_Style
     (Fragment : in out Root_Fragment_Type;
      Name     : String);

   overriding procedure Set_Style
     (Fragment : in out Root_Fragment_Type;
      Name     : String;
      State    : String;
      Value    : Css.Css_Element_Value);

   overriding function Style
     (Fragment : Root_Fragment_Type;
      Name     : String)
      return Css.Css_Element_Value;

   overriding function Get_Layout_Position
     (Fragment : Root_Fragment_Type)
      return Css.Layout_Position;

   overriding procedure Set_Layout_Position
     (Fragment : in out Root_Fragment_Type;
      Position : Css.Layout_Position);

   overriding function Get_Layout_Size
     (Fragment : Root_Fragment_Type)
      return Css.Layout_Size;

   overriding procedure Set_Layout_Size
     (Fragment : in out Root_Fragment_Type;
      Size     : Css.Layout_Size);

   overriding function Id
     (Fragment : Root_Fragment_Type)
      return String;

   overriding function Classes
     (Fragment : Root_Fragment_Type)
      return String;

   overriding function Minimum_Size
     (Fragment   : Root_Fragment_Type;
      Constraint : Css.Layout_Size)
      return Css.Layout_Size;

   overriding function Required_Parent_Tag
     (Fragment : Root_Fragment_Type)
      return String;

   overriding function Parent_Element
     (Fragment : Root_Fragment_Type)
      return access Css.Css_Element_Interface'Class;

   overriding function Child_Index
     (Fragment : Root_Fragment_Type)
      return Natural;

   overriding function Child_Elements
     (Fragment : Root_Fragment_Type)
      return Css.Array_Of_Elements;

   overriding function Contents_Layout_Size
     (Fragment : Root_Fragment_Type)
      return Css.Layout_Size;

   overriding procedure Set_Contents_Layout_Size
     (Fragment : in out Root_Fragment_Type;
      Size     : Css.Layout_Size);

   overriding function Default_Style_Value
     (Fragment : Root_Fragment_Type;
      Name     : String)
      return Css.Css_Element_Value;

   overriding function Inline_Style_Rules
     (Fragment : Root_Fragment_Type)
      return Css.Css_Rule;

   overriding function Is_Table
     (Fragment : Root_Fragment_Type)
      return Boolean
   is (False);

   function File_Name
     (Fragment : Root_Fragment_Type'Class)
      return String;

   function Path
     (Fragment : Root_Fragment_Type'Class)
      return String;

   function Title
     (Fragment : Root_Fragment_Type'Class)
      return String;

   function Key
     (Fragment : Root_Fragment_Type'Class)
      return String;

   function Rectangle
     (Fragment : Root_Fragment_Type'Class)
      return Layout_Rectangle;

   overriding function X (Fragment : Root_Fragment_Type) return Pixel_Position
   is (Fragment.Rectangle.X);

   overriding function Y (Fragment : Root_Fragment_Type) return Pixel_Position
   is (Fragment.Rectangle.Y);

   overriding function Width
     (Fragment : Root_Fragment_Type) return Pixel_Length
   is (Fragment.Rectangle.Width);

   overriding function Height
     (Fragment : Root_Fragment_Type) return Pixel_Length
   is (Fragment.Rectangle.Height);

   procedure Set_Text_Display
     (Fragment : in out Root_Fragment_Type;
      Display  : access Text_Editor_Display'Class);

   procedure Execute
     (Fragment : in out Root_Fragment_Type'Class;
      Command  : in out Komnenos.Commands.Root_Komnenos_Command'Class);

   procedure Rendered
     (Fragment : in out Root_Fragment_Type);

   procedure Set_Position
     (Fragment : in out Root_Fragment_Type'Class;
      X, Y     : Pixel_Position);

   procedure Set_Size
     (Fragment : in out Root_Fragment_Type'Class;
      Width    : Pixel_Length;
      Height   : Pixel_Length);

   procedure Set_Entity_Key
     (Fragment : in out Root_Fragment_Type'Class;
      Key      : String);

   function Entity_Key (Fragment : Root_Fragment_Type'Class)
                        return String;

   procedure On_Key_Press
     (Fragment : in out Root_Fragment_Type;
      Key      : Komnenos.Keys.Komnenos_Key);

   function Editable
     (Fragment : Root_Fragment_Type)
      return Boolean;

   function Background_Colour
     (Fragment : Root_Fragment_Type)
      return Komnenos.Colours.Komnenos_Colour;

   function Border_Colour
     (Fragment : Root_Fragment_Type)
      return Komnenos.Colours.Komnenos_Colour;

   function Foreground_Colour
     (Fragment : Root_Fragment_Type)
      return Komnenos.Colours.Komnenos_Colour;

   procedure Set_Canvas
     (Fragment : in out Root_Fragment_Type'Class;
      Canvas   : access Komnenos.Displays.Canvas_Display'Class);

   procedure On_Click
     (Fragment : not null access Root_Fragment_Type;
      X, Y     : Pixel_Position;
      Modifier : Komnenos.Keys.Modifier_Keys)
   is null;

   type Fragment_Type is access all Root_Fragment_Type'Class;

   type Text_Fragment_Type is
     new Root_Fragment_Type
     and Komnenos.Entities.Text_Entity_Visual
   with private;

   type Text_Fragment is access all Text_Fragment_Type'Class;

   overriding procedure Clear (Fragment : in out Text_Fragment_Type);

   overriding procedure Put
     (Fragment : in out Text_Fragment_Type;
      Text     : in     String;
      Style    : in     Komnenos.Styles.Komnenos_Style;
      Tool_Tip : in     String;
      Link     : access Komnenos.Entities.Root_Entity_Reference'Class);

   overriding procedure New_Line (Fragment : in out Text_Fragment_Type);

   overriding procedure Set_Text_Display
     (Fragment : in out Text_Fragment_Type;
      Display  : access Text_Editor_Display'Class);

   procedure On_Cursor_Move
     (Fragment     : in out Text_Fragment_Type;
      New_Position : Text_Position);

   function Text_Contents
     (Fragment : Text_Fragment_Type)
      return String;

   function Get_Link
     (Fragment : Text_Fragment_Type;
      Offset   : Positive)
      return Komnenos.Entities.Entity_Reference;
   --  If the text at the given offset has an associated reference, return it
   --  otherwise, return null.

   function Get_Tool_Tip
     (Fragment : Text_Fragment_Type;
      Position : Text_Position)
      return String;
   --  Return the tool tip (if any) at the given position.
   --  If there is no tool tip, return ""

   procedure Get_Style
     (Fragment : Text_Fragment_Type;
      State    : Element_State;
      Offset   : Positive;
      Style    : out Komnenos.Styles.Komnenos_Style;
      Start    : out Natural;
      Finish   : out Natural);

   function Get_Style
     (Fragment : Text_Fragment_Type;
      State    : Element_State;
      Offset   : Positive)
      return Komnenos.Styles.Komnenos_Style;

   procedure Iterate
     (Fragment : Text_Fragment_Type;
      Put      : not null access
        procedure (Text : String;
                   Style : Komnenos.Styles.Komnenos_Style;
                   Tool_Tip : String;
                   Link  : Komnenos.Entities.Entity_Reference);
      New_Line : not null access procedure);

   function New_Text_Fragment
     (Entity : not null access Komnenos.Entities.Root_Entity_Reference'Class)
      return Fragment_Type;

   procedure Register;

private

   type Style_Collection is
     array (Element_State) of Komnenos.Styles.Komnenos_Style;

   type Style_Info is
      record
         Length    : Natural;
         Styles    : Style_Collection;
         Reference : Komnenos.Entities.Entity_Reference;
         Tool_Tip  : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   package Style_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Style_Info);

   type Line_Info is
      record
         Text   : Ada.Strings.Unbounded.Unbounded_String;
         Styles : Style_Lists.List;
      end record;

   type Line_Info_Access is access Line_Info;

   package Line_Vectors is
     new Ada.Containers.Vectors
       (Positive, Line_Info_Access);

   type Root_Fragment_Type is
     abstract new Ada.Finalization.Controlled
     and Css.Css_Element_Interface
     and Komnenos.Entities.Entity_Visual
     and Komnenos.Session_Objects.Session_Object_Interface with
      record
         Content           : Komnenos.Entities.Entity_Reference;
         Styles            : Css.Css_Style_Map;
         Point             : Text_Position := 0;
         Commands          : Komnenos.Commands.Manager.Command_Manager;
         Default_Style     : Komnenos.Styles.Komnenos_Style;
         Layout_Rec        : Layout_Rectangle;
         Css_Position      : Css.Layout_Position;
         Css_Size          : Css.Layout_Size;
         Path              : Ada.Strings.Unbounded.Unbounded_String;
         Title             : Ada.Strings.Unbounded.Unbounded_String;
         Key               : Ada.Strings.Unbounded.Unbounded_String;
         Classes           : Ada.Strings.Unbounded.Unbounded_String;
         Editable          : Boolean;
         Background_Colour : Komnenos.Colours.Komnenos_Colour;
         Foreground_Colour : Komnenos.Colours.Komnenos_Colour;
         Border_Colour     : Komnenos.Colours.Komnenos_Colour;
         Key_Sequence      : Komnenos.Keys.Sequences.Key_Sequence;
         Bindings          : Komnenos.Commands.Bindings.Binding_Table;
         Lines             : Line_Vectors.Vector;
         Display           : access Fragment_Display'Class;
         Canvas            : access Komnenos.Displays.Canvas_Display'Class;
         Needs_Render      : Boolean := False;
      end record;

   function Needs_Render
     (Fragment : Root_Fragment_Type)
      return Boolean;

   function Get_Style_Info
     (Fragment : Root_Fragment_Type'Class;
      Offset   : Positive)
      return Style_Info;

   overriding procedure Initialize (Fragment : in out Root_Fragment_Type);
   overriding procedure Finalize (Fragment : in out Root_Fragment_Type);
   overriding procedure Adjust (Fragment : in out Root_Fragment_Type);

   overriding procedure To_Config
     (Fragment : Root_Fragment_Type;
      Config   : in out Tropos.Configuration);

   overriding procedure From_Config
     (Fragment : not null access Root_Fragment_Type;
      Config   : Tropos.Configuration);

   overriding procedure Set_Content
     (Fragment : in out Root_Fragment_Type;
      Content  : access Komnenos.Entities.Root_Entity_Reference'Class);

   overriding function Get_Content
     (Fragment : Root_Fragment_Type)
      return access Komnenos.Entities.Root_Entity_Reference'Class
   is (Fragment.Content);

   overriding function Style
     (Fragment : Root_Fragment_Type;
      Name     : String)
      return Css.Css_Element_Value
   is (Fragment.Styles.Style (Name));

   overriding function Get_Layout_Position
     (Fragment : Root_Fragment_Type)
      return Css.Layout_Position
   is (Float (Fragment.Rectangle.X), Float (Fragment.Rectangle.Y));

   overriding function Get_Layout_Size
     (Fragment : Root_Fragment_Type)
      return Css.Layout_Size
   is (True, True,
       Float (Fragment.Rectangle.Width), Float (Fragment.Rectangle.Height));

   overriding function Id
     (Fragment : Root_Fragment_Type)
      return String
   is (Ada.Strings.Unbounded.To_String (Fragment.Key));

   overriding function Classes
     (Fragment : Root_Fragment_Type)
      return String
   is (Ada.Strings.Unbounded.To_String (Fragment.Classes));

   overriding function Minimum_Size
     (Fragment : Root_Fragment_Type;
      Constraint : Css.Layout_Size)
      return Css.Layout_Size
   is (Root_Fragment_Type'Class (Fragment).Get_Layout_Size);

   overriding function Required_Parent_Tag
     (Fragment : Root_Fragment_Type)
      return String
   is ("");

   overriding function Parent_Element
     (Fragment : Root_Fragment_Type)
      return access Css.Css_Element_Interface'Class
   is (null);

   overriding function Child_Index
     (Fragment : Root_Fragment_Type)
      return Natural
   is (0);

   overriding function Child_Elements
     (Fragment : Root_Fragment_Type)
      return Css.Array_Of_Elements
   is (Css.No_Elements);

   overriding function Contents_Layout_Size
     (Fragment : Root_Fragment_Type)
      return Css.Layout_Size
   is (Root_Fragment_Type'Class (Fragment).Get_Layout_Size);

   overriding procedure Set_Contents_Layout_Size
     (Fragment : in out Root_Fragment_Type;
      Size     : Css.Layout_Size)
   is null;

   overriding function Default_Style_Value
     (Fragment : Root_Fragment_Type;
      Name     : String)
      return Css.Css_Element_Value
   is (Css.Null_Element_Value);

   overriding function Inline_Style_Rules
     (Fragment : Root_Fragment_Type)
      return Css.Css_Rule
   is (null);

   type Text_Fragment_Type is
     new Root_Fragment_Type
     and Komnenos.Entities.Text_Entity_Visual with
      record
         Text_Display : access Text_Editor_Display'Class;
      end record;

   overriding function Tag
     (Fragment : Text_Fragment_Type)
      return String
   is ("text-fragment");

   overriding procedure Invalidate
     (Fragment : not null access Text_Fragment_Type);

   overriding procedure Set_Cursor
     (Fragment : in out Text_Fragment_Type;
      Cursor   : Cursor_Type;
      Position : Text_Position);

   overriding procedure Insert_At_Cursor
     (Fragment : in out Text_Fragment_Type;
      Text     : String);

   overriding procedure Delete_From_Cursor
     (Fragment  : in out Text_Fragment_Type;
      Movement  : Text_Movement);

end Komnenos.Fragments;
