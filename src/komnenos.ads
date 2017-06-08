with Tropos;

package Komnenos is

   type Line_Offset is new Integer;
   type Column_Offset is new Integer;

   subtype Line_Number is Line_Offset range 1 .. Line_Offset'Last;
   subtype Column_Number is Column_Offset range 1 .. Column_Offset'Last;

   type Text_Movement_Unit is
     (Character_Unit,
      Word_Unit,
      Line_Unit,
      Page_Unit,
      Buffer_Unit);

   type Text_Offset is new Integer;

   subtype Text_Position is
     Text_Offset range 0 .. Text_Offset'Last;

   type Text_Movement is
      record
         Unit   : Text_Movement_Unit;
         Offset : Text_Offset;
      end record;

   type Cursor_Type is (Point, Mark, Selection_Start, Selection_End);

   type Cursor_Position_Array is
     array (Cursor_Type) of Text_Position;

   type Element_State is (Normal, Hover, Selected, Disabled);

   type Pixel_Position is new Integer;
   subtype Pixel_Offset is Pixel_Position;
   subtype Pixel_Length is Pixel_Position range 0 .. Pixel_Position'Last;

   type Layout_Point is
      record
         X, Y : Pixel_Position;
      end record;

   type Layout_Rectangle is
      record
         X, Y          : Pixel_Position;
         Width, Height : Pixel_Length;
      end record;

   function Contains
     (Rectangle : Layout_Rectangle;
      X, Y      : Pixel_Position)
      return Boolean
   is (X in Rectangle.X .. Rectangle.X + Rectangle.Width
       and then Y in Rectangle.Y .. Rectangle.Y + Rectangle.Height);

   function Contains
     (Rectangle : Layout_Rectangle;
      Point     : Layout_Point)
      return Boolean
   is (Contains (Rectangle, Point.X, Point.Y));

   function Union
     (Left, Right : Layout_Rectangle)
      return Layout_Rectangle;

   function To_Config
     (Rectangle : Layout_Rectangle)
      return Tropos.Configuration;

   function From_Config
     (Config : Tropos.Configuration)
      return Layout_Rectangle;

   type Layout_Line is array (Positive range <>) of Layout_Point;

   type Node_Style is
     (Box, Rounded_Box, Circle, Internal);

   type Node_Edge is (Left, Top, Right, Bottom);

   type Node_Visibility is
     (Always_Visible, Show_On_Hover, Show_On_Selected,
      Show_On_Parent_Selected, Never_Visible);

   type Node_Reference is new Positive;

end Komnenos;
