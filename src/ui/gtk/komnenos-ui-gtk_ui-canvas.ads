with Gtk.Widget;

private with Gtk.Drawing_Area;
private with Cairo;

private with Komnenos.Colours;
private with Komnenos.Fonts;

with Komnenos.Displays;
with Komnenos.Fragments;

package Komnenos.UI.Gtk_UI.Canvas is

   type Komnenos_Canvas_View_Record is
     new Gtk.Widget.Gtk_Widget_Record
     and Komnenos.Displays.Canvas_Display
   with private;

   type Komnenos_Canvas_View is access all Komnenos_Canvas_View_Record'Class;

   function Create_Canvas_View
     (Fragment : Komnenos.Fragments.Fragment_Type)
      return Komnenos_Canvas_View;

private

   type Layer_Array is
     array (Komnenos.Displays.Canvas_Layer) of Cairo.Cairo_Surface;

   type Komnenos_Canvas_View_Record is
     new Gtk.Drawing_Area.Gtk_Drawing_Area_Record
     and Komnenos.Displays.Canvas_Display with
      record
         Fragment  : Komnenos.Fragments.Fragment_Type;
         Draw_Area : Gtk.Drawing_Area.Gtk_Drawing_Area;
         Layers    : Layer_Array :=
                       (others => Cairo.Null_Surface);
      end record;

   overriding function Get_Bounding_Rectangle
     (Canvas    : Komnenos_Canvas_View_Record;
      Font      : Komnenos.Fonts.Komnenos_Font;
      Text      : String)
     return Layout_Rectangle;

   overriding procedure Draw_Rectangle
     (Canvas            : in out Komnenos_Canvas_View_Record;
      Layer             : Komnenos.Displays.Canvas_Layer;
      Rectangle         : Layout_Rectangle;
      Border_Colour     : Komnenos.Colours.Komnenos_Colour;
      Background_Colour : Komnenos.Colours.Komnenos_Colour;
      Filled            : Boolean;
      Corner_Radius     : Pixel_Length);

   overriding procedure Draw_Text
     (Canvas    : in out Komnenos_Canvas_View_Record;
      Layer     : Komnenos.Displays.Canvas_Layer;
      Rectangle : Layout_Rectangle;
      Font      : Komnenos.Fonts.Komnenos_Font;
      Text      : String);

   overriding procedure Draw_Line
     (Canvas    : in out Komnenos_Canvas_View_Record;
      Layer     : Komnenos.Displays.Canvas_Layer;
      Line      : Layout_Line;
      Width     : Pixel_Length;
      Colour    : Komnenos.Colours.Komnenos_Colour;
      Curved    : Boolean;
      Arrow     : Boolean);

   overriding procedure Draw_Turtle_Path
     (Canvas          : in out Komnenos_Canvas_View_Record;
      Layer           : Komnenos.Displays.Canvas_Layer;
      Start_Location  : Layout_Point;
      Start_Direction : Komnenos.Displays.Compass_Direction;
      Path            : Komnenos.Displays.Turtle_Path;
      Width           : Pixel_Length;
      Colour          : Komnenos.Colours.Komnenos_Colour;
      Arrow           : Boolean);

end Komnenos.UI.Gtk_UI.Canvas;
