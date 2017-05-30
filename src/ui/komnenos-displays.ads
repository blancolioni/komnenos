with Komnenos.Colours;
with Komnenos.Fonts;

package Komnenos.Displays is

   type Canvas_Display is interface;

   procedure Draw_Rectangle
     (Canvas            : in out Canvas_Display;
      Rectangle         : Layout_Rectangle;
      Border_Colour     : Komnenos.Colours.Komnenos_Colour;
      Background_Colour : Komnenos.Colours.Komnenos_Colour;
      Filled            : Boolean;
      Corner_Radius     : Pixel_Length)
   is abstract;

   procedure Draw_Text
     (Canvas    : in out Canvas_Display;
      Top_Left  : Layout_Point;
      Font      : Komnenos.Fonts.Komnenos_Font;
      Text      : String)
   is abstract;

   procedure Draw_Line
     (Canvas    : in out Canvas_Display;
      Line      : Layout_Line;
      Colour    : Komnenos.Colours.Komnenos_Colour;
      Curved    : Boolean;
      Arrow     : Boolean)
   is abstract;

end Komnenos.Displays;
