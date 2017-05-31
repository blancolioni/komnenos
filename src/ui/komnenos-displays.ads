with Komnenos.Colours;
with Komnenos.Fonts;

package Komnenos.Displays is

   type Canvas_Display is interface;

   function Get_Bounding_Rectangle
     (Canvas    : Canvas_Display;
      Font      : Komnenos.Fonts.Komnenos_Font;
      Text      : String)
      return Layout_Rectangle
      is abstract;

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
      Rectangle : Layout_Rectangle;
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
