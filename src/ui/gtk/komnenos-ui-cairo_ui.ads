with Cairo;

with Css;

with Komnenos.Fonts;

package Komnenos.UI.Cairo_UI is

   procedure Create_Line_Path
     (Context : Cairo.Cairo_Context;
      Line    : Layout_Line;
      Curved  : Boolean;
      Arrow   : Boolean);

   procedure Create_Rectangle_Path
     (Context       : Cairo.Cairo_Context;
      Rectangle     : Layout_Rectangle;
      Corner_Radius : Pixel_Length);

   procedure Set_Font
     (Context : Cairo.Cairo_Context;
      Font    : Komnenos.Fonts.Komnenos_Font);

   procedure Set_Css_Style
     (Context : Cairo.Cairo_Context;
      Element : Css.Css_Element_Interface'Class)
   is null;

end Komnenos.UI.Cairo_UI;
