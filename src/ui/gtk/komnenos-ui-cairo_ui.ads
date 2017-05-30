with Cairo;

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

end Komnenos.UI.Cairo_UI;
