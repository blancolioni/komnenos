with Gdk.RGBA;

with Komnenos.Colours.Gtk_Colours;

package body Komnenos.Colours.Cairo_Colours is

   --------------------
   -- Set_Source_Rgb --
   --------------------

   procedure Set_Source_Rgb
     (Context : Cairo.Cairo_Context;
      Colour  : Komnenos_Colour)
   is
      Gdk_Colour : constant Gdk.RGBA.Gdk_RGBA :=
                     Komnenos.Colours.Gtk_Colours.To_Gdk_RGBA (Colour);
   begin
      Cairo.Set_Source_Rgba
        (Cr    => Context,
         Red   => Gdk_Colour.Red,
         Green => Gdk_Colour.Green,
         Blue  => Gdk_Colour.Blue,
         Alpha => Gdk_Colour.Alpha);
   end Set_Source_Rgb;

end Komnenos.Colours.Cairo_Colours;
