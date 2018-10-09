with Cairo;

package Komnenos.UI.Gtk_UI.Strings is

   function Cairo_Text_Extents
     (Context : Cairo.Cairo_Context;
      Text    : String)
      return Cairo.Cairo_Text_Extents;

end Komnenos.UI.Gtk_UI.Strings;
