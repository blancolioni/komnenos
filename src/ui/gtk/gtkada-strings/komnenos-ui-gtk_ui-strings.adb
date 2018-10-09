with Gtkada.Types;

package body Komnenos.UI.Gtk_UI.Strings is

   ------------------------
   -- Cairo_Text_Extents --
   ------------------------

   function Cairo_Text_Extents
     (Context : Cairo.Cairo_Context;
      Text    : String)
      return Cairo.Cairo_Text_Extents
   is
      Extents   : aliased Cairo.Cairo_Text_Extents;
      C_Text    : Gtkada.Types.Chars_Ptr :=
                 Gtkada.Types.New_String (Text);
   begin
      Cairo.Text_Extents (Context, C_Text, Extents'Access);
      Gtkada.Types.Free (C_Text);
      return Extents;
   end Cairo_Text_Extents;

end Komnenos.UI.Gtk_UI.Strings;
