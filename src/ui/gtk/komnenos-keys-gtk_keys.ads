with Gdk.Types;

package Komnenos.Keys.Gtk_Keys is

   function To_Komnenos_Key
     (Key   : Gdk.Types.Gdk_Key_Type;
      State : Gdk.Types.Gdk_Modifier_Type)
      return Komnenos_Key;

end Komnenos.Keys.Gtk_Keys;
