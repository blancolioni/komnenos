with Gdk.Types.Keysyms;

package body Komnenos.Keys.Gtk_Keys is

   ---------------------
   -- To_Komnenos_Key --
   ---------------------

   function To_Komnenos_Key
     (Key   : Gdk.Types.Gdk_Key_Type;
      State : Gdk.Types.Gdk_Modifier_Type)
      return Komnenos_Key
   is
      use Gdk.Types;
      use Gdk.Types.Keysyms;
      Result : Komnenos_Key := Null_Key;
      Shift  : constant Boolean :=
                 (State and Gdk.Types.Shift_Mask) /= 0
                 and then Key not in 20 .. 126;
   begin
      case Key is
         when 20 .. 126 =>
            Result := Character_Key (Character'Val (Key));
         when GDK_BackSpace =>
            Result := Back_Space;
         when GDK_Linefeed =>
            Result := Line_Feed;
         when GDK_Return =>
            Result := Carriage_Return;
         when GDK_Tab =>
            Result := Tab;
         when GDK_Up =>
            Result := Up_Arrow;
         when GDK_Down =>
            Result := Down_Arrow;
         when GDK_Left =>
            Result := Left_Arrow;
         when GDK_Right =>
            Result := Right_Arrow;
         when others =>
            Result := Null_Key;
      end case;

      return Modify (Result,
                     Shift   => Shift,
                     Control => (State and Gdk.Types.Control_Mask) /= 0,
                     Alt     => (State and Gdk.Types.Mod1_Mask) /= 0,
                     Meta    => (State and Gdk.Types.Mod2_Mask) /= 0);

   end To_Komnenos_Key;

   -------------------------------
   -- To_Komnenos_Modifier_Keys --
   -------------------------------

   function To_Komnenos_Modifier_Keys
     (State : Gdk.Types.Gdk_Modifier_Type)
      return Modifier_Keys
   is
      use type Gdk.Types.Gdk_Modifier_Type;
   begin
      return Modifier
        (Shift   => (State and Gdk.Types.Shift_Mask) /= 0,
         Control => (State and Gdk.Types.Control_Mask) /= 0,
         Alt     => (State and Gdk.Types.Mod1_Mask) /= 0,
         Meta    => (State and Gdk.Types.Mod2_Mask) /= 0);
   end To_Komnenos_Modifier_Keys;

end Komnenos.Keys.Gtk_Keys;
