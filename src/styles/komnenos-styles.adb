package body Komnenos.Styles is

   ------------------
   -- Create_Style --
   ------------------

   function Create_Style
     (Name         : String;
      Font         : Komnenos.Fonts.Komnenos_Font;
      Mouse_Cursor : Mouse_Cursor_Type := Default)
      return Komnenos_Style
   is
      Style : Komnenos_Root_Style := Komnenos_Root_Style'
        (Komnenos.Named_Objects.Root_Named_Object with
         Font         => Font,
         Mouse_Cursor => Mouse_Cursor);
   begin
      Style.Set_Name (Name);
      return new Komnenos_Root_Style'(Style);
   end Create_Style;

end Komnenos.Styles;
