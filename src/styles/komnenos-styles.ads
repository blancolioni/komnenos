with Komnenos.Fonts;
with Komnenos.Named_Objects;

package Komnenos.Styles is

   type Mouse_Cursor_Type is
     (Default,
      Hand);

   type Komnenos_Root_Style is
     new Komnenos.Named_Objects.Root_Named_Object with private;

   type Komnenos_Style is access constant Komnenos_Root_Style'Class;

   Null_Style : constant Komnenos_Style := null;

   function Font (Style : Komnenos_Root_Style'Class)
                  return Komnenos.Fonts.Komnenos_Font;

   function Mouse_Cursor
     (Style : Komnenos_Root_Style'Class)
      return Mouse_Cursor_Type;

   function Create_Style
     (Name         : String;
      Font         : Komnenos.Fonts.Komnenos_Font;
      Mouse_Cursor : Mouse_Cursor_Type := Default)
      return Komnenos_Style;

private

   type Komnenos_Root_Style is
     new Komnenos.Named_Objects.Root_Named_Object with
      record
         Font         : Komnenos.Fonts.Komnenos_Font;
         Mouse_Cursor : Mouse_Cursor_Type;
      end record;

   function Font (Style : Komnenos_Root_Style'Class)
                  return Komnenos.Fonts.Komnenos_Font
   is (Style.Font);

   function Mouse_Cursor
     (Style : Komnenos_Root_Style'Class)
      return Mouse_Cursor_Type
   is (Style.Mouse_Cursor);

end Komnenos.Styles;
