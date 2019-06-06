with Komnenos.Colours;
with Komnenos.Named_Objects;

package Komnenos.Fonts is

   type Root_Komnenos_Font is
     new Komnenos.Named_Objects.Root_Named_Object with private;

   function Size (Font : Root_Komnenos_Font'Class) return Natural;

   function Is_Bold
     (Font : Root_Komnenos_Font'Class)
      return Boolean;

   function Is_Italic
     (Font : Root_Komnenos_Font'Class)
      return Boolean;

   function Is_Underlined
     (Font : Root_Komnenos_Font'Class)
      return Boolean;

   function Is_Strike_Through
     (Font : Root_Komnenos_Font'Class)
      return Boolean;

   function Has_Foreground_Color
     (Font : Root_Komnenos_Font'Class)
      return Boolean;

   function Has_Background_Color
     (Font : Root_Komnenos_Font'Class)
      return Boolean;

   function Foreground_Color
     (Font : Root_Komnenos_Font'Class)
      return Komnenos.Colours.Komnenos_Colour
     with Pre => Has_Foreground_Color (Font);

   function Background_Color
     (Font : Root_Komnenos_Font'Class)
      return Komnenos.Colours.Komnenos_Colour
     with Pre => Has_Background_Color (Font);

   procedure Set_Bold
     (Font  : in out Root_Komnenos_Font'Class;
      Value : in     Boolean);

   procedure Set_Italic
     (Font  : in out Root_Komnenos_Font'Class;
      Value : in     Boolean);

   procedure Set_Underlined
     (Font  : in out Root_Komnenos_Font'Class;
      Value : in     Boolean);

   procedure Set_Foreground_Color
     (Font   : in out Root_Komnenos_Font'Class;
      Colour : in     Komnenos.Colours.Komnenos_Colour);

   procedure Set_Background_Color
     (Font   : in out Root_Komnenos_Font'Class;
      Colour : in     Komnenos.Colours.Komnenos_Colour);

   type Komnenos_Font is access all Root_Komnenos_Font'Class;

   function Create_Font
     (Name       : String;
      Size       : Natural;
      Foreground : Komnenos.Colours.Komnenos_Colour;
      Bold       : Boolean         := False;
      Italic     : Boolean         := False;
      Underlined : Boolean         := False)
      return Komnenos_Font;

   function Create_Font
     (Name       : String;
      Size       : Natural;
      Bold       : Boolean         := False;
      Italic     : Boolean         := False;
      Underlined : Boolean         := False)
      return Komnenos_Font;

private

   type Root_Komnenos_Font is
     new Komnenos.Named_Objects.Root_Named_Object with
      record
         Size                     : Natural;
         Foreground               : Komnenos.Colours.Komnenos_Colour;
         Background               : Komnenos.Colours.Komnenos_Colour;
         Bold, Italic, Underlined : Boolean           := False;
         Strike_Through           : Boolean           := False;
         Have_Foreground          : Boolean           := False;
         Have_Background          : Boolean           := False;
      end record;

   function Size (Font : Root_Komnenos_Font'Class) return Natural
   is (Font.Size);

   function Is_Bold
     (Font : Root_Komnenos_Font'Class)
      return Boolean
   is (Font.Bold);

   function Is_Italic
     (Font : Root_Komnenos_Font'Class)
      return Boolean
   is (Font.Italic);

   function Is_Underlined
     (Font : Root_Komnenos_Font'Class)
      return Boolean
   is (Font.Underlined);

   function Is_Strike_Through
     (Font : Root_Komnenos_Font'Class)
      return Boolean
   is (Font.Strike_Through);

   function Has_Foreground_Color
     (Font : Root_Komnenos_Font'Class)
      return Boolean
   is (Font.Have_Foreground);

   function Has_Background_Color
     (Font : Root_Komnenos_Font'Class)
      return Boolean
   is (Font.Have_Background);

   function Foreground_Color
     (Font : Root_Komnenos_Font'Class)
      return Komnenos.Colours.Komnenos_Colour
   is (Font.Foreground);

   function Background_Color
     (Font : Root_Komnenos_Font'Class)
      return Komnenos.Colours.Komnenos_Colour
   is (Font.Background);

end Komnenos.Fonts;
