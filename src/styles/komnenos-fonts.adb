package body Komnenos.Fonts is

   -----------------
   -- Create_Font --
   -----------------

   function Create_Font
     (Name       : String;
      Size       : Natural;
      Foreground : Komnenos.Colours.Komnenos_Colour;
      Bold       : Boolean         := False;
      Italic     : Boolean         := False;
      Underlined : Boolean         := False)
      return Komnenos_Font
   is
   begin
      return Font : constant Komnenos_Font := new Root_Komnenos_Font'
        (Komnenos.Named_Objects.Root_Named_Object with
         Size            => Size,
         Foreground      => Foreground,
         Background      => Komnenos.Colours.White,
         Bold            => Bold,
         Italic          => Italic,
         Underlined      => Underlined,
         Strike_Through  => False,
         Have_Foreground => True,
         Have_Background => False)
      do
         Font.Set_Name (Name);
      end return;
   end Create_Font;

   -----------------
   -- Create_Font --
   -----------------

   function Create_Font
     (Name       : String;
      Size       : Natural;
      Bold       : Boolean         := False;
      Italic     : Boolean         := False;
      Underlined : Boolean         := False)
      return Komnenos_Font
   is
   begin
      return Font : constant Komnenos_Font := new Root_Komnenos_Font'
        (Komnenos.Named_Objects.Root_Named_Object with
         Size            => Size,
         Foreground      => Komnenos.Colours.Black,
         Background      => Komnenos.Colours.White,
         Bold            => Bold,
         Italic          => Italic,
         Underlined      => Underlined,
         Strike_Through  => False,
         Have_Foreground => False,
         Have_Background => False)
      do
         Font.Set_Name (Name);
      end return;
   end Create_Font;

   --------------------------
   -- Set_Background_Color --
   --------------------------

   procedure Set_Background_Color
     (Font   : in out Root_Komnenos_Font'Class;
      Colour : in     Komnenos.Colours.Komnenos_Colour)
   is
   begin
      Font.Background := Colour;
      Font.Have_Background := True;
   end Set_Background_Color;

   --------------
   -- Set_Bold --
   --------------

   procedure Set_Bold
     (Font  : in out Root_Komnenos_Font'Class;
      Value : in     Boolean)
   is
   begin
      Font.Bold := Value;
   end Set_Bold;

   --------------------------
   -- Set_Foreground_Color --
   --------------------------

   procedure Set_Foreground_Color
     (Font   : in out Root_Komnenos_Font'Class;
      Colour : in     Komnenos.Colours.Komnenos_Colour)
   is
   begin
      Font.Foreground := Colour;
      Font.Have_Foreground := True;
   end Set_Foreground_Color;

   ----------------
   -- Set_Italic --
   ----------------

   procedure Set_Italic
     (Font  : in out Root_Komnenos_Font'Class;
      Value : in     Boolean)
   is
   begin
      Font.Italic := Value;
   end Set_Italic;

   --------------------
   -- Set_Underlined --
   --------------------

   procedure Set_Underlined
     (Font  : in out Root_Komnenos_Font'Class;
      Value : in     Boolean)
   is
   begin
      Font.Underlined := Value;
   end Set_Underlined;

end Komnenos.Fonts;
