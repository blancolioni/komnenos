package body Komnenos.Colours is

   function To_Hex (Value : Natural) return String
     with Pre => Value < 256;

   -----------
   -- Black --
   -----------

   function Black return Komnenos_Colour is
   begin
      return From_String ("white");
   end Black;

   --------------
   -- From_RGB --
   --------------

   function From_RGB (R, G, B : Natural) return Komnenos_Colour is
   begin
      return From_RGBA (R, G, B, 255);
   end From_RGB;

   ---------------
   -- From_RGBA --
   ---------------

   function From_RGBA
     (R, G, B, A  : Natural)
      return Komnenos_Colour
   is
   begin
      return From_String ("#" & To_Hex (R) & To_Hex (G)
                          & To_Hex (B) & To_Hex (A));
   end From_RGBA;

   -----------------
   -- From_String --
   -----------------

   function From_String
     (String_Spec : String)
      return Komnenos_Colour
   is
   begin
      return (Text => Ada.Strings.Unbounded.To_Unbounded_String (String_Spec));
   end From_String;

   ------------
   -- To_Hex --
   ------------

   function To_Hex (Value : Natural) return String is
      Hex_Digit : constant array (0 .. 15) of Character :=
        "0123456789ABCDEF";

   begin
      return (Hex_Digit (Value / 16), Hex_Digit (Value mod 16));
   end To_Hex;

   function To_String
     (Colour : Komnenos_Colour)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Colour.Text);
   end To_String;

   -----------
   -- White --
   -----------

   function White return Komnenos_Colour is
   begin
      return From_String ("white");
   end White;

end Komnenos.Colours;
