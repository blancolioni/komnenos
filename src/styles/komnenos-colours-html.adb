package body Komnenos.Colours.Html is

   --------------------
   -- To_Html_Colour --
   --------------------

   function To_Html_Colour
     (Colour : Komnenos_Colour)
      return String
   is
   begin
      return To_String (Colour);
   end To_Html_Colour;

end Komnenos.Colours.Html;
