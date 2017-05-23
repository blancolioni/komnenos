private with Ada.Strings.Unbounded;

package Komnenos.Colours is

   type Komnenos_Colour is private;

   function From_String
     (String_Spec : String)
      return Komnenos_Colour;

   function From_RGB
     (R, G, B     : Natural)
      return Komnenos_Colour;

   function From_RGBA
     (R, G, B, A  : Natural)
      return Komnenos_Colour;

   function Black return Komnenos_Colour;
   function White return Komnenos_Colour;

   function To_String
     (Colour : Komnenos_Colour)
      return String;

private

   type Komnenos_Colour is
      record
         Text : Ada.Strings.Unbounded.Unbounded_String;
      end record;

end Komnenos.Colours;
