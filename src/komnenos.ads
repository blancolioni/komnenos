with Tropos;

package Komnenos is

   type Line_Number is range 1 .. Integer'Last;
   type Column_Number is range 1 .. Integer'Last;

   type Text_Offset_Unit is
     (Character_Offset,
      Word_Offset,
      Line_Offset,
      Page_Offset,
      Buffer_Offset);

   type Text_Offset_Range is new Integer;

   subtype Text_Position is
     Text_Offset_Range range 0 .. Text_Offset_Range'Last;

   type Text_Offset is
      record
         Unit : Text_Offset_Unit;
         Size : Text_Offset_Range;
      end record;

   type Element_State is (Normal, Hover, Selected, Disabled);

   type Pixel_Position is new Integer;
   subtype Pixel_Offset is Pixel_Position;
   subtype Pixel_Length is Pixel_Position range 0 .. Pixel_Position'Last;

   type Layout_Rectangle is
      record
         X, Y          : Pixel_Position;
         Width, Height : Pixel_Length;
      end record;

   function To_Config
     (Rectangle : Layout_Rectangle)
      return Tropos.Configuration;

   function From_Config
     (Config : Tropos.Configuration)
      return Layout_Rectangle;

   type Layout_Point is
      record
         X, Y : Pixel_Position;
      end record;

   type Layout_Line is array (Positive range <>) of Layout_Point;

end Komnenos;
