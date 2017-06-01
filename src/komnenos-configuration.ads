with Komnenos.Colours;
with Komnenos.Fonts;

package Komnenos.Configuration is

   function Get_Colour
     (Principle_Name : String;
      Secondary_Name : String := "")
      return Komnenos.Colours.Komnenos_Colour;

   function Enabled (Setting_Name : String) return Boolean;

   procedure Get_Connector_Metrics
     (Class_Name   : String;
      Colour       : out Komnenos.Colours.Komnenos_Colour;
      Curved       : out Boolean;
      Line_Width   : out Positive;
      Arrow_Length : out Positive;
      Arrow_Width  : out Positive);

   type Diagram_Config is
      record
         Node_Label_Font         : Komnenos.Fonts.Komnenos_Font;
         Node_Border_Width       : Pixel_Length;
         Node_Border_Colour      : Komnenos.Colours.Komnenos_Colour;
         Node_Selected_Colour    : Komnenos.Colours.Komnenos_Colour;
         Connector_Width         : Pixel_Length;
         Connector_Colour        : Komnenos.Colours.Komnenos_Colour;
         Connector_Corner_Radius : Pixel_Length;
         Arrow_Length            : Pixel_Length;
         Arrow_Width             : Pixel_Length;
         Min_Node_Width          : Pixel_Length;
         Min_Node_Height         : Pixel_Length;
         Node_Across_Margin      : Pixel_Length;
         Node_Down_Margin        : Pixel_Length;
         Visible_Node_Gap        : Pixel_Length;
         Internal_Node_Gap       : Pixel_Length;
         Layout_Row_Size         : Pixel_Length;
      end record;

   function Get_Diagram_Config return Diagram_Config;

end Komnenos.Configuration;
