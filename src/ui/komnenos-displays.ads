with Komnenos.Colours;
with Komnenos.Fonts;

package Komnenos.Displays is

   type Canvas_Layer is (Base, Overlay, Selection);

   type Canvas_Display is interface;

   function Get_Bounding_Rectangle
     (Canvas    : Canvas_Display;
      Font      : Komnenos.Fonts.Komnenos_Font;
      Text      : String)
      return Layout_Rectangle
      is abstract;

   procedure Draw_Rectangle
     (Canvas            : in out Canvas_Display;
      Layer             : Canvas_Layer;
      Rectangle         : Layout_Rectangle;
      Border_Colour     : Komnenos.Colours.Komnenos_Colour;
      Background_Colour : Komnenos.Colours.Komnenos_Colour;
      Filled            : Boolean;
      Corner_Radius     : Pixel_Length)
   is abstract;

   procedure Draw_Text
     (Canvas    : in out Canvas_Display;
      Layer     : Canvas_Layer;
      Rectangle : Layout_Rectangle;
      Font      : Komnenos.Fonts.Komnenos_Font;
      Text      : String)
   is abstract;

   procedure Draw_Line
     (Canvas    : in out Canvas_Display;
      Layer     : Canvas_Layer;
      Line      : Layout_Line;
      Width     : Pixel_Length;
      Colour    : Komnenos.Colours.Komnenos_Colour;
      Curved    : Boolean;
      Arrow     : Boolean)
   is abstract;

   procedure Refresh (Canvas : in out Canvas_Display) is abstract;

   type Compass_Direction is (East, North, West, South);
   type Turtle_Atom is (Move, Turn);

   type Turtle_Command is
      record
         Atom      : Turtle_Atom;
         Length    : Pixel_Length;
         Direction : Compass_Direction;
      end record;

   function Move (Length : Pixel_Length) return Turtle_Command;
   function Turn (Direction : Compass_Direction) return Turtle_Command;
   function Turn (Direction : Compass_Direction;
                  Radius  : Pixel_Length)
                  return Turtle_Command;

   type Turtle_Path is array (Positive range <>) of Turtle_Command;

   procedure Draw_Turtle_Path
     (Canvas          : in out Canvas_Display;
      Layer           : Canvas_Layer;
      Start_Location  : Layout_Point;
      Start_Direction : Compass_Direction;
      Path            : Turtle_Path;
      Width           : Pixel_Length;
      Colour          : Komnenos.Colours.Komnenos_Colour;
      Arrow           : Boolean)
   is abstract;

private

   function Move (Length : Pixel_Length) return Turtle_Command
   is (Move, Length, North);

   function Turn (Direction : Compass_Direction) return Turtle_Command
   is (Turn, 0, Direction);

   function Turn (Direction : Compass_Direction;
                  Radius    : Pixel_Length)
                  return Turtle_Command
   is (Turn, Radius, Direction);

end Komnenos.Displays;
