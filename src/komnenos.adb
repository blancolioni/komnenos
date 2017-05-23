package body Komnenos is

   -----------------
   -- From_Config --
   -----------------

   function From_Config (Config : Tropos.Configuration)
                         return Layout_Rectangle
   is
      function Get (Name : String) return Pixel_Position
      is (Pixel_Position (Integer'(Config.Get (Name))));

   begin
      return Layout_Rectangle'
        (X      => Get ("x"),
         Y      => Get ("y"),
         Width  => Get ("width"),
         Height => Get ("height"));
   end From_Config;

   ---------------
   -- To_Config --
   ---------------

   function To_Config (Rectangle : Layout_Rectangle)
                       return Tropos.Configuration
   is
      Config : Tropos.Configuration := Tropos.New_Config ("rectangle");
   begin
      Config.Add ("x", Integer (Rectangle.X));
      Config.Add ("y", Integer (Rectangle.Y));
      Config.Add ("width", Integer (Rectangle.Width));
      Config.Add ("height", Integer (Rectangle.Height));
      return Config;
   end To_Config;

end Komnenos;
