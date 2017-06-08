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

   -----------
   -- Union --
   -----------

   function Union
     (Left, Right : Layout_Rectangle)
      return Layout_Rectangle
   is
   begin
      return Result : Layout_Rectangle := Left do
         if Result.X > Right.X then
            Result.Width := Result.Width + Result.X - Right.X;
            Result.X := Right.X;
         end if;
         if Result.Y > Right.Y then
            Result.Height := Result.Height + Result.Y - Right.Y;
            Result.Y := Right.Y;
         end if;
         if Result.X + Result.Width < Right.X + Right.Width then
            Result.Width := Right.X + Right.Width - Result.X;
         end if;
         if Result.Y + Result.Height < Right.Y + Right.Height then
            Result.Height := Right.Y + Right.Height - Result.Y;
         end if;
      end return;
   end Union;

end Komnenos;
