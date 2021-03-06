with Ada.Numerics;

with Glib;

package body Komnenos.UI.Cairo_UI is

   Line_Width   : constant Positive := 2;
   Arrow_Length : constant Positive := 10;
   Arrow_Width  : constant Positive := 5;

   procedure Draw_Rounded_Rectangle
     (Cr            : Cairo.Cairo_Context;
      X, Y          : Glib.Gdouble;
      Width, Height : Glib.Gdouble;
      Aspect        : Glib.Gdouble;
      Corner_Radius : Glib.Gdouble);

   ----------------------
   -- Create_Line_Path --
   ----------------------

   procedure Create_Line_Path
     (Context : Cairo.Cairo_Context;
      Line    : Layout_Line;
      Curved  : Boolean;
      Arrow   : Boolean)
   is
      use Glib;
      Xs : array (Line'Range) of Gdouble;
      Ys : array (Line'Range) of Gdouble;
   begin

      Cairo.Set_Line_Width (Context, Glib.Gdouble (Line_Width));
      Cairo.Set_Line_Cap (Context, Cairo.Cairo_Line_Cap_Butt);
      Cairo.Set_Line_Join (Context, Cairo.Cairo_Line_Join_Round);

      for I in Line'Range loop
         declare
            X : constant Glib.Gdouble :=
                  Glib.Gdouble
                    (Line (I).X);
            Y : constant Glib.Gdouble :=
                  Glib.Gdouble
                    (Line (I).Y);
         begin
            Xs (I) := X;
            Ys (I) := Y;
         end;
      end loop;

      if Line'Length = 2 then
         Cairo.Move_To (Context, Xs (Xs'First), Ys (Ys'First));
         Cairo.Line_To (Context, Xs (Xs'Last), Ys (Ys'Last));
      elsif not Curved then
         for I in Xs'Range loop
            if I = Xs'First then
               Cairo.Move_To (Context, Xs (I), Ys (I));
            else
               Cairo.Line_To (Context, Xs (I), Ys (I));
            end if;
         end loop;
      elsif Line'Length = 3 then
         Cairo.Move_To (Context, Xs (Xs'First), Ys (Ys'First));
         Cairo.Curve_To
           (Context,
            Xs (Xs'First + 1), Ys (Ys'First + 1),
            Xs (Xs'First + 1), Ys (Ys'First + 1),
            Xs (Xs'First + 2), Ys (Ys'First + 2));
      else
         for I in 1 .. Xs'Last - 3 loop
            Cairo.Move_To (Context, Xs (I), Ys (I));
            Cairo.Curve_To
              (Context,
               Xs (I + 1), Ys (I + 1),
               Xs (I + 2), Ys (I + 2),
               Xs (I + 3), Ys (I + 3));
         end loop;
      end if;

      if Arrow then
         declare
            L  : constant Gdouble :=
                   Gdouble (Arrow_Length);
            W  : constant Gdouble :=
                   Gdouble (Arrow_Width);
            X  : constant Gdouble := Xs (Xs'Last);
            Y  : constant Gdouble := Ys (Ys'Last);
            PX : constant Gdouble := Xs (Xs'Last - 1);
            PY : constant Gdouble := Ys (Ys'Last - 1);
         begin
            if PX /= X then
               declare
                  Offset : constant Gdouble :=
                             (if PX < X then -L else L);
               begin
                  Cairo.Line_To
                    (Context, X + Offset, Y - W);
                  Cairo.Move_To (Context, X, Y);
                  Cairo.Line_To
                    (Context, X + Offset, Y + W);
               end;
            else
               declare
                  Offset : constant Gdouble :=
                             (if PY < Y then -L else L);
               begin
                  Cairo.Line_To
                    (Context, X - W, Y + Offset);
                  Cairo.Move_To (Context, X, Y);
                  Cairo.Line_To
                    (Context, X + W, Y + Offset);
               end;
            end if;
         end;
      end if;
   end Create_Line_Path;

   ---------------------------
   -- Create_Rectangle_Path --
   ---------------------------

   procedure Create_Rectangle_Path
     (Context       : Cairo.Cairo_Context;
      Rectangle     : Layout_Rectangle;
      Corner_Radius : Pixel_Length)
   is
      use Glib;
   begin
      if Corner_Radius = 0 then
         Cairo.Rectangle
           (Cr     => Context,
            X      => Gdouble (Rectangle.X),
            Y      => Gdouble (Rectangle.Y),
            Width  => Gdouble (Rectangle.Width),
            Height => Gdouble (Rectangle.Height));
      else
         Draw_Rounded_Rectangle
           (Cr            => Context,
            X             => Gdouble (Rectangle.X),
            Y             => Gdouble (Rectangle.Y),
            Width         => Gdouble (Rectangle.Width),
            Height        => Gdouble (Rectangle.Height),
            Aspect        => 1.0,
            Corner_Radius => Gdouble (Corner_Radius));
      end if;
   end Create_Rectangle_Path;

   ----------------------------
   -- Draw_Rounded_Rectangle --
   ----------------------------

   procedure Draw_Rounded_Rectangle
     (Cr            : Cairo.Cairo_Context;
      X, Y          : Glib.Gdouble;
      Width, Height : Glib.Gdouble;
      Aspect        : Glib.Gdouble;
      Corner_Radius : Glib.Gdouble)
   is
      use Glib;
      Pi : constant := Ada.Numerics.Pi;
      Radius : constant Gdouble := Corner_Radius / Aspect;
   begin
      Cairo.New_Sub_Path (Cr);
      Cairo.Arc (Cr, X + Width - Radius, Y + Radius, Radius,
                 -Pi / 2.0, 0.0);
      Cairo.Arc (Cr, X + Width - Radius, Y + Height - Radius, Radius,
                 0.0, Pi / 2.0);
      Cairo.Arc (Cr, X + Radius, Y + Height - Radius, Radius,
                 Pi / 2.0, Pi);
      Cairo.Arc (Cr, X + Radius, Y + Radius, Radius,
                 Pi, Pi * 1.5);
      Cairo.Close_Path (Cr);
   end Draw_Rounded_Rectangle;

   --------------
   -- Set_Font --
   --------------

   procedure Set_Font
     (Context : Cairo.Cairo_Context;
      Font    : Komnenos.Fonts.Komnenos_Font)
   is
   begin
      Cairo.Select_Font_Face
        (Cr     => Context,
         Family => Font.Name,
         Slant  => (if Font.Is_Italic
                    then Cairo.Cairo_Font_Slant_Italic
                    else Cairo.Cairo_Font_Slant_Normal),
         Weight => (if Font.Is_Bold
                    then Cairo.Cairo_Font_Weight_Bold
                    else Cairo.Cairo_Font_Weight_Normal));
      Cairo.Set_Font_Size
        (Context, Glib.Gdouble (Font.Size));
   end Set_Font;

end Komnenos.UI.Cairo_UI;
