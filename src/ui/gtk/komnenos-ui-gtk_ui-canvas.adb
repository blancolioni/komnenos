with Ada.Numerics;

with Interfaces.C.Strings;

with Glib;

with Gdk.Event;
with Gdk.Window;

with Komnenos.Configuration;

with Komnenos.Colours.Cairo_Colours;
with Komnenos.UI.Cairo_UI;

package body Komnenos.UI.Gtk_UI.Canvas is

   function Draw_Area_Draw_Handler
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Cr     : Cairo.Cairo_Context)
      return Boolean;

   function Draw_Area_Configure_Handler
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event : Gdk.Event.Gdk_Event_Configure)
      return Boolean;

   ------------------------
   -- Create_Canvas_View --
   ------------------------

   function Create_Canvas_View
     (Fragment : Komnenos.Fragments.Fragment_Type)
      return Komnenos_Canvas_View
   is
      Result : constant Komnenos_Canvas_View :=
                 new Komnenos_Canvas_View_Record;
   begin

      Gtk.Drawing_Area.Initialize (Result);
      Result.Fragment := Fragment;
      Result.Fragment.Set_Canvas (Result);

      Result.Draw_Area := Gtk.Drawing_Area.Gtk_Drawing_Area (Result);

      Result.Draw_Area.Set_Size_Request
        (Width  => Glib.Gint (Fragment.Width),
         Height => Glib.Gint (Fragment.Height));

      declare
         use Gdk.Event;
      begin
         Result.Draw_Area.Add_Events
           (Button_Press_Mask or Button_Release_Mask
            or Pointer_Motion_Mask or Key_Press_Mask or Key_Release_Mask);
      end;

--        Text_View_Event_Handler.Connect
--          (Result.Text, Gtk.Widget.Signal_Button_Release_Event,
--           Text_View_Event_Handler.To_Marshaller
--             (Text_View_Button_Release_Handler'Access),
--           Result);
--
--        Text_View_Event_Handler.Connect
--          (Result.Text, Gtk.Widget.Signal_Button_Press_Event,
--           Text_View_Event_Handler.To_Marshaller
--             (Text_View_Button_Press_Handler'Access),
--           Result);
--
--        Text_View_Event_Handler.Connect
--          (Result.Text, Gtk.Widget.Signal_Motion_Notify_Event,
--           Text_View_Event_Handler.To_Marshaller
--             (Text_View_Motion_Handler'Access),
--           Result);

      Result.Draw_Area.On_Configure_Event
        (Draw_Area_Configure_Handler'Access);
      Result.Draw_Area.On_Draw
        (Draw_Area_Draw_Handler'Access);

--        Result.Text.On_Key_Press_Event
--          (Text_View_Key_Press'Access,
--           Result);

--        Result.Buffer := Result.Text.Get_Buffer;

      --        Result.Set_Min_Content_Height (Glib.Gint (Fragment.Height));
      --        Result.Set_Min_Content_Width (Glib.Gint (Fragment.Width));

      --        Result.Add (Result.Text);

      return Result;

   end Create_Canvas_View;

   ---------------------------------
   -- Draw_Area_Configure_Handler --
   ---------------------------------

   function Draw_Area_Configure_Handler
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Configure)
      return Boolean
   is
      use type Cairo.Cairo_Surface;
      Canvas : constant Komnenos_Canvas_View :=
                 Komnenos_Canvas_View (Widget);
   begin
      if Canvas.Layers (Komnenos.Displays.Base) /= Cairo.Null_Surface then
         for Surface of Canvas.Layers loop
            Cairo.Surface_Destroy (Surface);
         end loop;
      end if;

      for Surface of Canvas.Layers loop
         Surface :=
           Gdk.Window.Create_Similar_Surface
             (Self    => Widget.Get_Window,
              Content => Cairo.Cairo_Content_Color_Alpha,
              Width   => Event.Width,
              Height  => Event.Height);
      end loop;

      Canvas.Fragment.Invalidate;

      return True;
   end Draw_Area_Configure_Handler;

   ----------------------------
   -- Draw_Area_Draw_Handler --
   ----------------------------

   function Draw_Area_Draw_Handler
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Cr     : Cairo.Cairo_Context)
      return Boolean
   is
   begin
      for Surface of Komnenos_Canvas_View_Record (Widget.all).Layers loop
         Cairo.Set_Source_Surface
           (Cr, Surface, 0.0, 0.0);
         Cairo.Paint (Cr);
      end loop;
      return True;
   end Draw_Area_Draw_Handler;

   ---------------
   -- Draw_Line --
   ---------------

   overriding procedure Draw_Line
     (Canvas    : in out Komnenos_Canvas_View_Record;
      Layer     : Komnenos.Displays.Canvas_Layer;
      Line      : Layout_Line;
      Width     : Pixel_Length;
      Colour    : Komnenos.Colours.Komnenos_Colour;
      Curved    : Boolean;
      Arrow     : Boolean)
   is
      use Glib;
      Cr           : constant Cairo.Cairo_Context :=
                       Cairo.Create (Canvas.Layers (Layer));
      Config       : constant Komnenos.Configuration.Diagram_Config :=
                       Komnenos.Configuration.Get_Diagram_Config;
      Xs           : array (Line'Range) of Gdouble;
      Ys           : array (Line'Range) of Gdouble;

   begin

      if Curved and then Line'Length > 4 then
         Draw_Line (Canvas, Layer, Line (Line'First .. Line'First + 3),
                    Width, Colour, Curved, False);
         Draw_Line (Canvas, Layer, Line (Line'First + 3 .. Line'Last),
                    Width, Colour, Curved, Arrow);
         return;
      end if;

      Cairo.Set_Line_Width (Cr, Gdouble (Width));
      Komnenos.Colours.Cairo_Colours.Set_Source_Rgb (Cr, Colour);

      Cairo.Set_Line_Width (Cr, Glib.Gdouble (Config.Connector_Width));
      Cairo.Set_Line_Cap (Cr, Cairo.Cairo_Line_Cap_Butt);
      Cairo.Set_Line_Join (Cr, Cairo.Cairo_Line_Join_Round);

      for I in Line'Range loop
         Xs (I) := Gdouble (Line (I).X);
         Ys (I) := Gdouble (Line (I).Y);
      end loop;

      if Line'Length = 2 then
         Cairo.Move_To (Cr, Xs (Xs'First), Ys (Ys'First));
         Cairo.Line_To (Cr, Xs (Xs'Last), Ys (Ys'Last));
      elsif not Curved then
         for I in Xs'Range loop
            if I = Xs'First then
               Cairo.Move_To (Cr, Xs (I), Ys (I));
            else
               Cairo.Line_To (Cr, Xs (I), Ys (I));
            end if;
         end loop;
      elsif Line'Length = 3 then
         Cairo.Move_To (Cr, Xs (Xs'First), Ys (Ys'First));
         Cairo.Curve_To
           (Cr,
            Xs (Xs'First + 1), Ys (Ys'First + 1),
            Xs (Xs'First + 1), Ys (Ys'First + 1),
            Xs (Xs'First + 2), Ys (Ys'First + 2));
      else
         Cairo.Move_To (Cr, Xs (Xs'First), Ys (Ys'First));
         Cairo.Curve_To
           (Cr,
            Xs (Xs'First + 1), Ys (Ys'First + 1),
            Xs (Xs'First + 2), Ys (Ys'First + 2),
            Xs (Xs'First + 3), Ys (Ys'First + 3));
      end if;

      if Arrow then
         declare
            L  : constant Gdouble :=
                   Gdouble (Config.Arrow_Length);
            W  : constant Gdouble :=
                   Gdouble (Config.Arrow_Width);
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
                    (Cr, X + Offset, Y - W);
                  Cairo.Move_To (Cr, X, Y);
                  Cairo.Line_To
                    (Cr, X + Offset, Y + W);
               end;
            else
               declare
                  Offset : constant Gdouble :=
                             (if PY < Y then -L else L);
               begin
                  Cairo.Line_To
                    (Cr, X - W, Y + Offset);
                  Cairo.Move_To (Cr, X, Y);
                  Cairo.Line_To
                    (Cr, X + W, Y + Offset);
               end;
            end if;
         end;
      end if;
      Cairo.Stroke (Cr);
      Cairo.Destroy (Cr);
   end Draw_Line;

   --------------------
   -- Draw_Rectangle --
   --------------------

   overriding procedure Draw_Rectangle
     (Canvas            : in out Komnenos_Canvas_View_Record;
      Layer             : Komnenos.Displays.Canvas_Layer;
      Rectangle         : Layout_Rectangle;
      Border_Colour     : Komnenos.Colours.Komnenos_Colour;
      Background_Colour : Komnenos.Colours.Komnenos_Colour;
      Filled            : Boolean;
      Corner_Radius     : Pixel_Length)
   is
      Context : constant Cairo.Cairo_Context :=
                  Cairo.Create (Canvas.Layers (Layer));
      Config    : constant Komnenos.Configuration.Diagram_Config :=
                    Komnenos.Configuration.Get_Diagram_Config;
   begin
      Cairo.Set_Line_Width
        (Context, Glib.Gdouble (Config.Node_Border_Width));

      Cairo.Set_Line_Cap (Context, Cairo.Cairo_Line_Cap_Butt);
      Cairo.Set_Line_Join (Context, Cairo.Cairo_Line_Join_Round);

      Cairo_UI.Create_Rectangle_Path
        (Context       => Context,
         Rectangle     => Rectangle,
         Corner_Radius => Corner_Radius);

      if Filled then
         Komnenos.Colours.Cairo_Colours.Set_Source_Rgb
           (Context, Background_Colour);
         Cairo.Fill_Preserve (Context);
      end if;

      Komnenos.Colours.Cairo_Colours.Set_Source_Rgb
        (Context, Border_Colour);
      Cairo.Stroke (Context);

      Cairo.Destroy (Context);

   end Draw_Rectangle;

   ---------------
   -- Draw_Text --
   ---------------

   overriding procedure Draw_Text
     (Canvas    : in out Komnenos_Canvas_View_Record;
      Layer     : Komnenos.Displays.Canvas_Layer;
      Rectangle : Layout_Rectangle;
      Font      : Komnenos.Fonts.Komnenos_Font;
      Text      : String)
   is
      use Glib;
      Extents : aliased Cairo.Cairo_Text_Extents;
      Context : constant Cairo.Cairo_Context :=
                  Cairo.Create (Canvas.Layers (Layer));
      C_Text  : Interfaces.C.Strings.chars_ptr :=
                  Interfaces.C.Strings.New_String (Text);
   begin
      Komnenos.UI.Cairo_UI.Set_Font (Context, Font);
      Cairo.Text_Extents (Context, C_Text, Extents'Access);
      Cairo.Move_To
        (Context,
         Glib.Gdouble (Rectangle.X + Rectangle.Width / 2)
         - Extents.Width / 2.0,
         Glib.Gdouble (Rectangle.Y + Rectangle.Height / 2)
         + Extents.Height / 2.0);
      Cairo.Show_Text (Context, Text);
      Interfaces.C.Strings.Free (C_Text);
      Cairo.Destroy (Context);

   end Draw_Text;

   ----------------------
   -- Draw_Turtle_Path --
   ----------------------

   overriding procedure Draw_Turtle_Path
     (Canvas          : in out Komnenos_Canvas_View_Record;
      Layer           : Komnenos.Displays.Canvas_Layer;
      Start_Location  : Layout_Point;
      Start_Direction : Komnenos.Displays.Compass_Direction;
      Path            : Komnenos.Displays.Turtle_Path;
      Width           : Pixel_Length;
      Colour          : Komnenos.Colours.Komnenos_Colour;
      Arrow           : Boolean)
   is
      pragma Unreferenced (Arrow);
      use Glib;
      use Komnenos.Displays;
      Pi : constant := Ada.Numerics.Pi;
      X : Gdouble := Gdouble (Start_Location.X);
      Y : Gdouble := Gdouble (Start_Location.Y);
      D : Compass_Direction := Start_Direction;
      R : Gdouble;
      Cr : constant Cairo.Cairo_Context :=
             Cairo.Create (Canvas.Layers (Layer));
      Cos : constant array (Compass_Direction) of Gdouble :=
              (1.0, 0.0, -1.0, 0.0);
      Sin : constant array (Compass_Direction) of Gdouble :=
              (0.0, 1.0, 0.0, -1.0);
   begin
      Cairo.Set_Line_Width (Cr, Gdouble (Width));
      Cairo.Move_To (Cr, X, Y);
      Komnenos.Colours.Cairo_Colours.Set_Source_Rgb (Cr, Colour);
      for Command of Path loop
         R := Gdouble (Command.Length);
         case Command.Atom is
            when Move =>
               X := X + R * Cos (D);
               Y := Y - R * Sin (D);
               Cairo.Line_To (Cr, X, Y);
            when Turn =>
               declare
                  New_D : constant Compass_Direction := Command.Direction;
                  Xc    : constant Gdouble :=
                            X + R * Cos (New_D);
                  Yc    : constant Gdouble :=
                            Y - R * Sin (New_D);
                  A_1   : constant Gdouble :=
                            (case New_D is
                                when East   => Pi,
                                when West   => 0.0,
                                when North  => 3.0 * Pi / 2.0,
                                when South  => Pi / 2.0);
                  A_2   : constant Gdouble :=
                            (case D is
                                when North => Pi / 2.0,
                                when South => 3.0 * Pi / 2.0,
                                when East  => 0.0,
                                when West  => Pi);
                  Left_Turn : constant Boolean :=
                                (D = Compass_Direction'Last
                                 and then New_D = Compass_Direction'First)
                    or else (D /= Compass_Direction'Last
                             and then New_D = Compass_Direction'Succ (D));
               begin
                  if Command.Length > 0 then
                     if Left_Turn then
                        Cairo.Arc_Negative
                          (Cr     => Cr,
                           Xc     => Xc,
                           Yc     => Yc,
                           Radius => R,
                           Angle1 => 2.0 * Pi - A_1,
                           Angle2 => 2.0 * Pi - A_2);
                     else
                        Cairo.Arc
                          (Cr     => Cr,
                           Xc     => Xc,
                           Yc     => Yc,
                           Radius => R,
                           Angle1 => 2.0 * Pi - A_1,
                           Angle2 => 2.0 * Pi - A_2);
                     end if;

                     X := Xc + R * Cos (D);
                     Y := Yc - R * Sin (D);

                  end if;
                  D := New_D;
               end;
         end case;
      end loop;

      Cairo.Stroke (Cr);
      Cairo.Destroy (Cr);
   end Draw_Turtle_Path;

   ----------------------------
   -- Get_Bounding_Rectangle --
   ----------------------------

   overriding function Get_Bounding_Rectangle
     (Canvas    : Komnenos_Canvas_View_Record;
      Font      : Komnenos.Fonts.Komnenos_Font;
      Text      : String)
      return Layout_Rectangle
   is
      use Glib;
      Extents   : aliased Cairo.Cairo_Text_Extents;
      Context   : constant Cairo.Cairo_Context :=
                    Cairo.Create (Canvas.Layers (Komnenos.Displays.Base));
      C_Text    : Interfaces.C.Strings.chars_ptr :=
                    Interfaces.C.Strings.New_String (Text);
      Rectangle : Layout_Rectangle;
      Config    : constant Komnenos.Configuration.Diagram_Config :=
                    Komnenos.Configuration.Get_Diagram_Config;
   begin
      Komnenos.UI.Cairo_UI.Set_Font (Context, Font);
      Cairo.Text_Extents (Context, C_Text, Extents'Access);
      Rectangle :=
        (0, 0, Pixel_Length (Extents.Width), Pixel_Length (Extents.Height));
      declare
         W_Min : constant Pixel_Length :=
                   Pixel_Length'Max
                     (Pixel_Length (Extents.Width)
                      + 2 * Config.Node_Across_Margin,
                      Config.Min_Node_Width);
         D     : constant Pixel_Length :=
                   (if W_Min < Rectangle.Width then 0
                    else W_Min - Rectangle.Width);
      begin
         if D > 0 then
            Rectangle.X := Rectangle.X - D / 2;
            Rectangle.Width := Rectangle.Width + D;
         end if;
      end;

      declare
         H_Min : constant Pixel_Length :=
                   Pixel_Length'Max
                     (Pixel_Length (Extents.Height)
                      + 2 * Config.Node_Down_Margin,
                      Config.Min_Node_Height);
         D     : constant Pixel_Length :=
                   (if H_Min < Rectangle.Height then 0
                    else H_Min - Rectangle.Height);
      begin
         if D > 0 then
            Rectangle.Y := Rectangle.Y - D / 2;
            Rectangle.Height := Rectangle.Height + D;
         end if;
      end;

      Interfaces.C.Strings.Free (C_Text);

      return Rectangle;

   end Get_Bounding_Rectangle;

   -------------------------
   -- On_Fragment_Resized --
   -------------------------

   overriding procedure On_Fragment_Resized
     (Canvas : in out Komnenos_Canvas_View_Record)
   is
   begin
      Canvas.Draw_Area.Set_Size_Request
        (Width  => Glib.Gint (Canvas.Fragment.Width),
         Height => Glib.Gint (Canvas.Fragment.Height));
   end On_Fragment_Resized;

end Komnenos.UI.Gtk_UI.Canvas;
