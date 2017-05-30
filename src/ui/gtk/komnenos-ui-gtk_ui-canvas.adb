with Ada.Text_IO;

with Glib;

with Gdk.Event;
with Gdk.Window;

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

      Ada.Text_IO.Put_Line
        ("gtk: new canvas view");

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
      if Canvas.Surface /= Cairo.Null_Surface then
         Cairo.Surface_Destroy (Canvas.Surface);
      end if;

      Canvas.Surface :=
        Gdk.Window.Create_Similar_Surface
          (Self    => Widget.Get_Window,
           Content => Cairo.Cairo_Content_Color_Alpha,
           Width   => Event.Width,
           Height  => Event.Height);

      Ada.Text_IO.Put_Line
        ("canvas:" & Integer'Image (Integer (Event.Width))
         & " x" & Integer'Image (Integer (Event.Height)));

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
      Ada.Text_IO.Put_Line
        ("gtk: canvas: draw");
      Cairo.Set_Source_Surface
        (Cr, Komnenos_Canvas_View_Record (Widget.all).Surface,
         0.0, 0.0);
      Cairo.Paint (Cr);
      return True;
   end Draw_Area_Draw_Handler;

   ---------------
   -- Draw_Line --
   ---------------

   overriding procedure Draw_Line
     (Canvas    : in out Komnenos_Canvas_View_Record;
      Line      : Layout_Line;
      Colour    : Komnenos.Colours.Komnenos_Colour;
      Curved    : Boolean;
      Arrow     : Boolean)
   is null;

   --------------------
   -- Draw_Rectangle --
   --------------------

   overriding procedure Draw_Rectangle
     (Canvas            : in out Komnenos_Canvas_View_Record;
      Rectangle         : Layout_Rectangle;
      Border_Colour     : Komnenos.Colours.Komnenos_Colour;
      Background_Colour : Komnenos.Colours.Komnenos_Colour;
      Filled            : Boolean;
      Corner_Radius     : Pixel_Length)
   is
      Context : constant Cairo.Cairo_Context :=
                  Cairo.Create (Canvas.Surface);
   begin
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
      Top_Left  : Layout_Point;
      Font      : Komnenos.Fonts.Komnenos_Font;
      Text      : String)
   is null;

end Komnenos.UI.Gtk_UI.Canvas;
