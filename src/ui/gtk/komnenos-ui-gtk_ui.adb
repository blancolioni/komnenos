with Ada.Numerics;
with Ada.Text_IO;

with Tropos;

with Komnenos.Connectors;
with Komnenos.Layouts;

with Komnenos.UI.Gtk_UI.Entity_Lists;
with Komnenos.UI.Gtk_UI.Layout_Table;
with Komnenos.UI.Gtk_UI.Navigation;

with Glib.Error;

with Gdk.Pixbuf;

with Gtk.Builder;
with Gtk.Drawing_Area;
with Gtk.GEntry;
with Gtk.Handlers;
with Gtk.Layout;
with Gtk.Main;
with Gtk.Scrolled_Window;
with Gtk.Style_Provider;
with Gtk.Tree_View;
with Gtk.Window;

with Gtkada.Style;

with Cairo;

package body Komnenos.UI.Gtk_UI is

   type Root_Gtk_UI is
     new Root_Komnenos_UI with
      record
         Top_Level       : Gtk.Window.Gtk_Window;
         Entity_Tree     : Gtk.Tree_View.Gtk_Tree_View;
         Entity_Filter   : Gtk.GEntry.Gtk_Entry;
         Navigation      : Komnenos.UI.Gtk_UI.Navigation.Gtk_Navigation_Panel;
         Layout          : Komnenos.UI.Gtk_UI.Layout_Table.Gtk_Layout_Table;
         Active          : Komnenos.Fragments.Fragment_Type := null;
      end record;

   type Gtk_UI_Access is access all Root_Gtk_UI'Class;

   overriding procedure Start (UI : in out Root_Gtk_UI);

   overriding procedure Place_Fragment
     (UI       : in out Root_Gtk_UI;
      Parent   : access Komnenos.Entities.Entity_Visual'Class;
      Offset   : Pixel_Offset;
      Fragment : Komnenos.Fragments.Fragment_Type);

   overriding procedure Remove_Fragment
     (UI       : in out Root_Gtk_UI;
      Fragment : Komnenos.Fragments.Fragment_Type)
   is null;

   overriding procedure Update_Visual
     (UI       : in out Root_Gtk_UI;
      Visual   : not null access Komnenos.Entities.Entity_Visual'Class)
   is null;

   overriding function Active_Fragment
     (UI : Root_Gtk_UI)
      return Komnenos.Fragments.Fragment_Type
   is (UI.Active);

   overriding procedure To_Config
     (UI     : Root_Gtk_UI;
      Config : in out Tropos.Configuration);

   overriding procedure From_Config
     (UI : not null access Root_Gtk_UI;
      Config : Tropos.Configuration);

   overriding function Get_Visual
     (UI  : Root_Gtk_UI;
      Key : String)
      return Komnenos.Entities.Entity_Visual_Access
   is (Komnenos.Entities.Entity_Visual_Access
         (UI.Layout.Find_Fragment (Key)));

   package Window_Callback is
      new Gtk.Handlers.Callback (Gtk.Window.Gtk_Window_Record);

   procedure Destroy_Handler (W : access Gtk.Window.Gtk_Window_Record'Class);

   procedure Draw_Rounded_Rectangle
     (Context       : Cairo.Cairo_Context;
      X, Y          : Glib.Gdouble;
      Width, Height : Glib.Gdouble;
      Radius        : Glib.Gdouble) with Unreferenced;

   ---------------
   -- Create_UI --
   ---------------

   function Create_UI
     (Config_Folder_Path : String)
      return Komnenos_UI
   is
      Result : constant Gtk_UI_Access := new Root_Gtk_UI;
      Builder : Gtk.Builder.Gtk_Builder;
   begin

      Result.View_Left := 0;
      Result.View_Top := 0;
      Result.View_Width := 400;
      Result.View_Height := 400;

      Gtk.Main.Init;

      declare
         Path  : constant String :=
                   Config_Folder_Path & "/komnenos.css";

         procedure Error (Message : String);

         -----------
         -- Error --
         -----------

         procedure Error (Message : String) is
         begin
            raise Program_Error with "Could not load " & Path & ": " & Message;
         end Error;

      begin
         Gtkada.Style.Load_Css_File
           (Path     => Path,
            Error    => Error'Access,
            Priority => Gtk.Style_Provider.Priority_Application);
      end;

      Gtk.Builder.Gtk_New (Builder);

      Ada.Text_IO.Put_Line ("komnenos: loading UI");

      declare
         use type Glib.Guint;
         Path  : constant String :=
                   Config_Folder_Path & "/komnenos.ui";
         Error : aliased Glib.Error.GError;
         Result : constant Glib.Guint :=
                    Builder.Add_From_File
                      (Filename => Path,
                       Error    => Error'Access);
      begin
         if Result = 0 then
            raise Program_Error with
              "Error opening GUI definition: " & Path
              & ": " & Glib.Error.Get_Message (Error);
         end if;
      end;

      Ada.Text_IO.Put_Line ("komnenos: creating main window");
      declare
         Main_Window : constant Gtk.Window.Gtk_Window :=
                         Gtk.Window.Gtk_Window
                           (Builder.Get_Object
                              ("Top_Window"));
         Pixbuf      : Gdk.Pixbuf.Gdk_Pixbuf;
         Error       : Glib.Error.GError;
      begin

         Gdk.Pixbuf.Gdk_New_From_File
           (Pixbuf,
            Config_Folder_Path & "/komnenos.png",
            Error);
         Main_Window.Set_Icon (Pixbuf);
         Result.Top_Level := Main_Window;
         Window_Callback.Connect
           (Main_Window,
            "destroy",
            Window_Callback.To_Marshaller (Destroy_Handler'Access));
         Main_Window.Maximize;
      end;

      Result.Entity_Filter :=
        Gtk.GEntry.Gtk_Entry
          (Builder.Get_Object ("Entity_Filter"));

      Result.Entity_Tree :=
        Gtk.Tree_View.Gtk_Tree_View
          (Builder.Get_Object ("Entity_Tree"));

      declare
         Navigation_Draw : constant Gtk.Drawing_Area.Gtk_Drawing_Area :=
                             Gtk.Drawing_Area.Gtk_Drawing_Area
                               (Builder.Get_Object
                                  ("Navigation_Pane"));
         Main_Scroll     : constant Gtk.Scrolled_Window.Gtk_Scrolled_Window :=
                             Gtk.Scrolled_Window.Gtk_Scrolled_Window
                               (Builder.Get_Object ("Main_Scroll"));
         Main_Layout     : constant Gtk.Layout.Gtk_Layout :=
                             Gtk.Layout.Gtk_Layout
                               (Builder.Get_Object ("Main_Layout"));
      begin
         Result.Navigation :=
           Komnenos.UI.Gtk_UI.Navigation.Create_Navigation_Panel
             (Navigation_Draw);
         Result.Layout :=
           Komnenos.UI.Gtk_UI.Layout_Table.Create_Layout_Table
             (Navigation    => Result.Navigation,
              Main_Scroll   => Main_Scroll,
              Main_Layout   => Main_Layout);
      end;

      return Komnenos_UI (Result);

   end Create_UI;

   ---------------------
   -- Destroy_Handler --
   ---------------------

   procedure Destroy_Handler
     (W : access Gtk.Window.Gtk_Window_Record'Class)
   is
      pragma Unreferenced (W);
   begin
      Gtk.Main.Main_Quit;
   end Destroy_Handler;

   ----------------------------
   -- Draw_Rounded_Rectangle --
   ----------------------------

   procedure Draw_Rounded_Rectangle
     (Context       : Cairo.Cairo_Context;
      X, Y          : Glib.Gdouble;
      Width, Height : Glib.Gdouble;
      Radius        : Glib.Gdouble)
   is
      use Glib;
      R  : constant Gdouble := Radius;
      Pi : constant := Ada.Numerics.Pi;
      X1 : constant Gdouble := X + R;
      X2 : constant Gdouble := X + Width - R;
      Y1 : constant Gdouble := Y + R;
      Y2 : constant Gdouble := Y + Height - R;
   begin
      Cairo.New_Path (Context);
      Cairo.Arc (Context, X1, Y1, R, 2.0 * Pi / 2.0, 3.0 * Pi / 2.0);
      Cairo.Arc (Context, X2, Y1, R, 3.0 * Pi / 2.0, 4.0 * Pi / 2.0);
      Cairo.Arc (Context, X2, Y2, R, 0.0 * Pi / 2.0, 1.0 * Pi / 2.0);
      Cairo.Arc (Context, X1, Y2, R, 1.0 * Pi / 2.0, 2.0 * Pi / 2.0);
      Cairo.Close_Path (Context);
   end Draw_Rounded_Rectangle;

   -----------------
   -- From_Config --
   -----------------

   overriding procedure From_Config
     (UI : not null access Root_Gtk_UI;
      Config : Tropos.Configuration)
   is
      procedure Attach_Connection
        (Connection : Komnenos.Connectors.Connector_Type);

      procedure Attach_Entity
        (Fragment : Komnenos.Fragments.Fragment_Type);

      -----------------------
      -- Attach_Connection --
      -----------------------

      procedure Attach_Connection
        (Connection : Komnenos.Connectors.Connector_Type)
      is
      begin
         Connection.Update;
         UI.Layout.Add_Connection_Widget (Connection);
      end Attach_Connection;

      -------------------
      -- Attach_Entity --
      -------------------

      procedure Attach_Entity
        (Fragment : Komnenos.Fragments.Fragment_Type)
      is
         use type Komnenos.Entities.Entity_Reference;
         Table : constant Komnenos.Entities.Entity_Table_Access :=
                   UI.Main_Table;
         Entity : constant Komnenos.Entities.Entity_Reference :=
                    (if Table.Exists (Fragment.Entity_Key)
                     then Table.Get (Fragment.Entity_Key)
                     else null);
      begin
         if Entity /= null then
            Entity.Select_Entity
              (Table  => Table,
               Parent => null,
               Visual => Fragment,
               Offset => 0);
            UI.Layout.Item_Placed (Fragment);
         end if;
      end Attach_Entity;

   begin
      Root_Komnenos_UI (UI.all).From_Config (Config);
      if Config.Contains ("layout") then
         UI.Layout.From_Config (Config.Child ("layout"));
      end if;
      UI.Layout.Scan (Attach_Entity'Access);
      UI.Layout.Scan_Connections (Attach_Connection'Access);

      if Config.Contains ("connections") then
         for Conn_Config of Config.Child ("connections") loop
            declare
               Source_Key : constant String :=
                              Conn_Config.Get ("source");
               Dest_Key   : constant String :=
                              Conn_Config.Get ("destination");
               Source_Offset : constant Natural :=
                                 Conn_Config.Get ("source-offset");
               Dest_Offset   : constant Natural :=
                                 Conn_Config.Get ("destination-offset");
               Class         : constant Komnenos.Connectors.Connector_Class :=
                                 Komnenos.Connectors.Connector_Class'Value
                                   (Conn_Config.Get ("class"));
               Source        : constant Komnenos.Fragments.Fragment_Type :=
                                 UI.Layout.Find_Fragment (Source_Key);
               Dest          : constant Komnenos.Fragments.Fragment_Type :=
                                 UI.Layout.Find_Fragment (Dest_Key);

               Connection    : constant Komnenos.Connectors.Connector_Type :=
                                 Komnenos.Connectors.Connect
                                   (Class              => Class,
                                    Source             => Source,
                                    Source_Offset      =>
                                      Pixel_Position (Source_Offset),
                                    Destination        => Dest,
                                    Destination_Offset =>
                                      Pixel_Position (Dest_Offset));
            begin
               UI.Layout.Connection (Connection);
            end;
         end loop;
      end if;
   end From_Config;

   --------------------
   -- Place_Fragment --
   --------------------

   overriding procedure Place_Fragment
     (UI       : in out Root_Gtk_UI;
      Parent   : access Komnenos.Entities.Entity_Visual'Class;
      Offset   : Pixel_Offset;
      Fragment : Komnenos.Fragments.Fragment_Type)
   is
      use Komnenos.Fragments;
   begin
      if UI.Active = null then
         UI.Active := Fragment;
      end if;
      if Parent = null then
         Fragment.Set_Position (Fragment.X + UI.View_Left,
                                Fragment.Y + UI.View_Top);
         UI.Layout.Place_Item (Fragment);
      else
         UI.Layout.Place_Item
           (Fragment, Komnenos.Fragments.Fragment_Type (Parent), Offset);

         declare
            Connector : constant Komnenos.Connectors.Connector_Type :=
                          Komnenos.Connectors.Connect
                            (Class              => Komnenos.Connectors.Arrow,
                             Source             => Parent,
                             Source_Offset      => Offset,
                             Destination        => Fragment,
                             Destination_Offset => 0);
         begin
            UI.Layout.Connection (Connector);
         end;

      end if;
   end Place_Fragment;

   -----------
   -- Start --
   -----------

   overriding procedure Start (UI : in out Root_Gtk_UI) is
   begin

      Entity_Lists.Create (UI.Entity_Tree, UI.Entity_Filter);

      UI.Top_Level.Show_All;

      Ada.Text_IO.Put_Line ("komnenos: showing UI");

      Gtk.Main.Main;
   end Start;

   ---------------
   -- To_Config --
   ---------------

   overriding procedure To_Config
     (UI     : Root_Gtk_UI;
      Config : in out Tropos.Configuration)
   is
      Layout_Config : Tropos.Configuration :=
                        Tropos.New_Config ("layout");
   begin
      Root_Komnenos_UI (UI).To_Config (Config);
      UI.Layout.To_Config (Layout_Config);
      Config.Add (Layout_Config);
   end To_Config;

   -------------
   -- To_RGBA --
   -------------

   function To_RGBA
     (Colour_Spec : String)
      return Gdk.RGBA.Gdk_RGBA
   is
      Success : Boolean;
      Result  : Gdk.RGBA.Gdk_RGBA;
   begin
      Gdk.RGBA.Parse (Result, Colour_Spec, Success);
      if Success then
         return Result;
      else
         return Gdk.RGBA.Null_RGBA;
      end if;
   end To_RGBA;

   -------------------
   -- Update_Visual --
   -------------------

--     overriding procedure Update_Visual
--       (UI       : in out Root_Gtk_UI;
--        Visual   : not null access Komnenos.Entities.Entity_Visual'Class)
--     is
--        use Komnenos.Entities;
--        V : constant Entity_Visual_Access := Entity_Visual_Access (Visual);
--     begin
--        for Connector of UI.Connectors loop
--           if Connector.Source = V or else Connector.Destination = V then
--              Connector.Update;
--              Connector.Display.Update;
--              UI.Layout.Update_Connector (Connector);
--           end if;
--        end loop;
--     end Update_Visual;

end Komnenos.UI.Gtk_UI;
