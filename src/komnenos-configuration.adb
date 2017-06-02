with Tropos.Reader;

with Komnenos.Paths;

package body Komnenos.Configuration is

   Komnenos_Config : Tropos.Configuration;
   Have_Config     : Boolean := False;

   Local_Diagram_Config : Diagram_Config;

   procedure Check_Configuration;
   procedure Load_Diagram_Config;

   -------------------------
   -- Check_Configuration --
   -------------------------

   procedure Check_Configuration is
   begin
      if not Have_Config then
         Komnenos_Config :=
           Tropos.Reader.Read_Config
             (Komnenos.Paths.Config_File
                ("komnenos.config"));
         Load_Diagram_Config;
         Have_Config := True;
      end if;
   exception
      when others =>
         raise Program_Error with
           "fatal: cannot open komnenos.config";
   end Check_Configuration;

   -------------
   -- Enabled --
   -------------

   function Enabled (Setting_Name : String) return Boolean is
   begin
      Check_Configuration;
      if Komnenos_Config.Contains ("settings") then
         return Komnenos_Config.Child ("settings").Get (Setting_Name);
      else
         return False;
      end if;
   end Enabled;

   ----------------
   -- Get_Colour --
   ----------------

   function Get_Colour
     (Principle_Name : String;
      Secondary_Name : String := "")
      return Komnenos.Colours.Komnenos_Colour
   is
   begin
      Check_Configuration;

      declare
         Colour_Config : constant Tropos.Configuration :=
                           Komnenos_Config.Child ("colours");
         Colour        : constant String :=
                           (if Colour_Config.Contains (Principle_Name)
                            then Colour_Config.Get (Principle_Name)
                            else Colour_Config.Get (Secondary_Name, "purple"));
      begin
         return Komnenos.Colours.From_String (Colour);
      end;

   end Get_Colour;

   ---------------------------
   -- Get_Connector_Metrics --
   ---------------------------

   procedure Get_Connector_Metrics
     (Class_Name   : String;
      Colour       : out Komnenos.Colours.Komnenos_Colour;
      Curved       : out Boolean;
      Line_Width   : out Positive;
      Arrow_Length : out Positive;
      Arrow_Width  : out Positive)
   is
   begin
      Check_Configuration;

      if Komnenos_Config.Contains ("connectors") then
         declare
            Connector_Config : constant Tropos.Configuration :=
                                 Komnenos_Config.Child ("connectors");
            Config           : constant Tropos.Configuration :=
                                 (if Connector_Config.Contains (Class_Name)
                                  then Connector_Config.Child (Class_Name)
                                  else Connector_Config.Child ("default"));
         begin
            Colour :=
              Komnenos.Colours.From_String
                (Config.Get ("colour", "pink"));
            Line_Width := Config.Get ("line_width", 2);
            Arrow_Length := Config.Get ("arrow_length", 8);
            Arrow_Width := Config.Get ("arrow_width", 4);
            Curved := Config.Get ("curved");
         end;
      else
         Colour :=
           Komnenos.Colours.From_String ("pink");
         Line_Width := 2;
         Arrow_Length := 8;
         Arrow_Width := 4;
         Curved := True;
      end if;
   end Get_Connector_Metrics;

   ------------------------
   -- Get_Diagram_Config --
   ------------------------

   function Get_Diagram_Config return Diagram_Config is
   begin
      Check_Configuration;

      return Local_Diagram_Config;
   end Get_Diagram_Config;

   procedure Load_Diagram_Config is
   begin
      Local_Diagram_Config := Diagram_Config'
        (Node_Label_Font      =>
           Komnenos.Fonts.Create_Font
             ("Courier", 12),
         Node_Border_Width    => 2,
         Node_Border_Colour   => Komnenos.Colours.From_String ("black"),
         Node_Selected_Colour => Komnenos.Colours.From_String ("blue"),
         Connector_Width      => 2,
         Connector_Colour     => Komnenos.Colours.From_String ("black"),
         Connector_Corner_Radius => 15,
         Arrow_Length         => 8,
         Arrow_Width          => 6,
         Min_Node_Width       => 30,
         Min_Node_Height      => 30,
         Node_Across_Margin   => 4,
         Node_Down_Margin     => 3,
         Visible_Node_Gap     => 30,
         Internal_Node_Gap    => 20,
         Layout_Row_Size      => 50,
         Debug_Layout         => False);

      if Komnenos_Config.Contains ("diagram") then
         declare
            Config : constant Tropos.Configuration :=
                       Komnenos_Config.Child ("diagram");
         begin
            Local_Diagram_Config.Debug_Layout := Config.Get ("debug_layout");
            if Config.Contains ("label_font") then
               declare
                  Font_Config : constant Tropos.Configuration :=
                                  Config.Child ("label_font");
               begin
                  Local_Diagram_Config.Node_Label_Font :=
                    Komnenos.Fonts.Create_Font
                      (Name       => Font_Config.Get ("family"),
                       Size       => Font_Config.Get ("size"),
                       Foreground =>
                         Komnenos.Colours.From_String
                           (Font_Config.Get ("colour", "black")),
                       Bold       =>
                         Font_Config.Get ("bold"),
                       Italic     =>
                         Font_Config.Get ("italic"),
                       Underlined =>
                         Font_Config.Get ("underline"));
               end;
            end if;
            if Config.Contains ("node") then
               declare
                  Node_Config : constant Tropos.Configuration :=
                                  Config.Child ("node");
               begin
                  if Node_Config.Contains ("margin") then
                     Local_Diagram_Config.Node_Across_Margin :=
                       Pixel_Length (Natural'
                                       (Node_Config.Child ("margin").Get (1)));
                     Local_Diagram_Config.Node_Down_Margin :=
                       Pixel_Length (Natural'
                                       (Node_Config.Child ("margin").Get (2)));
                  end if;
                  if Node_Config.Contains ("minimum_size") then
                     Local_Diagram_Config.Min_Node_Width :=
                       Pixel_Length
                         (Natural'
                            (Node_Config.Child ("minimum_size").Get (1)));
                     Local_Diagram_Config.Min_Node_Height :=
                       Pixel_Length
                         (Natural'
                            (Node_Config.Child ("minimum_size").Get (2)));
                  end if;
                  if Node_Config.Contains ("gap") then
                     Local_Diagram_Config.Visible_Node_Gap :=
                       Pixel_Length (Natural'
                                       (Node_Config.Child ("gap")
                                        .Get ("visible", 30)));
                     Local_Diagram_Config.Internal_Node_Gap :=
                       Pixel_Length (Natural'
                                       (Node_Config.Child ("gap")
                                        .Get ("internal", 10)));
                  end if;
                  if Node_Config.Contains ("border") then
                     Local_Diagram_Config.Node_Border_Width :=
                       Pixel_Length
                         (Natural'
                            (Node_Config.Child ("border").Get ("width", 2)));
                     Local_Diagram_Config.Node_Border_Colour :=
                       Komnenos.Colours.From_String
                         (Node_Config.Child ("border").Get
                          ("colour", "black"));
                     Local_Diagram_Config.Node_Selected_Colour :=
                       Komnenos.Colours.From_String
                         (Node_Config.Child ("border").Get
                          ("colour", "blue"));
                  end if;

                  Local_Diagram_Config.Layout_Row_Size :=
                    Pixel_Length (Natural'(Node_Config.Get ("row_size", 60)));

               end;
            end if;
            if Config.Contains ("connector") then
               declare
                  Connector_Config : constant Tropos.Configuration :=
                                       Config.Child ("connector");
               begin
                  Local_Diagram_Config.Connector_Width :=
                    Pixel_Length
                      (Natural'
                         (Connector_Config.Get ("line_width", 2)));
                  Local_Diagram_Config.Arrow_Width :=
                    Pixel_Length
                      (Natural'
                         (Connector_Config.Get ("arrow_width", 4)));
                  Local_Diagram_Config.Arrow_Length :=
                    Pixel_Length
                      (Natural'
                         (Connector_Config.Get ("arrow_length", 4)));
                  Local_Diagram_Config.Connector_Corner_Radius :=
                    Pixel_Length
                      (Natural'
                         (Connector_Config.Get ("corner_radius", 10)));
               end;
            end if;
         end;
      end if;
   end Load_Diagram_Config;

end Komnenos.Configuration;
