with Komnenos.Colours;
with Komnenos.Fonts;

package body Komnenos.Themes is

   function Get_Colour
     (Config     : Tropos.Configuration;
      Child_Name : String;
      Default_Colour : Komnenos.Colours.Komnenos_Colour)
      return Komnenos.Colours.Komnenos_Colour with Unreferenced;

   -----------------------
   -- Default_Font_Name --
   -----------------------

   function Default_Font_Name
     (Theme : Komnenos_Root_Theme'Class)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Theme.Default_Font_Name);
   end Default_Font_Name;

   -----------------------
   -- Default_Font_Size --
   -----------------------

   function Default_Font_Size
     (Theme : Komnenos_Root_Theme'Class)
      return Natural
   is
   begin
      return Theme.Default_Font_Size;
   end Default_Font_Size;

   ------------------------
   -- Default_Link_Style --
   ------------------------

   function Default_Link_Style
     (Theme : Komnenos_Root_Theme'Class)
      return Komnenos.Styles.Komnenos_Style
   is
   begin
      return Theme.Default_Link_Style;
   end Default_Link_Style;

   -------------------
   -- Default_Style --
   -------------------

   function Default_Style
     (Theme : Komnenos_Root_Theme'Class)
      return Komnenos.Styles.Komnenos_Style
   is
   begin
      return Theme.Default_Style;
   end Default_Style;

   ----------------
   -- Get_Colour --
   ----------------

   function Get_Colour
     (Config     : Tropos.Configuration;
      Child_Name : String;
      Default_Colour : Komnenos.Colours.Komnenos_Colour)
      return Komnenos.Colours.Komnenos_Colour
   is
   begin
      if not Config.Contains (Child_Name) then
         return Default_Colour;
      end if;

      declare
         Color_Config : constant Tropos.Configuration :=
                          Config.Child (Child_Name);
      begin
         if Color_Config.Contains ("r")
           or else Color_Config.Contains ("g")
           or else Color_Config.Contains ("b")
         then
            declare
               R : constant Integer := Color_Config.Get ("r", 0);
               G : constant Integer := Color_Config.Get ("g", 0);
               B : constant Integer := Color_Config.Get ("b", 0);
            begin
               return Komnenos.Colours.From_RGB (R, G, B);
            end;
         else
            return Komnenos.Colours.From_String
              (Color_Config.Config_Name);
         end if;
      end;
   end Get_Colour;

   ----------------
   -- Load_Theme --
   ----------------

   function Load_Theme
     (Theme_Config : Tropos.Configuration)
      return Komnenos_Theme
   is
      Result : Komnenos_Root_Theme;
      Fixed_Font : constant Tropos.Configuration :=
                     Theme_Config.Child ("fixed_font");
   begin
      Result.Set_Name (Theme_Config.Config_Name);

      for Child of Theme_Config.Child ("classes") loop
         declare
            use Komnenos.Styles;
            Class       : constant String  := Child.Config_Name;
            Font_Name   : constant String :=
                            Child.Get
                              ("font_family",
                               Fixed_Font.Get
                                 ("font_family",
                                  "courier"));
            Font_Size   : constant String :=
                            Child.Get
                              ("font_size",
                               Fixed_Font.Get
                                 ("font_size",
                                  "10"));
            State_Text  : constant String :=
                            Child.Get ("state", "normal");
            Bold        : constant Boolean :=
                            Child.Get ("bold");
            Italic      : constant Boolean :=
                            Child.Get ("italic");
            Underlined  : constant Boolean :=
                            Child.Get ("underline");
            Foreground  : constant String :=
                            Child.Get ("foreground", "");
            Background  : constant String  :=
                            Child.Get ("background", "");
            Cursor_Text : constant String :=
                            Child.Get ("mouse_cursor", "default");
            Font        : constant Komnenos.Fonts.Komnenos_Font :=
                            Komnenos.Fonts.Create_Font
                              (Name       => Font_Name,
                               Size       => Natural'Value (Font_Size),
                               Bold       => Bold,
                               Italic     => Italic,
                               Underlined => Underlined);
         begin
            if Foreground /= "" then
               Font.Set_Foreground_Color
                 (Komnenos.Colours.From_String (Foreground));
            end if;

            if Background /= "" then
               Font.Set_Background_Color
                 (Komnenos.Colours.From_String (Background));
            end if;

            declare
               Style       : constant Komnenos.Styles.Komnenos_Style :=
                               Komnenos.Styles.Create_Style
                                 (Name         => Class & "-" & State_Text,
                                  Font         => Font,
                                  Mouse_Cursor =>
                                    Mouse_Cursor_Type'Value (Cursor_Text));
               State       : constant Element_State :=
                               Element_State'Value (State_Text);
            begin
               if Class = "default" then
                  Result.Default_Style := Style;
                  Result.Default_Link_Style := Style;
               end if;
               if Class = "link" then
                  Result.Default_Link_Style := Style;
               end if;
               Result.Entries.Append
                 ((Class => Ada.Strings.Unbounded.To_Unbounded_String (Class),
                   State => State,
                   Style => Style));
            end;
         end;
      end loop;

      return Theme : constant Komnenos_Theme :=
        new Komnenos_Root_Theme'(Result)
      do
         Current_Active_Theme := Theme;
      end return;
   end Load_Theme;

   ----------------
   -- Load_Theme --
   ----------------

   procedure Load_Theme
     (Theme_Config : Tropos.Configuration)
   is
      Theme : constant Komnenos_Theme :=
                Load_Theme (Theme_Config);
   begin
      pragma Unreferenced (Theme);
   end Load_Theme;

   -----------
   -- Style --
   -----------

   function Style
     (Theme : Komnenos_Root_Theme'Class;
      Class : in     String;
      State :        Element_State := Normal)
      return Komnenos.Styles.Komnenos_Style
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      for T of Theme.Entries loop
         if T.Class = Class and then T.State = State then
            return T.Style;
         end if;
      end loop;
      return Theme.Default_Style;
   end Style;

end Komnenos.Themes;
