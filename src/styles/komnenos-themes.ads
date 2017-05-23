private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded;

with Tropos;

with Komnenos.Named_Objects;
with Komnenos.Styles;

package Komnenos.Themes is

   type Komnenos_Root_Theme is
     new Komnenos.Named_Objects.Root_Named_Object with private;

   function Style
     (Theme : Komnenos_Root_Theme'Class;
      Class : in     String;
      State :        Element_State := Normal)
      return Komnenos.Styles.Komnenos_Style;

   function Default_Style
     (Theme : Komnenos_Root_Theme'Class)
      return Komnenos.Styles.Komnenos_Style;

   function Default_Link_Style
     (Theme : Komnenos_Root_Theme'Class)
      return Komnenos.Styles.Komnenos_Style;

   function Default_Font_Name
     (Theme : Komnenos_Root_Theme'Class)
      return String;

   function Default_Font_Size
     (Theme : Komnenos_Root_Theme'Class)
      return Natural;

   type Komnenos_Theme is access constant Komnenos_Root_Theme'Class;

   function Load_Theme
     (Theme_Config : Tropos.Configuration)
      return Komnenos_Theme;

   procedure Load_Theme
     (Theme_Config : Tropos.Configuration);

   function Active_Theme
      return Komnenos_Theme;

private

   type Theme_Entry is
      record
         Class : Ada.Strings.Unbounded.Unbounded_String;
         State : Element_State;
         Style : Komnenos.Styles.Komnenos_Style;
      end record;

   package Theme_Entry_Vectors is
     new Ada.Containers.Vectors (Positive, Theme_Entry);

   type Komnenos_Root_Theme is
     new Komnenos.Named_Objects.Root_Named_Object with
      record
         Entries            : Theme_Entry_Vectors.Vector;
         Default_Style      : Komnenos.Styles.Komnenos_Style;
         Default_Link_Style : Komnenos.Styles.Komnenos_Style;
         Default_Font_Name  : Ada.Strings.Unbounded.Unbounded_String;
         Default_Font_Size  : Natural;
      end record;

   Current_Active_Theme : Komnenos_Theme;

   function Active_Theme
     return Komnenos_Theme
   is (Current_Active_Theme);

end Komnenos.Themes;
