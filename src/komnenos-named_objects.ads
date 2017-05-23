private with Ada.Strings.Unbounded;

package Komnenos.Named_Objects is

   type Root_Named_Object is abstract tagged private;

   function Name (Item : Root_Named_Object'Class) return String;
   procedure Set_Name (Item : in out Root_Named_Object'Class;
                       Name : String);

private

   type Root_Named_Object is abstract tagged
      record
         Object_Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   function Name (Item : Root_Named_Object'Class) return String
   is (Ada.Strings.Unbounded.To_String (Item.Object_Name));

end Komnenos.Named_Objects;
