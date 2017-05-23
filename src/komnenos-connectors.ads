private with Tropos;

with Komnenos.Entities;
with Komnenos.Session_Objects;

package Komnenos.Connectors is

   type Connector_Class is (Arrow, Dashed_Arrow);

   type Root_Connector_Type is
     new Komnenos.Session_Objects.Session_Object_Interface with private;

   function Class
     (Connector : Root_Connector_Type'Class)
      return Connector_Class;

   function Source
     (Connector : Root_Connector_Type'Class)
      return Komnenos.Entities.Entity_Visual_Access;

   function Source_Offset
     (Connector : Root_Connector_Type'Class)
      return Pixel_Offset;

   function Destination
     (Connector : Root_Connector_Type'Class)
      return Komnenos.Entities.Entity_Visual_Access;

   function Destination_Offset
     (Connector : Root_Connector_Type'Class)
      return Pixel_Offset;

   function Layout_Boundary
     (Connector : Root_Connector_Type'Class)
      return Layout_Rectangle;

   function Layout_Path
     (Connector : Root_Connector_Type'Class)
      return Layout_Line;

   procedure Update (Connector : in out Root_Connector_Type'Class);

   function Connects
     (Connector : Root_Connector_Type'Class;
      Visual    : not null access Komnenos.Entities.Entity_Visual'Class)
      return Boolean;

   type Connector_Type is access all Root_Connector_Type'Class;

   function Connect
     (Class              : Connector_Class;
      Source             : not null access
        Komnenos.Entities.Entity_Visual'Class;
      Source_Offset      : Pixel_Offset;
      Destination        : not null access
        Komnenos.Entities.Entity_Visual'Class;
      Destination_Offset : Pixel_Offset)
      return Connector_Type;

   type Connector_Display_Interface is interface;

   procedure Remove (Display : in out Connector_Display_Interface)
   is abstract;

   procedure Update (Display : in out Connector_Display_Interface)
   is abstract;

   type Connector_Display is access all Connector_Display_Interface'Class;

   procedure Set_Display
     (Connector : in out Root_Connector_Type'Class;
      Display   : not null access Connector_Display_Interface'Class);

   function Display
     (Connector : Root_Connector_Type'Class)
      return Connector_Display;

   procedure Register;

private

   type Root_Connector_Type is
     new Komnenos.Session_Objects.Session_Object_Interface with
      record
         Class              : Connector_Class;
         Source             : Komnenos.Entities.Entity_Visual_Access;
         Source_Offset      : Pixel_Offset;
         Destination        : Komnenos.Entities.Entity_Visual_Access;
         Destination_Offset : Pixel_Offset;
         Boundary           : Layout_Rectangle;
         Path               : Layout_Line (1 .. 4);
         Display            : Connector_Display;
      end record;

   overriding function Config_Name
     (Item : Root_Connector_Type)
      return String;

   overriding procedure To_Config
     (Item : Root_Connector_Type;
      Config : in out Tropos.Configuration);

   overriding procedure From_Config
     (Item : not null access Root_Connector_Type;
      Config : Tropos.Configuration);

   function Class
     (Connector : Root_Connector_Type'Class)
      return Connector_Class
   is (Connector.Class);

   function Source
     (Connector : Root_Connector_Type'Class)
      return Komnenos.Entities.Entity_Visual_Access
   is (Connector.Source);

   function Source_Offset
     (Connector : Root_Connector_Type'Class)
      return Pixel_Offset
   is (Connector.Source_Offset);

   function Destination
     (Connector : Root_Connector_Type'Class)
      return Komnenos.Entities.Entity_Visual_Access
   is (Connector.Destination);

   function Destination_Offset
     (Connector : Root_Connector_Type'Class)
      return Pixel_Offset
   is (Connector.Destination_Offset);

   overriding function Config_Name
     (Item : Root_Connector_Type)
      return String
   is ("connector");

   function Display
     (Connector : Root_Connector_Type'Class)
      return Connector_Display
   is (Connector.Display);

   function Layout_Boundary
     (Connector : Root_Connector_Type'Class)
      return Layout_Rectangle
   is (Connector.Boundary);

   function Layout_Path
     (Connector : Root_Connector_Type'Class)
      return Layout_Line
   is (Connector.Path);

end Komnenos.Connectors;
