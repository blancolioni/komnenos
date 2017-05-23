with Komnenos.UI;

package body Komnenos.Connectors is

   Boundary_Size : constant := 16;

   function New_Connector
     return access Komnenos.Session_Objects.Session_Object_Interface'Class;

   -------------
   -- Connect --
   -------------

   function Connect
     (Class              : Connector_Class;
      Source             : not null access
        Komnenos.Entities.Entity_Visual'Class;
      Source_Offset      : Pixel_Offset;
      Destination        : not null access
        Komnenos.Entities.Entity_Visual'Class;
      Destination_Offset : Pixel_Offset)
      return Connector_Type
   is
      use Komnenos.Entities;
   begin
      return Connector : constant Connector_Type := new Root_Connector_Type'
        (Class              => Class,
         Source             => Entity_Visual_Access (Source),
         Source_Offset      => Source_Offset,
         Destination        => Entity_Visual_Access (Destination),
         Destination_Offset => Destination_Offset,
         Boundary           => (0, 0, 1, 1),
         Path               => (others => (0, 0)),
         Display            => null)
      do
         Connector.Update;
      end return;
   end Connect;

   --------------
   -- Connects --
   --------------

   function Connects
     (Connector : Root_Connector_Type'Class;
      Visual    : not null access Komnenos.Entities.Entity_Visual'Class)
      return Boolean
   is
      use Komnenos.Entities;
      V : constant Entity_Visual_Access :=
            Entity_Visual_Access (Visual);
   begin
      return V = Connector.Source
        or else V = Connector.Destination;
   end Connects;

   -----------------
   -- From_Config --
   -----------------

   overriding procedure From_Config
     (Item : not null access Root_Connector_Type;
      Config : Tropos.Configuration)
   is
   begin
      Item.Class :=
        Connector_Class'Value (Config.Get ("class", "arrow"));
      Item.Source :=
        Komnenos.UI.Current_UI.Get_Visual
          (Config.Get ("source"));
      Item.Destination :=
        Komnenos.UI.Current_UI.Get_Visual
          (Config.Get ("destination"));
      Item.Source_Offset :=
        Pixel_Offset (Integer'(Config.Get ("source-offset")));
      Item.Destination_Offset :=
        Pixel_Offset (Integer'(Config.Get ("destination-offset")));
   end From_Config;

   -------------------
   -- New_Connector --
   -------------------

   function New_Connector
     return access Komnenos.Session_Objects.Session_Object_Interface'Class
   is
      Result : constant Connector_Type :=
                 new Root_Connector_Type;
   begin
      return Result;
   end New_Connector;

   --------------
   -- Register --
   --------------

   procedure Register is
   begin
      Komnenos.Session_Objects.Register_Session_Object
        ("connector", New_Connector'Access);
   end Register;

   -----------------
   -- Set_Display --
   -----------------

   procedure Set_Display
     (Connector : in out Root_Connector_Type'Class;
      Display   : not null access Connector_Display_Interface'Class)
   is
   begin
      Connector.Display := Connector_Display (Display);
   end Set_Display;

   ---------------
   -- To_Config --
   ---------------

   overriding procedure To_Config
     (Item : Root_Connector_Type;
      Config : in out Tropos.Configuration)
   is
   begin
      Config.Add ("class", Connector_Class'Image (Item.Class));
      Config.Add ("source", Item.Source.Get_Content.Key);
      Config.Add ("destination", Item.Destination.Get_Content.Key);
      Config.Add ("source-offset", Integer (Item.Source_Offset));
      Config.Add ("destination-offset", Integer (Item.Destination_Offset));
   end To_Config;

   ------------
   -- Update --
   ------------

   procedure Update (Connector : in out Root_Connector_Type'Class) is
      Source : Komnenos.Entities.Entity_Visual_Access renames
                 Connector.Source;
      Dest : Komnenos.Entities.Entity_Visual_Access renames
                 Connector.Destination;
      X1 : Pixel_Position renames Connector.Path (1).X;
      Y1 : Pixel_Position renames Connector.Path (1).Y;
      X2 : Pixel_Position renames Connector.Path (2).X;
      Y2 : Pixel_Position renames Connector.Path (2).Y;
      X3 : Pixel_Position renames Connector.Path (3).X;
      Y3 : Pixel_Position renames Connector.Path (3).Y;
      X4 : Pixel_Position renames Connector.Path (4).X;
      Y4 : Pixel_Position renames Connector.Path (4).Y;
   begin
      Y1 := Source.Y + Connector.Source_Offset + Boundary_Size;
      Y4 := Dest.Y + Connector.Destination_Offset + Boundary_Size;
      if Dest.X > Source.X + Source.Width then
         X1 := Source.X + Source.Width + Boundary_Size;
         X4 := Dest.X + Boundary_Size;
         X2 := (X4 + X1) / 2;
         Y2 := Y1;
         X3 := X2;
         Y3 := Y4;
      else
         X1 := Source.X + Boundary_Size;
         if Dest.X > Source.X then
            X4 := Dest.X + Boundary_Size;
            X2 := X1 - Boundary_Size * 2;
            Y2 := Y1;
            X3 := X2;
            Y3 := Y4;
         else
            X4 := Dest.X + Dest.Width + Boundary_Size;
            X2 := (X4 + X1) / 2;
            Y2 := Y1;
            X3 := X2;
            Y3 := Y4;
         end if;
      end if;

      declare
         Min_X, Min_Y : Pixel_Position := Pixel_Position'Last;
         Max_X, Max_Y : Pixel_Position := Pixel_Position'First;
      begin
         for I in Connector.Path'Range loop
            declare
               X : Pixel_Position renames Connector.Path (I).X;
               Y : Pixel_Position renames Connector.Path (I).Y;
            begin
               Min_X := Pixel_Position'Min (Min_X, X);
               Max_X := Pixel_Position'Max (Max_X, X);
               Min_Y := Pixel_Position'Min (Min_Y, Y);
               Max_Y := Pixel_Position'Max (Max_Y, Y);
            end;
         end loop;

         Connector.Boundary := (Min_X, Min_Y, Max_X - Min_X, Max_Y - Min_Y);
      end;

      if Connector.Display /= null then
         Connector.Display.Update;
      end if;

   end Update;

end Komnenos.Connectors;
