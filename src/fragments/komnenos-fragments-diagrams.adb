with Ada.Text_IO;

with Komnenos.Displays;

package body Komnenos.Fragments.Diagrams is

   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   procedure Render
     (Node      : Diagram_Node;
      Rectangle : Layout_Rectangle;
      Display   : not null access Komnenos.Displays.Canvas_Display'Class);

   overriding procedure Clear (Fragment : in out Diagram_Fragment_Type)
   is null;

   -------------------
   -- Connect_Nodes --
   -------------------

   overriding procedure Connect_Nodes
     (Diagram     : in out Diagram_Fragment_Type;
      From, To    : String)
   is null;

   ----------------
   -- Invalidate --
   ----------------

   overriding procedure Invalidate
     (Fragment : not null access Diagram_Fragment_Type)
   is
   begin
      Ada.Text_IO.Put_Line
        ("invalidate: " & Ada.Strings.Unbounded.To_String (Fragment.Title));
      if Fragment.Canvas /= null then
         for Node of Fragment.Nodes loop
            declare
               Width     : constant Pixel_Length :=
                             Fragment.Width / Pixel_Length (Fragment.Columns);
               Height    : constant Pixel_Length :=
                             Fragment.Height / Pixel_Length (Fragment.Rows);
               Rectangle : constant Layout_Rectangle :=
                             (Pixel_Position (Node.X - 1) * Width,
                              Pixel_Position (Node.Y - 1) * Height,
                              Width, Height);
            begin
               Render (Node, Rectangle, Fragment.Canvas);
            end;
         end loop;
      end if;
   end Invalidate;

   ---------------
   -- Move_Node --
   ---------------

   overriding procedure Move_Node
     (Diagram     : in out Diagram_Fragment_Type;
      Key         : String;
      X, Y        : Positive)
   is
      use Ada.Strings.Unbounded;
   begin
      for Node of Diagram.Nodes loop
         if Node.Key = Key then
            Node.X := X;
            Node.Y := Y;
            return;
         end if;
      end loop;
      raise Constraint_Error with
        "no such node: " & Key;
   end Move_Node;

   -----------------
   -- New_Diagram --
   -----------------

   function New_Diagram
     return Diagram_Fragment
   is
   begin
      return new Diagram_Fragment_Type;
   end New_Diagram;

   -----------------
   -- New_Diagram --
   -----------------

   function New_Diagram
     (Entity : not null access
        Komnenos.Entities.Root_Entity_Reference'Class)
      return Diagram_Fragment
   is
   begin
      return Diagram : constant Diagram_Fragment := New_Diagram do
         Diagram.Set_Content (Entity);
      end return;
   end New_Diagram;

   --------------
   -- Put_Node --
   --------------

   overriding procedure Put_Node
     (Diagram     : in out Diagram_Fragment_Type;
      Key         : String;
      X, Y        : Positive;
      Style       : Komnenos.Entities.Visuals.Node_Style;
      Label_Text  : String;
      Label_Style : Komnenos.Styles.Komnenos_Style;
      Tool_Tip    : String;
      Link        : access Komnenos.Entities.Root_Entity_Reference'Class)
   is

      Node : constant Diagram_Node := Diagram_Node'
        (Key         => +Key,
         X           => X,
         Y           => Y,
         Style       => Style,
         Label_Text  => +Label_Text,
         Label_Style => Label_Style,
         Tool_Tip    => +Tool_Tip,
         Link        => Komnenos.Entities.Entity_Reference (Link));
   begin
      Ada.Text_IO.Put_Line ("diagram: put-node: " & Label_Text);
      Diagram.Nodes.Append (Node);
      Diagram.Columns := Natural'Max (Diagram.Columns, X);
      Diagram.Rows := Natural'Max (Diagram.Rows, Y);
   end Put_Node;

   ------------
   -- Render --
   ------------

   procedure Render
     (Node      : Diagram_Node;
      Rectangle : Layout_Rectangle;
      Display   : not null access Komnenos.Displays.Canvas_Display'Class)
   is
      pragma Unreferenced (Node);
   begin
      Ada.Text_IO.Put_Line
        ("draw rectangle:"
         & Rectangle.X'Img & Rectangle.Y'Img
         & Rectangle.Width'Img & Rectangle.Height'Img);

      Display.Draw_Rectangle
        (Rectangle         => Rectangle,
         Border_Colour     => Komnenos.Colours.Black,
         Background_Colour => Komnenos.Colours.White,
         Filled            => False,
         Corner_Radius     => 15);
   end Render;

end Komnenos.Fragments.Diagrams;
