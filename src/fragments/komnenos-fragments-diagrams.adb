with Ada.Text_IO;

with Komnenos.Displays;

package body Komnenos.Fragments.Diagrams is

   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (S : Ada.Strings.Unbounded.Unbounded_String) return String
                 renames Ada.Strings.Unbounded.To_String;

   procedure Render
     (Node      : in out Diagram_Node;
      Display   : not null access Komnenos.Displays.Canvas_Display'Class);

   procedure Draw_Connection
     (From, To : Diagram_Node;
      Straight : Boolean;
      Display  : not null access Komnenos.Displays.Canvas_Display'Class);

   overriding procedure Clear (Fragment : in out Diagram_Fragment_Type)
   is null;

   -------------------
   -- Connect_Nodes --
   -------------------

   overriding procedure Connect_Nodes
     (Diagram     : in out Diagram_Fragment_Type;
      From_Key    : String;
      From_Edge   : Komnenos.Entities.Visuals.Node_Edge;
      To_Key      : String;
      To_Edge     : Komnenos.Entities.Visuals.Node_Edge)
   is
      use Ada.Strings.Unbounded;
      From, To : Natural := 0;
   begin
      for I in 1 .. Diagram.Nodes.Last_Index loop
         declare
            Node : Diagram_Node renames Diagram.Nodes (I);
         begin
            if Node.Key = From_Key then
               From := I;
            end if;
            if Node.Key = To_Key then
               To := I;
            end if;
            exit when From /= 0 and then To /= 0;
         end;
      end loop;

      if From = 0 then
         raise Constraint_Error with
           "cannot find 'from' node in connection: " & From_Key;
      end if;

      if To = 0 then
         raise Constraint_Error with
           "cannot find 'to' node in connection: " & From_Key;
      end if;

      Diagram.Nodes (From).Connections.Append
        ((From, To, From_Edge, To_Edge));

   end Connect_Nodes;

   ---------------------
   -- Draw_Connection --
   ---------------------

   procedure Draw_Connection
     (From, To : Diagram_Node;
      Straight : Boolean;
      Display  : not null access Komnenos.Displays.Canvas_Display'Class)
   is
      use Komnenos.Entities.Visuals;
      Start : constant Layout_Point :=
                (From.Rectangle.X + From.Rectangle.Width - 1,
                 From.Rectangle.Y + From.Rectangle.Height / 2);
      Finish : constant Layout_Point :=
                 (To.Rectangle.X,
                  To.Rectangle.Y + To.Rectangle.Height / 2);

   begin

      if Finish.X > Start.X then
         if Straight then
            Display.Draw_Line
              (Line   => (Start, Finish),
               Colour => Komnenos.Colours.Black,
               Curved => False,
               Arrow  => To.Style /= Internal);
         else
            declare
               Line : constant Layout_Line :=
                        (Start,
                         (Start.X + 6, Start.Y),
                         (Start.X + 6, Start.Y - 6),
                         (Start.X + 6, Start.Y - 32),
                         (Start.X + 12, Start.Y - 40),
                         (Finish.X - 12, Start.Y - 40),
                         (Finish.X - 6, Start.Y - 32),
                         (Finish.X - 6, Finish.Y - 6),
                         Finish);
            begin
               Display.Draw_Line
                 (Line   => Line,
                  Colour => Komnenos.Colours.Black,
                  Curved => True,
                  Arrow  => To.Style /= Internal);
            end;
         end if;
      else
         declare
            Line : constant Layout_Line :=
                     (Start,
                      (Start.X + 6, Start.Y),
                      (Start.X + 6, Start.Y + 6),
                      (Start.X + 6, Start.Y + 32),
                      (Start.X + 12, Start.Y + 40),
                      (Finish.X - 12, Start.Y + 40),
                      (Finish.X - 6, Start.Y + 32),
                      (Finish.X - 6, Finish.Y + 6),
                      Finish);
         begin
            Display.Draw_Line
              (Line   => Line,
               Colour => Komnenos.Colours.Black,
               Curved => True,
               Arrow  => To.Style /= Internal);
         end;

      end if;
   end Draw_Connection;

   ----------------
   -- Invalidate --
   ----------------

   overriding procedure Invalidate
     (Fragment : not null access Diagram_Fragment_Type)
   is
      use Komnenos.Entities.Visuals;

      function Straight_Line (From, To : Diagram_Node) return Boolean;

      -------------------
      -- Straight_Line --
      -------------------

      function Straight_Line (From, To : Diagram_Node) return Boolean is
      begin
         if From.X >= To.X
           or else From.Y /= To.Y
         then
            return False;
         end if;

         if From.X + 1 = To.X then
            return True;
         end if;

         for Node of Fragment.Nodes loop
            if Node.Style /= Internal
              and then Node.Y = From.Y
              and then Node.X in From.X + 1 .. To.X - 1
            then
               return False;
            end if;
         end loop;

         return True;
      end Straight_Line;

   begin
      Ada.Text_IO.Put_Line
        ("invalidate: " & Ada.Strings.Unbounded.To_String (Fragment.Title));
      if Fragment.Canvas /= null then
         for Node of Fragment.Nodes loop
            declare
               Default_Size : constant Pixel_Length := 8;
               Width        : constant Pixel_Length :=
                                Fragment.Width
                                  / Pixel_Length (Fragment.Columns);
               Height       : constant Pixel_Length :=
                                Fragment.Height
                                  / Pixel_Length (Fragment.Rows);
            begin
               Node.Rectangle :=
                 (Pixel_Position (Node.X - 1) * Width + Width / 2
                  - Default_Size / 2,
                  Pixel_Position (Node.Y - 1) * Height + Height / 2
                  - Default_Size / 2,
                  Default_Size, Default_Size);
               if Node.Style /= Internal then
                  Render (Node, Fragment.Canvas);
               end if;
            end;
         end loop;

         for Node of Fragment.Nodes loop
            for Conn of Node.Connections loop
               Draw_Connection
                 (From     => Node,
                  To       => Fragment.Nodes (Conn.To),
                  Straight => Straight_Line (Node, Fragment.Nodes (Conn.To)),
                  Display  => Fragment.Canvas);
            end loop;
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
         Diagram.Layout_Rec.Width := 500;
         Diagram.Layout_Rec.Height := 200;
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
         Rectangle   => (0, 0, 1, 1),
         Style       => Style,
         Label_Text  => +Label_Text,
         Label_Style => Label_Style,
         Tool_Tip    => +Tool_Tip,
         Link        => Komnenos.Entities.Entity_Reference (Link),
         Connections => Node_Connection_Lists.Empty_List);
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
     (Node      : in out Diagram_Node;
      Display   : not null access Komnenos.Displays.Canvas_Display'Class)
   is
      use Komnenos.Entities.Visuals;
      Label : constant String := -Node.Label_Text;
   begin

      if Label /= "" then
         Display.Draw_Text
           (Rectangle => Node.Rectangle,
            Font      => Node.Label_Style.Font,
            Text      => -Node.Label_Text);
      end if;

      declare
         Corner_Radius : constant Pixel_Length :=
                           (case Node.Style is
                               when Box         => 0,
                               when Rounded_Box =>
                                  Node.Rectangle.Height / 3,
                               when Circle =>
                                  Node.Rectangle.Height / 2,
                               when Internal     => 0);
      begin
         Display.Draw_Rectangle
           (Rectangle         => Node.Rectangle,
            Border_Colour     => Komnenos.Colours.Black,
            Background_Colour => Komnenos.Colours.White,
            Filled            => False,
            Corner_Radius     => Corner_Radius);
      end;

   end Render;

end Komnenos.Fragments.Diagrams;
