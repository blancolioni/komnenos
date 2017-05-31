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
      From        : Node_Reference;
      From_Edge   : Node_Edge;
      To          : Node_Reference;
      To_Edge     : Node_Edge)
   is
      use Ada.Strings.Unbounded;
   begin
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
               use Komnenos.Displays;
               Path : constant Komnenos.Displays.Turtle_Path :=
                        (Turn (North, 6),
                         Move (15),
                         Turn (East, 6),
                         Move (Pixel_Offset'Max
                           (Finish.X - Start.X - 42, 0)),
                         Turn (South, 6),
                         Move (15),
                         Turn (East, 6),
                         Move (18));
            begin
               Display.Draw_Turtle_Path
                 (Start_Location  => Start,
                  Start_Direction => East,
                  Path            => Path,
                  Colour          => Komnenos.Colours.Black,
                  Arrow           => To.Style /= Internal);
            end;
         end if;
      else
         declare
            use Komnenos.Displays;
            Path : constant Komnenos.Displays.Turtle_Path :=
                     (Turn (South, 6),
                      Move (15),
                      Turn (West, 6),
                      Move (Start.X - Finish.X),
                      Turn (North, 6),
                      Move (15),
                      Turn (East, 6),
                      Move (18));
         begin
            Display.Draw_Turtle_Path
              (Start_Location  => Start,
               Start_Direction => East,
               Path            => Path,
               Colour          => Komnenos.Colours.Black,
               Arrow           => To.Style /= Internal);
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
      if Fragment.Canvas /= null then
         for Node of Fragment.Nodes loop
            declare
               Text         : constant String := -Node.Label_Text;
               Default_Size : constant Pixel_Length := 8;
               Size_Rec     : constant Layout_Rectangle :=
                                (if -Node.Label_Text /= ""
                                 then Fragment.Canvas.Get_Bounding_Rectangle
                                   (Node.Label_Style.Font, Text)
                                 elsif Node.Style = Internal
                                 then  (0, 0, 0, 0)
                                 else (0, 0, Default_Size, Default_Size));
            begin
               Node.Rectangle := Size_Rec;
--                   (Pixel_Position (Node.X - 1) * Width + Width / 2
--                    - Size_Rec.Width / 2,
--                    Pixel_Position (Node.Y - 1) * Height + Height / 2
--                    - Size_Rec.Height / 2,
--                    Size_Rec.Width, Size_Rec.Height);
--                 if Node.Style /= Internal then
--                    Render (Node, Fragment.Canvas);
--                 end if;
            end;
         end loop;

         declare
            Height       : constant Pixel_Length :=
                             Fragment.Height
                               / Pixel_Length (Fragment.Rows);
            Col_Width    : array (1 .. Fragment.Columns) of Pixel_Length :=
                          (others => 0);
            Col_Left  : array (1 .. Fragment.Columns) of Pixel_Position :=
                             (others => 0);
            Col_Visible  : array (1 .. Fragment.Columns) of Boolean :=
                             (others => False);

         begin
            for Node of Fragment.Nodes loop
               Col_Width (Node.X) :=
                 Pixel_Length'Max
                   (Col_Width (Node.X),
                    Node.Rectangle.Width);
               if Node.Style /= Internal then
                  Col_Visible (Node.X) := True;
               end if;
            end loop;

            Col_Left (1) := 20;
            for I in 2 .. Col_Width'Last loop
               Col_Left (I) :=
                 Col_Left (I - 1) + Col_Width (I - 1)
                 + (if Col_Visible (I - 1)
                    and then Col_Visible (I)
                    then 50 else 25);
            end loop;

            for Node of Fragment.Nodes loop
               Node.Rectangle.X := Col_Left (Node.X);
               Node.Rectangle.Y :=
                 Pixel_Position (Node.Y - 1) * Height + Height / 2
                                   - Node.Rectangle.Height / 2;
            end loop;
         end;

         for Node of Fragment.Nodes loop
            if Node.Style /= Internal then
               Render (Node, Fragment.Canvas);
            end if;
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
     (Diagram : in out Diagram_Fragment_Type;
      Node    : Node_Reference;
      X, Y    : Positive)
   is
   begin
      Diagram.Nodes (Node).X := X;
      Diagram.Nodes (Node).Y := Y;
      Diagram.Columns := Natural'Max (Diagram.Columns, X);
      Diagram.Rows := Natural'Max (Diagram.Rows, Y);
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

   overriding function Put_Node
     (Diagram     : in out Diagram_Fragment_Type;
      X, Y        : Positive;
      Style       : Node_Style;
      Label_Text  : String;
      Label_Style : Komnenos.Styles.Komnenos_Style;
      Tool_Tip    : String;
      Link        : access Komnenos.Entities.Root_Entity_Reference'Class)
      return Node_Reference
   is
      Node : constant Diagram_Node := Diagram_Node'
        (X           => X,
         Y           => Y,
         Rectangle   => (0, 0, 1, 1),
         Style       => Style,
         Label_Text  => +Label_Text,
         Label_Style => Label_Style,
         Tool_Tip    => +Tool_Tip,
         Link        => Komnenos.Entities.Entity_Reference (Link),
         Connections => Node_Connection_Lists.Empty_List);
   begin
      Diagram.Nodes.Append (Node);
      Diagram.Columns := Natural'Max (Diagram.Columns, X);
      Diagram.Rows := Natural'Max (Diagram.Rows, Y);
      return Diagram.Nodes.Last_Index;
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
