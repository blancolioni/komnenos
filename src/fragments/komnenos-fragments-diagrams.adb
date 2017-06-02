with Ada.Text_IO;

with Komnenos.Configuration;
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

   procedure Create_Layout
     (Nodes : in out Node_Vectors.Vector);

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
        ((From, To, From_Edge, To_Edge, False));

      if Komnenos.Configuration.Get_Diagram_Config.Debug_Layout then
         Ada.Text_IO.Put_Line ("connect:" & From'Img & " -->" & To'Img);
      end if;

   end Connect_Nodes;

   -------------------
   -- Create_Layout --
   -------------------

   procedure Create_Layout
     (Nodes : in out Node_Vectors.Vector)
   is

      subtype Node_Reference_Range is
        Node_Reference range 1 .. Nodes.Last_Index;

      type Node_Flag_Array is array (Node_Reference_Range) of Boolean;

      type Node_Record is
         record
            Placed         : Boolean := False;
            Connected_From : Node_Flag_Array  := (others => False);
            Connects_To    : Node_Flag_Array  := (others => False);
            Rectangle      : Layout_Rectangle := (0, 0, 0, 0);
         end record;

      Node_Info : array (Node_Reference_Range) of Node_Record
        with Unreferenced;

      type Node_Reference_Array is
        array (Natural range 0 .. Natural (Nodes.Length)) of Node_Reference;

      type Layout_Row is
         record
            Count : Natural := 0;
            Nodes : Node_Reference_Array;
         end record;

      type Layout_Array is
        array (Natural range 0 .. Natural (Nodes.Length)) of Layout_Row;

      Layout : Layout_Array;
      Row_Count : Natural := 0;

      Config : constant Komnenos.Configuration.Diagram_Config :=
                 Komnenos.Configuration.Get_Diagram_Config;

      package Reference_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Node_Reference);

      Placed_Cell : array (Node_Reference_Range) of Natural := (others => 0);
      Placed_Row  : array (Node_Reference_Range) of Natural := (others => 0);
      Previous    : array (Node_Reference_Range) of Node_Reference_Range;
      Placed      : array (Node_Reference_Range) of Boolean :=
                      (others => False);

      --  function Connects_To (X, Y : Node_Reference) return Boolean;

      function Inter_Node_Gap
        (From, To : Node_Reference_Range)
         return Pixel_Length
      is (if Nodes (From).Style = Internal
          or else Nodes (To).Style = Internal
          then Config.Internal_Node_Gap
          else Config.Visible_Node_Gap);

   begin

      pragma Unreferenced (Placed_Cell);
      pragma Unreferenced (Placed_Row);

      Nodes (1).Rectangle.X := Config.Internal_Node_Gap;
      Nodes (1).Rectangle.Y := Config.Internal_Node_Gap;

      for I in 1 .. Nodes.Last_Index loop
         declare
            From : constant Diagram_Node := Nodes (I);
         begin
            Node_Info (I).Rectangle := Nodes (I).Rectangle;
            for Connection of From.Connections loop
               declare
                  To_Ref : constant Node_Reference := Connection.To;
               begin
                  Node_Info (I).Connects_To (To_Ref) := True;
                  Node_Info (To_Ref).Connected_From (I) := True;
               end;
            end loop;
         end;
      end loop;

      declare
         Current_Row  : Natural := 0;
         Queue        : Reference_Lists.List;
      begin

         Queue.Append (1);

         while not Queue.Is_Empty loop

            Current_Row := Current_Row + 1;

            declare
               Current_Node : Node_Reference := Queue.First_Element;
               Next_Node    : Node_Reference := Current_Node;
               Have_Node    : Boolean := True;
            begin

               if Config.Debug_Layout then
                  Ada.Text_IO.Put_Line ("layout:" & Current_Node'Img);
               end if;

               Layout (Current_Row).Count := 1;
               Layout (Current_Row).Nodes (1) := Current_Node;

               Placed_Cell (Current_Node) := 1;
               Placed_Row (Current_Node) := Current_Row;
               Placed (Current_Node) := True;

               Nodes (Current_Node).Row := Current_Row;

               Queue.Delete_First;

               while Have_Node loop

                  Current_Node := Next_Node;
                  Have_Node := False;
                  for Connection of Nodes (Current_Node).Connections loop
                     if not Placed (Connection.To) then
                        if Have_Node then
                           Previous (Connection.To) := Current_Node;

                           if Config.Debug_Layout then
                              Ada.Text_IO.Put_Line
                                ("queue:" & Connection.To'Img
                                 & " after" & Current_Node'Img);
                           end if;

                           Queue.Append (Connection.To);
                        else
                           if Config.Debug_Layout then
                              Ada.Text_IO.Put_Line
                                ("next in row:" & Connection.To'Img);
                           end if;

                           declare
                              Existing : Reference_Lists.Cursor :=
                                           Queue.Find (Current_Node);
                           begin
                              if Reference_Lists.Has_Element (Existing) then
                                 if Config.Debug_Layout then
                                    Ada.Text_IO.Put_Line
                                      ("canceled queued node:"
                                       & Connection.To'Img);
                                 end if;
                                 Queue.Delete (Existing);
                              end if;
                           end;

                           declare
                              Row : Layout_Row renames Layout (Current_Row);
                           begin
                              Row.Count := Row.Count + 1;
                              Row.Nodes (Row.Count) := Connection.To;
                              Placed_Cell (Connection.To) := Row.Count;
                              Placed_Row (Connection.To) := Current_Row;
                              Placed (Connection.To) := True;
                              Connection.Straight := True;
                              Next_Node := Connection.To;
                              Have_Node := True;
                           end;
                        end if;
                     end if;
                  end loop;

               end loop;

               if Config.Debug_Layout then
                  Ada.Text_IO.Put_Line
                    ("finished: row" & Current_Row'Img);
               end if;

            end;
         end loop;

         Row_Count := Current_Row;
      end;

      declare
         Y : Pixel_Length := Config.Layout_Row_Size;
      begin
         for Row_Index in 1 .. Row_Count loop
            declare
               Row : Layout_Row renames Layout (Row_Index);
               X   : Pixel_Length := Config.Internal_Node_Gap;
            begin
               for Col_Index in 1 .. Row.Count loop
                  declare
                     Ref : constant Node_Reference := Row.Nodes (Col_Index);
                     Node : Diagram_Node renames Nodes (Ref);
                  begin
                     if Col_Index = 1
                       and then Row_Index > 1
                     then
                        declare
                           Prev_Ref : constant Node_Reference :=
                                        Previous (Row.Nodes (1));
                           Prev_Rec : constant Layout_Rectangle :=
                                        Nodes.Element (Prev_Ref).Rectangle;
                        begin

                           if Config.Debug_Layout then
                              Ada.Text_IO.Put_Line
                                ("Node" & Ref'Img
                                 & " placed after"
                                 & Prev_Ref'Img
                                 & " at" & Prev_Rec.X'Img
                                 & Prev_Rec.Y'Img);
                           end if;

                           X := Prev_Rec.X + Prev_Rec.Width
                             + Inter_Node_Gap (Prev_Ref, Ref);
                        end;
                     end if;

                     Node.Rectangle.X := X;
                     Node.Rectangle.Y := Y - Node.Rectangle.Height / 2;

                     if Config.Debug_Layout then
                        Ada.Text_IO.Put_Line
                          ("place:" & Ref'Img & " at"
                           & Node.Rectangle.X'Img
                           & Node.Rectangle.Y'Img);
                     end if;

                     if Col_Index < Row.Count then
                        X := X + Node.Rectangle.Width
                          + Inter_Node_Gap (Ref, Row.Nodes (Col_Index + 1));
                     end if;

                  end;
               end loop;
            end;
            Y := Y + Config.Layout_Row_Size;
         end loop;
      end;

   end Create_Layout;

   ---------------------
   -- Draw_Connection --
   ---------------------

   procedure Draw_Connection
     (From, To : Diagram_Node;
      Straight : Boolean;
      Display  : not null access Komnenos.Displays.Canvas_Display'Class)
   is
      use Komnenos.Entities.Visuals;
      Config : constant Komnenos.Configuration.Diagram_Config :=
                 Komnenos.Configuration.Get_Diagram_Config;
      Start : constant Layout_Point :=
                (From.Rectangle.X + From.Rectangle.Width - 1,
                 From.Rectangle.Y + From.Rectangle.Height / 2);
      Finish : constant Layout_Point :=
                 (To.Rectangle.X,
                  To.Rectangle.Y + To.Rectangle.Height / 2);

   begin

      if Config.Debug_Layout then
         Ada.Text_IO.Put_Line
           ("draw connection:"
            & From.Reference'Img
            & ":" & Start.X'Img & Start.Y'Img
            & " -->"
            & To.Reference'Img
            & ":" & Finish.X'Img & Finish.Y'Img);
      end if;

      if Finish.X > Start.X then
         if Straight then
            Display.Draw_Line
              (Layer  => Komnenos.Displays.Base,
               Line   => (Start, Finish),
               Width  => Config.Connector_Width,
               Colour => Komnenos.Colours.Black,
               Curved => False,
               Arrow  => To.Style /= Internal);
         elsif To.Row > From.Row then
            declare
               use Komnenos.Displays;
               Path : constant Komnenos.Displays.Turtle_Path :=
                        (Turn (South, 6),
                         Move
                           (Pixel_Offset'Max
                              (Finish.Y - Start.Y - 12, 0)),
                         Turn (East, 6),
                         Move
                           (Pixel_Offset'Max
                              (Finish.X - Start.X - 12, 0)));
            begin
               Display.Draw_Turtle_Path
                 (Layer           => Komnenos.Displays.Base,
                  Start_Location  => Start,
                  Start_Direction => East,
                  Path            => Path,
                  Width           => Config.Connector_Width,
                  Colour          => Komnenos.Colours.Black,
                  Arrow           => To.Style /= Internal);
            end;

         elsif To.Row < From.Row then
            declare
               use Komnenos.Displays;
               Path : constant Komnenos.Displays.Turtle_Path :=
                        (Move
                           (Pixel_Offset'Max
                              (Finish.X - Start.X - 12, 0)),
                         Turn (North, 6),
                         Move
                           (Pixel_Offset'Max
                              (Start.Y - Finish.Y - 12, 0)),
                         Turn (East, 6),
                         Move (6));
            begin
               Display.Draw_Turtle_Path
                 (Layer           => Komnenos.Displays.Base,
                  Start_Location  => Start,
                  Start_Direction => East,
                  Path            => Path,
                  Width           => Config.Connector_Width,
                  Colour          => Komnenos.Colours.Black,
                  Arrow           => To.Style /= Internal);
            end;

         else
            declare
               use Komnenos.Displays;
               Path : constant Komnenos.Displays.Turtle_Path :=
                        (Turn (North, 6),
                         Move (15),
                         Turn (East, 6),
                         Move (Pixel_Offset'Max
                           (Finish.X - Start.X - 24, 0)),
                         Turn (South, 6),
                         Move (15),
                         Turn (East, 6));
            begin
               Display.Draw_Turtle_Path
                 (Layer           => Komnenos.Displays.Base,
                  Start_Location  => Start,
                  Start_Direction => East,
                  Path            => Path,
                  Width           => Config.Connector_Width,
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
                      Turn (East, 6));
         begin
            Display.Draw_Turtle_Path
              (Layer           => Komnenos.Displays.Base,
               Start_Location  => Start,
               Start_Direction => East,
               Path            => Path,
               Width           => Config.Connector_Width,
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
            end;
         end loop;

         Create_Layout (Fragment.Nodes);

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
                  Straight => Conn.Straight,
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
   is null;
--     begin
--        Diagram.Nodes (Node).X := X;
--        Diagram.Nodes (Node).Y := Y;
--        Diagram.Columns := Natural'Max (Diagram.Columns, X);
--        Diagram.Rows := Natural'Max (Diagram.Rows, Y);
--     end Move_Node;

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
        (Reference   => Diagram.Nodes.Last_Index + 1,
         Rectangle   => (0, 0, 1, 1),
         Style       => Style,
         Label_Text  => +Label_Text,
         Label_Style => Label_Style,
         Tool_Tip    => +Tool_Tip,
         Link        => Komnenos.Entities.Entity_Reference (Link),
         Connections => Node_Connection_Lists.Empty_List,
         Row         => 1);
   begin
      Diagram.Nodes.Append (Node);
      Diagram.Columns := Natural'Max (Diagram.Columns, X);
      Diagram.Rows := Natural'Max (Diagram.Rows, Y);

      if Komnenos.Configuration.Get_Diagram_Config.Debug_Layout then
         Ada.Text_IO.Put_Line
           ("put_node:" & Node_Reference'Image (Diagram.Nodes.Last_Index)
            & ": "
            & Style'Img
            & ": "
            & (if Label_Text = "" then "(no label)" else Label_Text));
      end if;

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
           (Layer     => Komnenos.Displays.Base,
            Rectangle => Node.Rectangle,
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
           (Layer             => Komnenos.Displays.Base,
            Rectangle         => Node.Rectangle,
            Border_Colour     => Komnenos.Colours.Black,
            Background_Colour => Komnenos.Colours.White,
            Filled            => False,
            Corner_Radius     => Corner_Radius);
      end;

   end Render;

end Komnenos.Fragments.Diagrams;
