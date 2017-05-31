private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;

with Komnenos.Entities.Visuals;

package Komnenos.Fragments.Diagrams is

   type Diagram_Fragment_Type is
     new Root_Fragment_Type
     and Komnenos.Entities.Visuals.Diagram_Visual
   with private;

   type Diagram_Fragment is access all Diagram_Fragment_Type'Class;

   overriding procedure Put_Node
     (Diagram     : in out Diagram_Fragment_Type;
      Key         : String;
      X, Y        : Positive;
      Style       : Komnenos.Entities.Visuals.Node_Style;
      Label_Text  : String;
      Label_Style : Komnenos.Styles.Komnenos_Style;
      Tool_Tip    : String;
      Link        : access Komnenos.Entities.Root_Entity_Reference'Class);

   overriding procedure Move_Node
     (Diagram     : in out Diagram_Fragment_Type;
      Key         : String;
      X, Y        : Positive);

   overriding procedure Connect_Nodes
     (Diagram     : in out Diagram_Fragment_Type;
      From_Key    : String;
      From_Edge   : Komnenos.Entities.Visuals.Node_Edge;
      To_Key      : String;
      To_Edge     : Komnenos.Entities.Visuals.Node_Edge);

   overriding procedure Invalidate
     (Fragment : not null access Diagram_Fragment_Type);

   function New_Diagram
     return Diagram_Fragment;

   function New_Diagram
     (Entity : not null access
        Komnenos.Entities.Root_Entity_Reference'Class)
     return Diagram_Fragment;

private

   type Node_Connection is
      record
         From, To : Positive;
         From_Edge, To_Edge : Komnenos.Entities.Visuals.Node_Edge;
      end record;

   package Node_Connection_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Node_Connection);

   type Diagram_Node is
      record
         Key         : Ada.Strings.Unbounded.Unbounded_String;
         X, Y        : Positive;
         Rectangle   : Layout_Rectangle;
         Style       : Komnenos.Entities.Visuals.Node_Style;
         Label_Text  : Ada.Strings.Unbounded.Unbounded_String;
         Label_Style : Komnenos.Styles.Komnenos_Style;
         Tool_Tip    : Ada.Strings.Unbounded.Unbounded_String;
         Link        : Komnenos.Entities.Entity_Reference;
         Connections : Node_Connection_Lists.List;
      end record;

   package Node_Vectors is
     new Ada.Containers.Vectors (Positive, Diagram_Node);

   type Diagram_Fragment_Type is
     new Root_Fragment_Type
     and Komnenos.Entities.Visuals.Diagram_Visual with
      record
         Rows        : Natural := 0;
         Columns     : Natural := 0;
         Nodes       : Node_Vectors.Vector;
      end record;

   overriding procedure Clear (Fragment : in out Diagram_Fragment_Type);

end Komnenos.Fragments.Diagrams;