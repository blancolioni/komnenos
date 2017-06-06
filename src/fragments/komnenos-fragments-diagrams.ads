private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Vectors;

with Komnenos.Entities.Visuals;

package Komnenos.Fragments.Diagrams is

   type Diagram_Fragment_Type is
     new Root_Fragment_Type
     and Komnenos.Entities.Visuals.Diagram_Visual
   with private;

   type Diagram_Fragment is access all Diagram_Fragment_Type'Class;

   overriding function Put_Node
     (Diagram     : in out Diagram_Fragment_Type;
      X, Y        : Positive;
      Style       : Node_Style;
      Label_Text  : String;
      Label_Style : Komnenos.Styles.Komnenos_Style;
      Tool_Tip    : String;
      Link        : access Komnenos.Entities.Root_Entity_Reference'Class)
      return Node_Reference;

   overriding function Put_Sub_Node
     (Diagram     : in out Diagram_Fragment_Type;
      Parent      : Node_Reference;
      Anchor      : Node_Edge;
      Visibility  : Node_Visibility;
      Style       : Node_Style;
      Label_Text  : String;
      Label_Style : Komnenos.Styles.Komnenos_Style;
      Tool_Tip    : String;
      Link        : access Komnenos.Entities.Root_Entity_Reference'Class)
      return Node_Reference;

   overriding procedure Move_Node
     (Diagram : in out Diagram_Fragment_Type;
      Node    : Node_Reference;
      X, Y    : Positive);

   overriding procedure Connect_Nodes
     (Diagram     : in out Diagram_Fragment_Type;
      From        : Node_Reference;
      From_Edge   : Node_Edge;
      To          : Node_Reference;
      To_Edge     : Node_Edge);

   overriding procedure Invalidate
     (Fragment : not null access Diagram_Fragment_Type);

   overriding procedure On_Click
     (Fragment : not null access Diagram_Fragment_Type;
      X, Y     : Pixel_Position;
      Modifier : Komnenos.Keys.Modifier_Keys);

   function New_Diagram
     return Diagram_Fragment;

   function New_Diagram
     (Entity : not null access
        Komnenos.Entities.Root_Entity_Reference'Class)
      return Diagram_Fragment;

private

   function To_String_With_Default
     (S       : Ada.Strings.Unbounded.Unbounded_String;
      Default : String)
      return String
   is (if Ada.Strings.Unbounded."="
       (S, Ada.Strings.Unbounded.Null_Unbounded_String)
       then Default else Ada.Strings.Unbounded.To_String (S));

   type Node_Connection is
      record
         From, To           : Node_Reference;
         From_Edge, To_Edge : Node_Edge;
         Straight           : Boolean;
      end record;

   package Node_Connection_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Node_Connection);

   type Diagram_Node is
      record
         Reference        : Node_Reference;
         Parent_Reference : Node_Reference;
         Has_Parent       : Boolean;
         Rectangle        : Layout_Rectangle;
         Anchor           : Node_Edge;
         Visibility       : Node_Visibility;
         Style            : Node_Style;
         Label_Text       : Ada.Strings.Unbounded.Unbounded_String;
         Label_Style      : Komnenos.Styles.Komnenos_Style;
         Tool_Tip         : Ada.Strings.Unbounded.Unbounded_String;
         Link             : Komnenos.Entities.Entity_Reference;
         Connections      : Node_Connection_Lists.List;
         Row              : Positive;
      end record;

   function Image (Node : Diagram_Node) return String
   is (Node_Reference'Image (Node.Reference)
       & ": "
       & Node.Style'Img
       & ": "
       & (To_String_With_Default
          (Node.Label_Text, "(no label)")));

   package Node_Vectors is
     new Ada.Containers.Vectors (Node_Reference, Diagram_Node);

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
