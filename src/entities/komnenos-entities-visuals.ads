package Komnenos.Entities.Visuals is

   type Node_Style is
     (Box, Rounded_Box, Small_Circle, Internal);

   type Diagram_Visual is interface and Entity_Visual;

   procedure Put_Node
     (Visual      : in out Diagram_Visual;
      Key         : String;
      X, Y        : Positive;
      Style       : Node_Style;
      Label_Text  : String;
      Label_Style : Komnenos.Styles.Komnenos_Style;
      Tool_Tip    : String;
      Link        : access Root_Entity_Reference'Class)
   is abstract
     with Pre'Class =>
       Key /= ""
       and then not (Komnenos.Styles."=" (Label_Style, null));

   procedure Move_Node
     (Visual   : in out Diagram_Visual;
      Key      : String;
      X, Y     : Positive)
   is abstract;

   procedure Connect_Nodes
     (Visual   : in out Diagram_Visual;
      From, To : String)
   is abstract;

end Komnenos.Entities.Visuals;
