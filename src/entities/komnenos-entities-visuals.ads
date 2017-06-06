package Komnenos.Entities.Visuals is

   type Diagram_Visual is interface and Entity_Visual;

   function Put_Node
     (Visual      : in out Diagram_Visual;
      X, Y        : Positive;
      Style       : Node_Style;
      Label_Text  : String;
      Label_Style : Komnenos.Styles.Komnenos_Style;
      Tool_Tip    : String;
      Link        : access Root_Entity_Reference'Class)
      return Node_Reference
   is abstract
     with Pre'Class =>
       Label_Text = ""
         or else not (Komnenos.Styles."=" (Label_Style, null));

   function Put_Sub_Node
     (Visual      : in out Diagram_Visual;
      Parent      : Node_Reference;
      Anchor      : Node_Edge;
      Visibility  : Node_Visibility;
      Style       : Node_Style;
      Label_Text  : String;
      Label_Style : Komnenos.Styles.Komnenos_Style;
      Tool_Tip    : String;
      Link        : access Komnenos.Entities.Root_Entity_Reference'Class)
      return Node_Reference
      is abstract
     with Pre'Class =>
       Label_Text = ""
       or else not (Komnenos.Styles."=" (Label_Style, null));

   procedure Move_Node
     (Visual   : in out Diagram_Visual;
      Node     : Node_Reference;
      X, Y     : Positive)
   is abstract;

   procedure Connect_Nodes
     (Visual    : in out Diagram_Visual;
      From      : Node_Reference;
      From_Edge : Node_Edge;
      To        : Node_Reference;
      To_Edge   : Node_Edge)
   is abstract;

end Komnenos.Entities.Visuals;
