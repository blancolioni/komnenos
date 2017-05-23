with Komnenos.Rendering;

package Komnenos.Fragments.Rendering is

   function Fragment_Renderer
     (Target : Fragment_Type;
      Entity_Table : access Komnenos.Entities.Entity_Table_Interface'Class)
      return Komnenos.Rendering.Komnenos_Renderer;

end Komnenos.Fragments.Rendering;
