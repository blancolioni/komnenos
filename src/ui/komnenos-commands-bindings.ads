with Komnenos.Keys.Bindings;

package Komnenos.Commands.Bindings is

   package Komnenos_Command_Bindings is
     new Komnenos.Keys.Bindings (Komnenos_Command);

   type Binding_Table is
     new Komnenos_Command_Bindings.Binding_Table with null record;

   procedure Default_Bindings
     (Table : in out Binding_Table'Class);

end Komnenos.Commands.Bindings;
