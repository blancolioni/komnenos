with Komnenos.Keys.Sequences;

with Komnenos.Commands.Standard_Commands;

package body Komnenos.Commands.Bindings is

   ----------------------
   -- Default_Bindings --
   ----------------------

   procedure Default_Bindings
     (Table : in out Binding_Table'Class)
   is
      use Komnenos.Keys;

      procedure Bind
        (Key : Komnenos.Keys.Komnenos_Key;
         Name : String);

      ----------
      -- Bind --
      ----------

      procedure Bind
        (Key : Komnenos.Keys.Komnenos_Key;
         Name : String)
      is
         Sequence : Komnenos.Keys.Sequences.Key_Sequence;
         Base_Command : constant Root_Komnenos_Command'Class :=
                          Standard_Commands.Standard_Table.Get_Command (Name);
         Command      : constant Komnenos_Command :=
                          new Root_Komnenos_Command'Class'(Base_Command);
      begin
         Komnenos.Keys.Sequences.Add_Key (Sequence, Key);
         Table.Add_Binding
           (Sequence, Command);
      end Bind;

   begin
      Table.Create;
      Bind (Komnenos.Keys.Up_Arrow, "previous-line");
      Bind (Komnenos.Keys.Down_Arrow, "next-line");
      Bind (Komnenos.Keys.Left_Arrow, "backward-character");
      Bind (Komnenos.Keys.Right_Arrow, "forward-character");

      Bind (Komnenos.Keys.Line_Feed, "new-line");
      Bind (Komnenos.Keys.Carriage_Return, "new-line");
      Bind (Komnenos.Keys.Back_Space, "delete-backward-character");

      for Ch in Character range ' ' .. '~' loop
         Bind (Komnenos.Keys.Character_Key (Ch),
               "insert-character" & Integer'Image (-Character'Pos (Ch)));
      end loop;

   end Default_Bindings;

end Komnenos.Commands.Bindings;
