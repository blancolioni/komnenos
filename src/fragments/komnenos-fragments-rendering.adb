with Komnenos.Layout;
with Komnenos.Messages;
with Komnenos.Programs;

package body Komnenos.Fragments.Rendering is

   use Komnenos.Rendering;

   type Root_Fragment_Renderer is new Root_Komnenos_Renderer with
      record
         Fragment      : Fragment_Type;
         Entity_Table  : access Komnenos.Entities.Entity_Table_Interface'Class;
      end record;

   overriding procedure Set_Text
     (Renderer  : in out Root_Fragment_Renderer;
      Terminal  : in     Source_Tree;
      Line      : in     Line_Number;
      Column    : in     Column_Number;
      Class     : in     String;
      Text      : in     String);

   overriding
   procedure Begin_Render (Renderer : in out Root_Fragment_Renderer);

   overriding
   procedure End_Render (Renderer : in out Root_Fragment_Renderer);

   overriding
   procedure Set_Point (Renderer : in out Root_Fragment_Renderer;
                        Line      : in     Line_Number;
                        Column    : in     Column_Number);

   function Get_Tooltip
     (Terminal : Source_Tree)
      return String;

   ------------------
   -- Begin_Render --
   ------------------

   overriding
   procedure Begin_Render (Renderer : in out Root_Fragment_Renderer) is
   begin
      Renderer.Fragment.Clear;
      Renderer.Set_Current_Position (1, 1);
   end Begin_Render;

   ----------------
   -- End_Render --
   ----------------

   overriding
   procedure End_Render (Renderer : in out Root_Fragment_Renderer) is
   begin
      null;
--        Renderer.Fragment.Display.Update;
--        Renderer.Fragment.Display.Set_Point (Renderer.Pos);
   end End_Render;

   -----------------------
   -- Fragment_Renderer --
   -----------------------

   function Fragment_Renderer
     (Target : Fragment_Type;
      Entity_Table : access Komnenos.Entities.Entity_Table_Interface'Class)
      return Komnenos_Renderer
   is
   begin
      return Result : Root_Fragment_Renderer do
         Result.Fragment := Target;
         Result.Entity_Table := Entity_Table;
      end return;
   end Fragment_Renderer;

   -----------------
   -- Get_Tooltip --
   -----------------

   function Get_Tooltip
     (Terminal : Source_Tree)
      return String
   is
      use Ada.Strings.Unbounded;
      use Komnenos.Messages;
      Msg_List : Komnenos.Messages.Message_List;
      Result   : Unbounded_String;
   begin
      if Terminal.Get_Inherited_Message_Level > No_Message then
         declare
            use Komnenos.Programs;
            T : Program_Tree := Terminal;
         begin
            while T /= null and then not T.Has_Messages loop
               T := T.Program_Parent;
            end loop;

            if T = null then
               return "no message";
            else
               T.Get_Messages (Msg_List);
               for I in 1 .. Message_Count (Msg_List) loop
                  Result := Result
                    & Get_Message_Text (Get_Message (Msg_List, I))
                    & Character'Val (10);
               end loop;
            end if;
         end;
      end if;
      return To_String (Result);
   end Get_Tooltip;

   ---------------
   -- Set_Point --
   ---------------

   overriding
   procedure Set_Point (Renderer  : in out Root_Fragment_Renderer;
                        Line      : in     Line_Number;
                        Column    : in     Column_Number)
   is
   begin
      Renderer.Set_Current_Position (Line, Column);
   end Set_Point;

   --------------
   -- Set_Text --
   --------------

   overriding procedure Set_Text
     (Renderer  : in out Root_Fragment_Renderer;
      Terminal  : in     Source_Tree;
      Line      : in     Line_Number;
      Column    : in     Column_Number;
      Class     : in     String;
      Text      : in     String)
   is
      use Komnenos.Layout;
      use Komnenos.Styles;
      Style : constant Komnenos_Style :=
                Komnenos.Themes.Active_Theme.Style
                  (Class, Komnenos.Themes.Normal);
      Reference  : Komnenos.Entities.Entity_Reference := null;
      Current_Line : Line_Number := Renderer.Current_Line;
      Current_Col  : Column_Number := Renderer.Current_Column;
   begin

--        Ada.Text_IO.Put_Line
--          (Position'Image (Terminal.Layout_Start_Position)
--           & Line'Img & Column'Img & " " & Text);

      while Current_Line < Line
        or else Current_Col > Column
      loop
         Renderer.Fragment.New_Line;
         Current_Line := Current_Line + 1;
         Current_Col := 1;
      end loop;

      declare
         Space_Count : constant Natural :=
                         Natural (Column - Current_Col);
         Spaces      : constant String (1 .. Space_Count) :=
                         (others => ' ');
      begin
         if Space_Count > 0 then
            Renderer.Fragment.Put
              (Spaces, Komnenos.Themes.Active_Theme.Default_Style,
               "", null);
         end if;
      end;

      if Renderer.Entity_Table /= null
        and then Terminal.Location_Line > 0
        and then Terminal.Location_Column > 0
      then
         declare
            References : constant Komnenos.Entities.Array_Of_Entities :=
                           Renderer.Entity_Table.Cross_References
                             (Renderer.Fragment.File_Name,
                              Terminal.Location_Line,
                              Terminal.Location_Column);
         begin
            if References'Length > 0 then
               Reference := References (References'First);
            end if;
         end;
      end if;

      Renderer.Fragment.Put (Text, Style, Get_Tooltip (Terminal), Reference);

      Renderer.Set_Current_Position
        (Line, Column + Column_Offset (Text'Length));
   end Set_Text;

end Komnenos.Fragments.Rendering;
