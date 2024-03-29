with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Fixed.Less_Case_Insensitive;
with Ada.Text_IO;

package body Komnenos.Entities is

   type Null_Program_Store_Record is
     new Program_Store_Interface with null record;

   overriding function Config_Name
     (Item : Null_Program_Store_Record)
      return String
   is ("null_program_store");

   overriding function Program_Store_Name
     (Item : Null_Program_Store_Record)
      return String
   is ("null_program_store");

   overriding function Find_File
     (Item : Null_Program_Store_Record;
      Name : String)
      return String;

   overriding procedure To_Config
     (Item   : Null_Program_Store_Record;
      Config : in out Tropos.Configuration)
   is null;

   overriding procedure From_Config
     (Item   : not null access Null_Program_Store_Record;
      Config : Tropos.Configuration)
   is null;

   overriding procedure Load
     (Store : not null access Null_Program_Store_Record)
   is null;

   overriding procedure Save
     (Store : not null access Null_Program_Store_Record)
   is null;

   Local_Null_Program_Store : aliased Null_Program_Store_Record;

   function Get_Key (File_Name : String;
                     Line      : Line_Number;
                     Column    : Column_Number)
                     return String;

   function Display_Text_Less_Than
     (Left, Right : Entity_Reference)
      return Boolean
   is (Ada.Strings.Fixed.Less_Case_Insensitive
       (Left.Display_Text, Right.Display_Text));

   package Entity_Sorting is
     new Entity_Vectors.Generic_Sorting
       ("<" => Display_Text_Less_Than);

   -------------------------
   -- Add_Cross_Reference --
   -------------------------

   overriding procedure Add_Cross_Reference
     (Table        : in out Entity_Table;
      Item         : Entity_Reference;
      Referrer     : Entity_Reference;
      File_Name    : String;
      Line         : Line_Number;
      Column       : Column_Number;
      Ref_Type     : String)
   is
      Key : constant String := Get_Key (File_Name, Line, Column);
      Ref_Store : constant Ada.Strings.Unbounded.Unbounded_String :=
                    Ada.Strings.Unbounded.To_Unbounded_String (Ref_Type);
   begin
      if Table.X_Ref.Contains (Key) then
         Table.X_Ref (Key).Append
           ((Item, Ref_Store));
      else
         declare
            List : Cross_Reference_Lists.List;
         begin
            List.Append ((Item, Ref_Store));
            Table.X_Ref.Insert (Key, List);
         end;
      end if;

      if not Table.File_Map.Contains (File_Name) then
         Table.File_Vector.Append (File_Name);
         Table.File_Map.Insert (File_Name, Table.File_Vector.Last_Index);
      end if;

      Item.References.Append
        (Reference_Record'
           (Referrer, Table.File_Map (File_Name),  Ref_Store, Line, Column));

   end Add_Cross_Reference;

   ----------------
   -- Add_Entity --
   ----------------

   overriding procedure Add_Entity
     (Table : in out Entity_Table;
      Key   : String;
      Item  : Entity_Reference)
   is
      Name      : constant String := Item.Name;
      Full_Name : constant String :=
                    Item.Get_String_Property (Full_Name_Property);
   begin
      Item.String_Props (Key_Property) := Key;
      Table.Table.Append (Item);
      Table.Map.Insert (Key, Item);
      if Table.Name_Map.Contains (Name) then
         Table.Name_Map (Name).Append (Item);
      else
         declare
            List : List_Of_Entities.List;
         begin
            List.Append (Item);
            Table.Name_Map.Insert (Name, List);
         end;
      end if;

      if not Table.Full_Name_Map.Contains (Full_Name) then
         Table.Full_Name_Map.Insert (Full_Name, Item);
      end if;

   end Add_Entity;

   function Class_Name
     (Entity   : Root_Entity_Reference)
      return String
   is ("komnenos__entity");

   ------------
   -- Create --
   ------------

   procedure Create
     (Item             : in out Root_Entity_Reference'Class;
      Key              : String;
      Identifier       : String;
      Full_Name        : String;
      Class_Name       : String;
      Path             : String;
      Display_Text     : String;
      Description      : String)
   is
   begin

      Item.Set (Key_Property, Key);
      Item.Set (Identifier_Property, Identifier);
      Item.Set (Name_Property, Identifier);
      Item.Set (Full_Name_Property, Full_Name);
      Item.Set (Class_Property, Class_Name);
      Item.Set (Path_Property, Path);

      if Display_Text = "" then
         Item.Set (Display_Text_Property, Identifier);
      else
         Item.Set (Display_Text_Property, Display_Text);
      end if;

      if Description = "" then
         Item.Set (Description_Property, Identifier);
      else
         Item.Set (Description_Property, Description);
      end if;

   end Create;

   ----------------------
   -- Cross_References --
   ----------------------

   overriding function Cross_References
     (Table        : Entity_Table;
      File_Name    : String;
      Line         : Line_Number;
      Column       : Column_Number;
      Enabled      : String := "")
      return Array_Of_Entities
   is
      Key : constant String := Get_Key (File_Name, Line, Column);
   begin
      if False then
         Ada.Text_IO.Put_Line
           ("xref [" & Table.Table_Name & "]" & Line'Img & Column'Img
            & ": " & Enabled & " " & Key);
      end if;

      if Table.X_Ref.Contains (Key) then
         declare
            List : constant Cross_Reference_Lists.List :=
                     Table.X_Ref (Key);
            Result : Array_Of_Entities (1 .. Natural (List.Length));
            Count  : Natural := 0;
         begin
            for Element of List loop
               Ada.Text_IO.Put_Line ("   " & Element.Entity.Class
                                     & " " & Element.Entity.Name);
               if Enabled = "all" then
                  Count := Count + 1;
                  Result (Count) := Element.Entity;
               end if;
            end loop;
            return Result (1 .. Count);
         end;
      else
         declare
            Result : Array_Of_Entities (1 .. 0);
         begin
            return Result;
         end;
      end if;
   end Cross_References;

   ---------------------
   -- Execute_Command --
   ---------------------

--     procedure Execute_Command
--       (Item    : not null access Root_Entity_Reference;
--        Command : Komnenos.Commands.Komnenos_Command)
--     is
--        pragma Unreferenced (Item);
--     begin
--        Ada.Text_IO.Put_Line (Komnenos.Commands.Show (Command));
--     end Execute_Command;

   ------------
   -- Exists --
   ------------

   overriding function Exists
     (Table : Entity_Table;
      Key   : String)
      return Boolean
   is
   begin
      return Table.Map.Contains (Key);
   end Exists;

   -----------------
   -- File_Column --
   -----------------

   function File_Column
     (Reference : Reference_Record)
      return Column_Number
   is
   begin
      return Reference.Column;
   end File_Column;

   ---------------
   -- File_Line --
   ---------------

   function File_Line
     (Reference : Reference_Record)
      return Line_Number
   is
   begin
      return Reference.Line;
   end File_Line;

   ----------
   -- Find --
   ----------

   overriding function Find
     (Table      : Entity_Table;
      Name       : String;
      Class_Name : String)
      return Entity_Reference
   is
   begin
      if Table.Name_Map.Contains (Name) then
         for Item of Table.Name_Map (Name) loop
            if Class_Name = ""
              or else Class (Item.all) = Class_Name
            then
               return Item;
            end if;
         end loop;
      end if;
      return null;
   end Find;

   ---------------
   -- Find_File --
   ---------------

   overriding function Find_File
     (Item : Null_Program_Store_Record;
      Name : String)
      return String
   is
      pragma Unreferenced (Item, Name);
   begin
      return "";
   end Find_File;

   ---------
   -- Get --
   ---------

   overriding function Get
     (Table : Entity_Table;
      Key   : String)
      return Entity_Reference
   is
   begin
      return Table.Map.Element (Key);
   end Get;

   ----------------
   -- Get_Column --
   ----------------

   function Get_Column
     (Entity   : Root_Entity_Reference;
      Position : Text_Position)
      return Column_Number
   is
      pragma Unreferenced (Entity, Position);
   begin
      return 1;
   end Get_Column;

   ----------------
   -- Get_Cursor --
   ----------------

   function Get_Cursor
     (Entity : Root_Entity_Reference;
      Cursor : Cursor_Type)
      return Text_Position
   is
      pragma Unreferenced (Entity, Cursor);
   begin
      return 1;
   end Get_Cursor;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (File_Name : String;
                     Line      : Line_Number;
                     Column    : Column_Number)
                     return String
   is
   begin
      return File_Name & Integer'Image (-Integer (Line))
        & Integer'Image (-Integer (Column));
   end Get_Key;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (Entity   : Root_Entity_Reference;
      Position : Text_Position)
      return Line_Number
   is
      pragma Unreferenced (Entity, Position);
   begin
      return 1;
   end Get_Line;

   ------------------
   -- Get_Property --
   ------------------

   function Get_Property
     (Entity   : in out Root_Entity_Reference;
      Name     : in String)
      return String
   is
   begin
      if Entity.String_Props.Contains (Name) then
         return Entity.String_Props.Element (Name);
      else
         return "";
      end if;
   end Get_Property;

   -------------------
   -- Get_Reference --
   -------------------

--     overriding function Get_Reference
--       (Item : Root_Entity_Reference)
--        return Aqua.External_Reference
--     is
--     begin
--        return Item.Aqua_Reference;
--     end Get_Reference;

   ------------------
   -- Get_Referrer --
   ------------------

   function Get_Referrer
     (Reference : Reference_Record)
      return Entity_Reference
   is
   begin
      return Reference.Referrer;
   end Get_Referrer;

   ---------------------
   -- Get_Region_Text --
   ---------------------

   function Get_Region_Text
     (Item  : Root_Entity_Reference;
      End_1 : Cursor_Type;
      End_2 : Cursor_Type)
      return String
   is
      pragma Unreferenced (Item, End_1, End_2);
   begin
      return "";
   end Get_Region_Text;

   -----------------------
   -- Get_Start_Of_Line --
   -----------------------

   function Get_Start_Of_Line
     (Entity : Root_Entity_Reference;
      Line   : Line_Number)
      return Text_Position
   is
      pragma Unreferenced (Entity, Line);
   begin
      return 1;
   end Get_Start_Of_Line;

   ------------------
   -- Has_Property --
   ------------------

   function Has_Property
     (Entity   : Root_Entity_Reference;
      Name     : in String)
      return Boolean
   is (False);

   -------------
   -- Iterate --
   -------------

   overriding procedure Iterate
     (Table   : Entity_Table;
      Filter  : in String;
      Process : not null access
        procedure (Item : Entity_Reference);
      Top_Level_Only : Boolean := True)
   is
      Filter_Text : constant String :=
                      Ada.Characters.Handling.To_Lower (Filter);
   begin
      if Filter = "" then
         for Entity of Table.Table loop
            if not Top_Level_Only or else Entity.Top_Level then
               Process (Entity);
            end if;
         end loop;
      else
         for Entity of Table.Table loop
            if not Top_Level_Only or else Entity.Top_Level then
               declare
                  Match_Text : constant String :=
                                 Ada.Characters.Handling.To_Lower
                                   (Entity.Identifier);
               begin
                  if Ada.Strings.Fixed.Index
                    (Match_Text, Filter_Text)
                    > 0
                  then
                     Process (Entity);
                  end if;
               end;
            end if;
         end loop;
      end if;
   end Iterate;

   -----------------------------
   -- Location_Reference_Type --
   -----------------------------

   function Location_Reference_Type
     (Reference : Reference_Record)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (Reference.Ref_Type);
   end Location_Reference_Type;

   ---------------
   -- New_Table --
   ---------------

   function New_Table
     (Name  : String;
      Store : not null access Program_Store_Interface'Class)
      return Entity_Table_Access
   is
      Rec : Entity_Table;
   begin
      Rec.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
      Rec.Store := Store;
      return Table : constant Entity_Table_Access := new Entity_Table'(Rec);
   end New_Table;

   ------------------------
   -- Null_Program_Store --
   ------------------------

   function Null_Program_Store return Program_Store_Access is
   begin
      return Local_Null_Program_Store'Access;
   end Null_Program_Store;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (Visual   : in out Text_Entity_Visual'Class;
      Text     : in     String;
      Style    : in     Komnenos.Styles.Komnenos_Style;
      Tool_Tip : in     String := "";
      Link     : access Root_Entity_Reference'Class := null)
   is
   begin
      Visual.Put (Text, Style, Tool_Tip, Link);
      Visual.New_Line;
   end Put_Line;

   -------------------------
   -- Reference_File_Name --
   -------------------------

   overriding function Reference_File_Name
     (Table : Entity_Table;
      Reference : Reference_Record)
      return String
   is
   begin
      return Table.File_Vector (Reference.File);
   end Reference_File_Name;

   ----------------
   -- References --
   ----------------

   overriding function References
     (Table  : Entity_Table;
      Entity : Entity_Reference)
      return Reference_Record_Array
   is
      pragma Unreferenced (Table);
      Result : Reference_Record_Array
        (1 .. Natural (Entity.References.Last_Index));
   begin
      for I in Result'Range loop
         Result (I) := Entity.References (File_Id (I));
      end loop;
      return Result;
   end References;

   ---------
   -- Set --
   ---------

   procedure Set
     (Item  : in out Root_Entity_Reference'Class;
      Name  : String;
      Value : String)
   is
   begin
      if Item.String_Props.Contains (Name) then
         Item.String_Props.Replace (Name, Value);
      else
         Item.String_Props.Insert (Name, Value);
      end if;
   end Set;

   -------------------
   -- Set_Reference --
   -------------------

--     overriding procedure Set_Reference
--       (Item : in out Root_Entity_Reference;
--        Reference : Aqua.External_Reference)
--     is
--     begin
--        Item.Aqua_Reference := Reference;
--     end Set_Reference;

   ----------
   -- Show --
   ----------

   function Show
     (Item           : Root_Entity_Reference;
      Recursive_Show : access
        function (Value : Aqua.Word) return String)
      return String
   is
      pragma Unreferenced (Recursive_Show);
   begin
      return "[entity:" & Item.Display_Text & "]";
   end Show;

   ----------
   -- Sort --
   ----------

   overriding procedure Sort
     (Table   : in out Entity_Table)
   is
   begin
      Entity_Sorting.Sort (Table.Table);
   end Sort;

   ----------
   -- Text --
   ----------

   function Text
     (Item : Root_Entity_Reference) return String
   is
   begin
      return Item.Display_Text;
   end Text;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Table     : Entity_Table_Interface'Class;
      Reference : Reference_Record)
      return String
   is
      pragma Unreferenced (Table);
   begin
      return Reference.Referrer.Name
--        return Table.Reference_File_Name (Reference)
--          & ":" & Trim (Natural'Image (Reference.Line), Left)
--          & ":" & Trim (Natural'Image (Reference.Column), Left)
        & " [" & Ada.Strings.Unbounded.To_String (Reference.Ref_Type) & "]";
   end To_String;

end Komnenos.Entities;
