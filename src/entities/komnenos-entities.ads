private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Containers.Vectors;
private with Ada.Containers.Indefinite_Vectors;

private with Ada.Strings.Fixed.Equal_Case_Insensitive;
private with Ada.Strings.Fixed.Hash_Case_Insensitive;
private with Ada.Strings.Unbounded;

private with WL.String_Maps;

with Komnenos.Styles;

--  with Komnenos.Commands;
with Komnenos.Session_Objects;
with Komnenos.Source;

with Aqua;

package Komnenos.Entities is

   type Entity_Visual is interface;

   function X (Visual : Entity_Visual) return Pixel_Position is abstract;
   function Y (Visual : Entity_Visual) return Pixel_Position is abstract;
   function Width (Visual : Entity_Visual) return Pixel_Length is abstract;
   function Height (Visual : Entity_Visual) return Pixel_Length is abstract;

   type Entity_Visual_Access is access all Entity_Visual'Class;

   type Root_Entity_Reference is abstract tagged private;

   function Name
     (Entity   : Root_Entity_Reference)
      return String;

   function Class_Name
     (Entity   : Root_Entity_Reference)
      return String;

   function Text
     (Item : Root_Entity_Reference)
      return String;

   function Show
     (Item           : Root_Entity_Reference;
      Recursive_Show : access
        function (Value : Aqua.Word) return String)
      return String;

   function Get_Property
     (Entity   : in out Root_Entity_Reference;
      Name     : in String)
      return String;

   function Has_Property
     (Entity   : Root_Entity_Reference;
      Name     : in String)
      return Boolean;

   procedure Set_Content
     (Visual : in out Entity_Visual;
      Entity : access Root_Entity_Reference'Class)
   is abstract;

   function Get_Content
     (Visual : Entity_Visual)
      return access Root_Entity_Reference'class
      is abstract;

   procedure Clear (Visual : in out Entity_Visual) is abstract;

   procedure Invalidate
     (Visual   : not null access Entity_Visual)
   is abstract;

   type Text_Entity_Visual is interface and Entity_Visual;

   procedure Put
     (Visual   : in out Text_Entity_Visual;
      Text     : in     String;
      Style    : in     Komnenos.Styles.Komnenos_Style;
      Tool_Tip : in     String;
      Link     : access Root_Entity_Reference'Class)
   is abstract;

   procedure New_Line (Visual : in out Text_Entity_Visual) is abstract;

   procedure Set_Cursor
     (Visual   : in out Text_Entity_Visual;
      Cursor   : Cursor_Type;
      Position : in     Text_Position)
   is abstract;

   procedure Insert_At_Cursor
     (Visual : in out Text_Entity_Visual;
      Text   : String)
   is abstract;

   procedure Delete_From_Cursor
     (Visual   : in out Text_Entity_Visual;
      Movement : Text_Movement)
   is abstract;

   procedure Put_Line
     (Visual   : in out Text_Entity_Visual'Class;
      Text     : in     String;
      Style    : in     Komnenos.Styles.Komnenos_Style;
      Tool_Tip : in     String := "";
      Link     : access Root_Entity_Reference'Class := null);

   function Identifier
     (Item : Root_Entity_Reference'Class)
      return String;

   function Class
     (Item : Root_Entity_Reference'Class)
      return String;

   function Display_Text
     (Item : Root_Entity_Reference)
      return String;

   function Description
     (Item : Root_Entity_Reference)
      return String;

   function Key
     (Item : Root_Entity_Reference'Class)
      return String;

   function Path
     (Item : Root_Entity_Reference)
      return String;

   function Top_Level
     (Item : Root_Entity_Reference)
      return Boolean
   is (True);

   function Get_Cursor
     (Entity : Root_Entity_Reference;
      Cursor : Cursor_Type)
      return Text_Position;

   function Get_Line
     (Entity : Root_Entity_Reference;
      Position : Text_Position)
      return Line_Number;

   function Get_Column
     (Entity   : Root_Entity_Reference;
      Position : Text_Position)
      return Column_Number;

   function Get_Start_Of_Line
     (Entity : Root_Entity_Reference;
      Line   : Line_Number)
      return Text_Position;

   procedure Set_Cursor
     (Entity       : in out Root_Entity_Reference;
      Cursor       : Cursor_Type;
      New_Position : Text_Position)
   is null;

   procedure Move_Cursor
     (Item     : in out Root_Entity_Reference;
      Cursor   : Cursor_Type;
      Movement : Text_Movement)
   is null;

   procedure Insert_Text
     (Item     : in out Root_Entity_Reference;
      Text     : String)
   is null;

   procedure Delete_Region
     (Item   : in out Root_Entity_Reference)
   is null;

   function Get_Region_Text
     (Item  : Root_Entity_Reference;
      End_1 : Cursor_Type;
      End_2 : Cursor_Type)
      return String;

--     function Get_Offset_Position
--       (Item     : in out Root_Entity_Reference;
--        Cursor   : Cursor_Type;
--        Movement : Cursor_Movement_Type;
--        Offset   : Text_Position_Offset)
--        return Text_Position
--        is abstract;

   procedure Create
     (Item             : in out Root_Entity_Reference'Class;
      Key              : String;
      Identifier       : String;
      Full_Name        : String;
      Class_Name       : String;
      Path             : String;
      Display_Text     : String;
      Description      : String);

--     procedure Execute_Command
--       (Item    : not null access Root_Entity_Reference;
--        Command : Komnenos.Commands.Komnenos_Command);

   type Entity_Reference is access all Root_Entity_Reference'Class;

   type Entity_Table_Interface is interface;

   function Table_Name (Table : Entity_Table_Interface) return String
                        is abstract;

   type Entity_Table_Access is access all Entity_Table_Interface'Class;

   procedure Select_Entity
     (Entity : not null access Root_Entity_Reference;
      Table  : access Entity_Table_Interface'Class;
      Parent : access Entity_Visual'Class;
      Visual : access Entity_Visual'Class;
      Offset : Pixel_Position)
   is abstract;

   procedure Render
     (Entity : not null access Root_Entity_Reference;
      Visual : not null access Entity_Visual'Class)
   is abstract;

   type Program_Store_Interface is interface
     and Komnenos.Session_Objects.Session_Object_Interface;

   type Program_Store_Access is access all Program_Store_Interface'Class;

   function Program_Store_Name
     (Store : Program_Store_Interface) return String
      is abstract;

   procedure Load
     (Store : not null access Program_Store_Interface)
   is abstract;

   procedure Save
     (Store : not null access Program_Store_Interface)
   is abstract;

   procedure On_Edit
     (Store : in out Program_Store_Interface;
      Item  : not null access Komnenos.Source.Source_Tree_Interface'Class)
   is null;

   function Find_File
     (Store : Program_Store_Interface;
      Name  : String)
      return String
      is abstract;

   function Program_Store
     (Table : Entity_Table_Interface)
      return access Program_Store_Interface'Class
      is abstract;

   function Null_Program_Store return Program_Store_Access;

   function New_Table
     (Name  : String;
      Store : not null access Program_Store_Interface'Class)
      return Entity_Table_Access;

   type Array_Of_Entities is array (Positive range <>) of Entity_Reference;

--     type Cross_Reference_Type is
--       (Body_Entity, Type_Completion, Type_Discriminant,
--        Object_Definition, End_Of_Spec, Abstract_Type,
--        Implicit_Reference, Implicit_Reference_Child_Parent,
--        End_Label, Modification,
--        Primitive_Operation, Overriding_Primitive_Operation,
--        Reference, Dispatching_Subprogram_Reference,
--        Static_Subprogram_Reference, End_Of_Body,
--        With_Package, Type_Extension, Generic_Formal_Parameter,
--        Subprogram_In_Parameter, Subprogram_In_Out_Parameter,
--        Subprogram_Out_Parameter, Subprogram_Access_Parameter,
--        Unknown);
--
--     type Cross_Reference_Enable_Array is
--       array (Cross_Reference_Type) of Boolean;

   procedure Add_Entity
     (Table        : in out Entity_Table_Interface;
      Key          : String;
      Item         : Entity_Reference)
   is abstract;

   function Find
     (Table      : Entity_Table_Interface;
      Name       : String;
      Class_Name : String)
      return Entity_Reference
      is abstract;

   procedure Add_Cross_Reference
     (Table        : in out Entity_Table_Interface;
      Item         : Entity_Reference;
      Referrer     : Entity_Reference;
      File_Name    : String;
      Line         : Line_Number;
      Column       : Column_Number;
      Ref_Type     : String)
   is abstract;

   function Cross_References
     (Table        : Entity_Table_Interface;
      File_Name    : String;
      Line         : Line_Number;
      Column       : Column_Number;
      Enabled      : String := "all")
      return Array_Of_Entities
      is abstract;

   type Reference_Record is private;

   function Get_Referrer
     (Reference : Reference_Record)
      return Entity_Reference;

   type Reference_Record_Array is
     array (Positive range <>) of Reference_Record;

   function References
     (Table  : Entity_Table_Interface;
      Entity : Entity_Reference)
      return Reference_Record_Array
      is abstract;

   function Exists
     (Table : Entity_Table_Interface;
      Key   : String)
      return Boolean
      is abstract;

   function Get
     (Table : Entity_Table_Interface;
      Key   : String)
      return Entity_Reference
      is abstract;

   procedure Sort
     (Table : in out Entity_Table_Interface)
   is abstract;

   procedure Iterate
     (Table   : Entity_Table_Interface;
      Filter  : in String;
      Process : not null access
        procedure (Item : Entity_Reference);
      Top_Level_Only : Boolean := True)
   is abstract;

   function Reference_File_Name
     (Table     : Entity_Table_Interface;
      Reference : Reference_Record)
      return String
      is abstract;

   function To_String
     (Table     : Entity_Table_Interface'Class;
      Reference : Reference_Record)
      return String;

   function File_Line
     (Reference : Reference_Record)
      return Line_Number;

   function File_Column
     (Reference : Reference_Record)
      return Column_Number;

   function Location_Reference_Type
     (Reference : Reference_Record)
      return String;

   type Entity_Table is
     new Entity_Table_Interface with private;

   overriding procedure Add_Entity
     (Table        : in out Entity_Table;
      Key          : String;
      Item         : Entity_Reference);

   overriding function Exists
     (Table : Entity_Table;
      Key   : String)
      return Boolean;

   overriding function Get
     (Table : Entity_Table;
      Key   : String)
      return Entity_Reference;

   overriding function Find
     (Table      : Entity_Table;
      Name       : String;
      Class_Name : String)
      return Entity_Reference;

   overriding procedure Add_Cross_Reference
     (Table        : in out Entity_Table;
      Item         : Entity_Reference;
      Referrer     : Entity_Reference;
      File_Name    : String;
      Line         : Line_Number;
      Column       : Column_Number;
      Ref_Type     : String);

   overriding function Cross_References
     (Table        : Entity_Table;
      File_Name    : String;
      Line         : Line_Number;
      Column       : Column_Number;
      Enabled      : String := "")
      return Array_Of_Entities;

   overriding function References
     (Table  : Entity_Table;
      Entity : Entity_Reference)
      return Reference_Record_Array;

   overriding procedure Sort
     (Table   : in out Entity_Table);

   overriding procedure Iterate
     (Table   : Entity_Table;
      Filter  : in String;
      Process : not null access
        procedure (Item : Entity_Reference);
      Top_Level_Only : Boolean := True);

   overriding function Reference_File_Name
     (Table     : Entity_Table;
      Reference : Reference_Record)
      return String;

private

   type File_Id is new Positive;

   type Reference_Record is
      record
         Referrer : Komnenos.Entities.Entity_Reference;
         File     : File_Id;
         Ref_Type : Ada.Strings.Unbounded.Unbounded_String;
         Line     : Line_Number;
         Column   : Column_Number;
      end record;

   package Reference_Vectors is
     new Ada.Containers.Vectors
       (Index_Type   => File_Id,
        Element_Type => Reference_Record);

   package File_Name_Maps is
     new WL.String_Maps (File_Id);

   package File_Name_Vectors is
     new Ada.Containers.Indefinite_Vectors (File_Id, String);

   package Entity_Vectors is
     new Ada.Containers.Vectors
       (Positive, Entity_Reference);

   package String_Property_Maps is
     new WL.String_Maps (String);

   type Root_Entity_Reference is
     abstract tagged
      record
         String_Props   : String_Property_Maps.Map;
--           Key            : Ada.Strings.Unbounded.Unbounded_String;
--           Identifier     : Ada.Strings.Unbounded.Unbounded_String;
--           Full_Name      : Ada.Strings.Unbounded.Unbounded_String;
--           Class          : Ada.Strings.Unbounded.Unbounded_String;
--           Display_Text   : Ada.Strings.Unbounded.Unbounded_String;
--           Description    : Ada.Strings.Unbounded.Unbounded_String;
--           Path           : Ada.Strings.Unbounded.Unbounded_String;
         References     : Reference_Vectors.Vector;
         Table          : Entity_Table_Access;
      end record;

   Name_Property         : constant String := "name";
   Key_Property          : constant String := "key";
   Identifier_Property   : constant String := "identifier";
   Full_Name_Property    : constant String := "full_name";
   Class_Property        : constant String := "class";
   Display_Text_Property : constant String := "display_text";
   Description_Property  : constant String := "description";
   Path_Property         : constant String := "path";

   function Get_String_Property
     (Item : Root_Entity_Reference'Class;
      Name : String)
      return String
   is (Item.String_Props.Element (Name));

   procedure Set
     (Item  : in out Root_Entity_Reference'Class;
      Name  : String;
      Value : String);

   function Name
     (Entity : Root_Entity_Reference) return String
   is (Entity.Get_String_Property (Name_Property));

   function Key
     (Item : Root_Entity_Reference'Class)
      return String
   is (Item.Get_String_Property (Key_Property));

   function Identifier
     (Item : Root_Entity_Reference'Class)
      return String
   is (Item.Get_String_Property (Identifier_Property));

   function Description
     (Item : Root_Entity_Reference)
      return String
   is (Item.Get_String_Property (Description_Property));

   function Display_Text
     (Item : Root_Entity_Reference)
      return String
   is (Item.Get_String_Property (Display_Text_Property));

   function Path
     (Item : Root_Entity_Reference)
      return String
   is (Item.Get_String_Property (Path_Property));

   function Class
     (Item : Root_Entity_Reference'Class)
      return String
   is (Item.Get_String_Property (Class_Property));

   package Entity_Maps is
     new WL.String_Maps (Entity_Reference);

   package List_Of_Entities is
     new Ada.Containers.Doubly_Linked_Lists (Entity_Reference);

   package Entity_Name_Maps is
     new WL.String_Maps (List_Of_Entities.List, List_Of_Entities."=");

   type Cross_Reference_Record is
      record
         Entity   : Entity_Reference;
         Ref_Type : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   package Cross_Reference_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Cross_Reference_Record);

   package Cross_Reference_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Cross_Reference_Lists.List,
        Hash            => Ada.Strings.Fixed.Hash_Case_Insensitive,
        Equivalent_Keys => Ada.Strings.Fixed.Equal_Case_Insensitive,
        "="             => Cross_Reference_Lists."=");

   type Entity_Table is new Entity_Table_Interface with
      record
         Name          : Ada.Strings.Unbounded.Unbounded_String;
         File_Map      : File_Name_Maps.Map;
         File_Vector   : File_Name_Vectors.Vector;
         Table         : Entity_Vectors.Vector;
         Map           : Entity_Maps.Map;
         Name_Map      : Entity_Name_Maps.Map;
         Full_Name_Map : Entity_Maps.Map;
         X_Ref         : Cross_Reference_Maps.Map;
         Store         : access Program_Store_Interface'Class;
      end record;

   overriding function Table_Name
     (Table : Entity_Table)
      return String
   is (Ada.Strings.Unbounded.To_String (Table.Name));

   overriding function Program_Store
     (Table : Entity_Table)
      return access Program_Store_Interface'Class
   is (Table.Store);

end Komnenos.Entities;
