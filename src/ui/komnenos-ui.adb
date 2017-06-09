with Css.Parser;

with Komnenos.UI.Gtk_UI;

package body Komnenos.UI is

   Local_Current_UI : Komnenos_UI;

   -------------------------
   -- Add_Cross_Reference --
   -------------------------

   overriding procedure Add_Cross_Reference
     (UI           : in out Root_Komnenos_UI;
      Item         : Komnenos.Entities.Entity_Reference;
      Referrer     : Komnenos.Entities.Entity_Reference;
      File_Name    : String;
      Line         : Line_Number;
      Column       : Column_Number;
      Ref_Type     : String)
   is
   begin
      UI.Entities.Add_Cross_Reference
        (Item, Referrer, File_Name, Line, Column, Ref_Type);
   end Add_Cross_Reference;

   --------------------------
   -- Add_Entity_Reference --
   --------------------------

   overriding procedure Add_Entity
     (UI     : in out Root_Komnenos_UI;
      Key    : String;
      Entity : Komnenos.Entities.Entity_Reference)
   is
   begin
      UI.Entities.Add_Entity (Key, Entity);
   end Add_Entity;

   ---------------
   -- Create_UI --
   ---------------

   function Create_UI
     (Config_Folder_Path : String := Komnenos.Paths.Config_Path)
      return Komnenos_UI
   is
   begin
      Local_Current_UI := Komnenos.UI.Gtk_UI.Create_UI (Config_Folder_Path);
      return Local_Current_UI;
   end Create_UI;

   ----------------------
   -- Cross_References --
   ----------------------

   overriding function Cross_References
     (UI           : Root_Komnenos_UI;
      File_Name    : String;
      Line         : Line_Number;
      Column       : Column_Number;
      Enabled      : String := "")
      return Komnenos.Entities.Array_Of_Entities
   is
   begin
      return UI.Entities.Cross_References
        (File_Name, Line, Column, Enabled);
   end Cross_References;

   ----------------
   -- Current_UI --
   ----------------

   function Current_UI return Komnenos_UI is
   begin
      return Local_Current_UI;
   end Current_UI;

   ------------
   -- Exists --
   ------------

   overriding function Exists
     (UI  : Root_Komnenos_UI;
      Key : String)
      return Boolean
   is
   begin
      return UI.Entities.Exists (Key);
   end Exists;

   ----------
   -- Find --
   ----------

   overriding function Find
     (UI         : Root_Komnenos_UI;
      Name       : String;
      Class_Name : String)
      return Komnenos.Entities.Entity_Reference
   is
   begin
      return UI.Entities.Find (Name, Class_Name);
   end Find;

   -----------------
   -- From_Config --
   -----------------

   overriding procedure From_Config
     (UI : not null access Root_Komnenos_UI;
      Config : Tropos.Configuration)
   is
      function Get (Name : String) return Pixel_Position
      is (Pixel_Position (Integer'(Config.Get (Name))));

   begin
      UI.View_Left   := Get ("view_left");
      UI.View_Top    := Get ("view_top");
      UI.View_Width  := Get ("view_width");
      UI.View_Height := Get ("view_height");
   end From_Config;

   ---------
   -- Get --
   ---------

   overriding function Get
     (UI  : Root_Komnenos_UI;
      Key : String)
      return Komnenos.Entities.Entity_Reference
   is
   begin
      return UI.Entities.Get (Key);
   end Get;

   -------------
   -- Iterate --
   -------------

   overriding procedure Iterate
     (UI             : Root_Komnenos_UI;
      Filter         : in String;
      Process        : not null access
        procedure (Item : Komnenos.Entities.Entity_Reference);
      Top_Level_Only : Boolean := True)
   is
   begin
      UI.Entities.Iterate (Filter, Process, Top_Level_Only);
   end Iterate;

   ----------------------
   -- Load_Style_Sheet --
   ----------------------

   procedure Load_Style_Sheet
     (UI   : in out Root_Komnenos_UI'Class;
      Path : String)
   is
      pragma Unreferenced (UI);
   begin
      Css.Parser.Load_Css_File (Path);
   end Load_Style_Sheet;

   -------------------
   -- Program_Store --
   -------------------

   overriding function Program_Store
     (UI             : Root_Komnenos_UI)
      return access Komnenos.Entities.Program_Store_Interface'Class
   is
   begin
      return UI.Store;
   end Program_Store;

   -------------------------
   -- Reference_File_Name --
   -------------------------

   overriding function Reference_File_Name
     (UI        : Root_Komnenos_UI;
      Reference : Komnenos.Entities.Reference_Record)
      return String
   is
   begin
      return UI.Entities.Reference_File_Name (Reference);
   end Reference_File_Name;

   ----------------
   -- References --
   ----------------

   overriding function References
     (UI     : Root_Komnenos_UI;
      Entity : Komnenos.Entities.Entity_Reference)
      return Komnenos.Entities.Reference_Record_Array
   is
   begin
      return UI.Entities.References (Entity);
   end References;

   -----------------------
   -- Set_Program_Store --
   -----------------------

   overriding procedure Set_Program_Store
     (UI    : in out Root_Komnenos_UI;
      Store : access Komnenos.Entities.Program_Store_Interface'Class)
   is
   begin
      UI.Store := Store;
   end Set_Program_Store;

   ----------
   -- Sort --
   ----------

   overriding procedure Sort
     (UI     : in out Root_Komnenos_UI)
   is
   begin
      UI.Entities.Sort;
   end Sort;

   ---------------
   -- To_Config --
   ---------------

   overriding procedure To_Config
     (UI     : Root_Komnenos_UI;
      Config : in out Tropos.Configuration)
   is
   begin
      Config.Add ("view_left", Integer (UI.View_Left));
      Config.Add ("view_top", Integer (UI.View_Top));
      Config.Add ("view_width", Integer (UI.View_Width));
      Config.Add ("view_height", Integer (UI.View_Height));
   end To_Config;

end Komnenos.UI;
