private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

with Gtk.Widget;

private with Glib;
--  private with Gtk.Scrolled_Window;
private with Gtk.Text_Buffer;
private with Gtk.Text_View;
private with Komnenos.Colours;
private with Komnenos.Styles;

with Komnenos.Fragments;

package Komnenos.UI.Gtk_UI.Text is

   type Komnenos_Text_View_Record is
     new Gtk.Widget.Gtk_Widget_Record
     and Komnenos.Fragments.Text_Editor_Display
   with private;

   type Komnenos_Text_View is access all Komnenos_Text_View_Record'Class;

   function Create_Text_View
     (Fragment : Komnenos.Fragments.Text_Fragment)
      return Komnenos_Text_View;

private

   type Highlight_Line is
      record
         Related_Entity     : Komnenos.Entities.Entity_Reference;
         Line_Start_Pixels  : Glib.Gint;
         Line_Height_Pixels : Glib.Gint;
         Highlight_Colour   : Komnenos.Colours.Komnenos_Colour;
      end record;

   package List_Of_Line_Highlights is
     new Ada.Containers.Doubly_Linked_Lists (Highlight_Line);

   type Komnenos_Text_View_Record is
     new Gtk.Text_View.Gtk_Text_View_Record
     and Komnenos.Fragments.Text_Editor_Display with
      record
         Text                   : Gtk.Text_View.Gtk_Text_View;
         Buffer                 : Gtk.Text_Buffer.Gtk_Text_Buffer;
         Hover_Start            : Natural  := 0;
         Hover_Finish           : Natural := 0;
         Hover_Style            : Komnenos.Styles.Komnenos_Style;
         Fragment               : Komnenos.Fragments.Text_Fragment;
         Highlights             : List_Of_Line_Highlights.List;
         Current_Line_Highlight : Komnenos.Colours.Komnenos_Colour;
         Initialising           : Boolean := True;
         Updating_Cursor        : Boolean := False;
         Updating_Text          : Boolean := False;
         Tool_Tip               : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding procedure Insert_At_Cursor
     (Text_View : in out Komnenos_Text_View_Record;
      Text      : in     String);

   overriding procedure Delete_From_Cursor
     (Text_View : in out Komnenos_Text_View_Record;
      Movement  : Text_Movement);

   overriding procedure Set_Cursor
     (Text_View    : in out Komnenos_Text_View_Record;
      New_Position : Text_Position);

   overriding procedure Set_Content
     (Text_View    : in out Komnenos_Text_View_Record;
      New_Content  : in String);

   overriding procedure Render_Fragment
     (Text_View    : in out Komnenos_Text_View_Record;
      Fragment     : not null access Fragments.Root_Fragment_Type'Class);

end Komnenos.UI.Gtk_UI.Text;
