package body Komnenos.Fragments.Notes is

   -----------------------
   -- New_Note_Fragment --
   -----------------------

   function New_Note_Fragment
     (Entity : Komnenos.Entities.Entity_Reference)
      return Fragment_Type
   is
      Result : constant Fragment_Type := new Root_Fragment_Type;
   begin
      Result.Editable := True;
      Result.Background_Colour := Komnenos.Colours.From_String ("seashell");
      Result.Foreground_Colour := Komnenos.Colours.From_String ("black");
      Result.Border_Colour     := Komnenos.Colours.From_String ("#660066");
      Result.Set_Content (Entity);
      Result.Set_Position (100, 100);
      Result.Set_Entity_Key (Entity.Name);
      return Result;
   end New_Note_Fragment;

end Komnenos.Fragments.Notes;
