with Tropos.Reader;

with Komnenos.Entities.Text_Entities;

with Komnenos.Themes;

with Komnenos.UI;

with Komnenos.Paths;

procedure Komnenos.Demo_Driver is
   UI : constant Komnenos.UI.Komnenos_UI :=
          Komnenos.UI.Create_UI
            (Komnenos.Paths.Config_Path);
begin
   Komnenos.Themes.Load_Theme
     (Tropos.Reader.Read_Config
        (Komnenos.Paths.Config_File ("themes/default.theme")));

   declare
      Welcome : constant Komnenos.Entities.Entity_Reference :=
                  Komnenos.Entities.Text_Entities.Create_Text_Entity
                    ("Welcome", "/komnenos/entities/welcome",
                     Contents => "Welcome to Komnenos");
      Child : constant Komnenos.Entities.Entity_Reference :=
                  Komnenos.Entities.Text_Entities.Create_Text_Entity
                    ("Linked child", "/komnenos/entities/linked-child",
                     Contents => "Linked child");
   begin
      Welcome.Select_Entity (UI, null, null, 0);
      Child.Select_Entity
        (Table  => UI,
         Parent => UI.Get_Visual ("Welcome"),
         Visual => null,
         Offset => 32);
   end;

   UI.Start;
end Komnenos.Demo_Driver;
