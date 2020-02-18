package Komnenos.UI.Sessions is

   procedure Load_Session
     (UI   : Komnenos_UI;
      Path : String);

   procedure Save_Session
     (UI   : Komnenos_UI;
      Path : String);

   procedure Create_Session
     (UI                 : Komnenos_UI;
      Store              : not null access
        Komnenos.Entities.Program_Store_Interface'Class);

end Komnenos.UI.Sessions;
