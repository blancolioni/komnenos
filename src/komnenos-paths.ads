package Komnenos.Paths is

   Config_Path : constant String :=
     "C:/Users/fwilson/alix\config\komnenos";

   function Config_File
     (File_Path : String)
     return String
   is (Config_Path & "/" & File_Path);

end Komnenos.Paths;
