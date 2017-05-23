package body Komnenos.Keys is

   -------------------
   -- Character_Key --
   -------------------

   function Character_Key (Ch    : Character) return Komnenos_Key is
   begin
      return Character'Pos (Ch);
   end Character_Key;

   -------------
   -- Control --
   -------------

   function Control (Key : Komnenos_Key) return Boolean is
   begin
      return (Key and Control_Mask) = Control_Mask;
   end Control;

   -------------------
   -- Get_Character --
   -------------------

   function Get_Character (Key : Komnenos_Key) return Character is
   begin
      return Character'Val (Key and Key_Mask);
   end Get_Character;

   ------------------
   -- Is_Character --
   ------------------

   function Is_Character (Key  : Komnenos_Key) return Boolean is
   begin
      return (Key and not Shift_Mask) in 32 .. 126;
   end Is_Character;

   -----------------
   -- Is_Function --
   -----------------

   function Is_Function  (Key : Komnenos_Key) return Boolean is
   begin
      return Key in Function_Keys;
   end Is_Function;

   ----------
   -- Mod1 --
   ----------

   function Mod1 (Key : Komnenos_Key) return Boolean is
   begin
      return (Key and Mod1_Mask) = Mod1_Mask;
   end Mod1;

   ----------
   -- Mod2 --
   ----------

   function Mod2 (Key : Komnenos_Key) return Boolean is
   begin
      return (Key and Mod2_Mask) = Mod2_Mask;
   end Mod2;

   ------------
   -- Modify --
   ------------

   function Modify (Key     : Komnenos_Key;
                    Shift   : Boolean       := False;
                    Control : Boolean       := False;
                    Mod1    : Boolean       := False;
                    Mod2    : Boolean       := False)
                   return Komnenos_Key
   is
      Result : Komnenos_Key := Key;

      procedure Modify (Value : in out Komnenos_Key;
                        Mask  : in     Komnenos_Key;
                        Set   : in     Boolean);

      ------------
      -- Modify --
      ------------

      procedure Modify (Value : in out Komnenos_Key;
                        Mask  : in     Komnenos_Key;
                        Set   : in     Boolean)
      is
      begin
         if Set then
            Value := Value or Mask;
         else
            Value := Value and not Mask;
         end if;
      end Modify;

   begin
      Modify (Result, Shift_Mask, Shift);
      Modify (Result, Control_Mask, Control);
      Modify (Result, Mod1_Mask, Mod1);
      Modify (Result, Mod2_Mask, Mod2);
      return Result;
   end Modify;

   -------------
   -- Raw_Key --
   -------------

   function Raw_Key (Key : Komnenos_Key) return Komnenos_Key is
   begin
      return Key and Key_Mask;
   end Raw_Key;

   -----------
   -- Shift --
   -----------

   function Shift   (Key : Komnenos_Key) return Boolean is
   begin
      return (Key and Shift_Mask) = Shift_Mask;
   end Shift;

end Komnenos.Keys;
