package Komnenos.Keys is

   type Komnenos_Key is private;

   function Shift   (Key : Komnenos_Key) return Boolean;
   function Control (Key : Komnenos_Key) return Boolean;
   function Mod1    (Key : Komnenos_Key) return Boolean;
   function Mod2    (Key : Komnenos_Key) return Boolean;

   function Modify (Key     : Komnenos_Key;
                    Shift   : Boolean       := False;
                    Control : Boolean       := False;
                    Mod1    : Boolean       := False;
                    Mod2    : Boolean       := False)
                   return Komnenos_Key;

   function Is_Character (Key  : Komnenos_Key) return Boolean;
   --  Return True if Key represents a self-inserting character

   function Is_Function  (Key : Komnenos_Key) return Boolean;
   --  return True if Key represents a function key

   function Character_Key (Ch    : Character) return Komnenos_Key;
   function Get_Character (Key : Komnenos_Key) return Character;

   function Raw_Key (Key : Komnenos_Key) return Komnenos_Key;

   Null_Key        : constant Komnenos_Key;
   Tab             : constant Komnenos_Key;
   Left_Tab        : constant Komnenos_Key;
   Up_Arrow        : constant Komnenos_Key;
   Down_Arrow      : constant Komnenos_Key;
   Left_Arrow      : constant Komnenos_Key;
   Right_Arrow     : constant Komnenos_Key;
   Page_Up         : constant Komnenos_Key;
   Page_Down       : constant Komnenos_Key;
   Home_Key        : constant Komnenos_Key;
   End_Key         : constant Komnenos_Key;
   Back_Space      : constant Komnenos_Key;
   Line_Feed       : constant Komnenos_Key;
   Carriage_Return : constant Komnenos_Key;

private

   type Komnenos_Key is mod 16#10_0000#;
   subtype Real_Key is Komnenos_Key range 1 .. 16#FFFF#;

   Null_Key    : constant Komnenos_Key := 16#0000#;

   Tab             : constant Komnenos_Key := 16#F000#;
   Left_Tab        : constant Komnenos_Key := 16#F001#;
   Up_Arrow        : constant Komnenos_Key := 16#F002#;
   Down_Arrow      : constant Komnenos_Key := 16#F003#;
   Left_Arrow      : constant Komnenos_Key := 16#F004#;
   Right_Arrow     : constant Komnenos_Key := 16#F005#;
   Page_Up         : constant Komnenos_Key := 16#F006#;
   Page_Down       : constant Komnenos_Key := 16#F007#;
   Home_Key        : constant Komnenos_Key := 16#F00B#;
   End_Key         : constant Komnenos_Key := 16#F009#;
   Back_Space      : constant Komnenos_Key := 16#F008#;
   Line_Feed       : constant Komnenos_Key := 16#F00A#;
   Carriage_Return : constant Komnenos_Key := 16#F00D#;

   subtype Function_Keys is Komnenos_Key range 16#F100# .. 16#F1FF#;

   Key_Mask     : constant Komnenos_Key := 16#FFFF#;
   Shift_Mask   : constant Komnenos_Key := 16#1_0000#;
   Control_Mask : constant Komnenos_Key := 16#2_0000#;
   Mod1_Mask    : constant Komnenos_Key := 16#4_0000#;
   Mod2_Mask    : constant Komnenos_Key := 16#8_0000#;

end Komnenos.Keys;
