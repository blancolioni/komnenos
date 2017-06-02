package Komnenos.Keys is

   type Modifier_Keys is private;

   function Shift   (Modifier : Modifier_Keys) return Boolean;
   function Control (Modifier : Modifier_Keys) return Boolean;
   function Alt     (Modifier : Modifier_Keys) return Boolean;
   function Meta    (Modifier : Modifier_Keys) return Boolean;

   function Modifier
     (Shift   : Boolean       := False;
      Control : Boolean       := False;
      Alt     : Boolean       := False;
      Meta    : Boolean       := False)
      return Modifier_Keys;

   type Komnenos_Key is private;

   function Modify (Key      : Komnenos_Key;
                    Modifier : Modifier_Keys)
                    return Komnenos_Key;

   function Modify (Key     : Komnenos_Key;
                    Shift   : Boolean       := False;
                    Control : Boolean       := False;
                    Alt     : Boolean       := False;
                    Meta    : Boolean       := False)
                    return Komnenos_Key
   is (Modify (Key, Modifier (Shift, Control, Alt, Meta)));

   function Is_Character (Key  : Komnenos_Key) return Boolean;
   --  Return True if Key represents a self-inserting character

   function Is_Function  (Key : Komnenos_Key) return Boolean;
   --  return True if Key represents a function key

   function Character_Key (Ch : Character) return Komnenos_Key;
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

   type Modifier_Key is (Shift, Control, Alt, Meta);

   type Modifier_Keys is array (Modifier_Key) of Boolean;

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

   Modifier_Masks : constant array (Modifier_Key) of Komnenos_Key :=
                      (Shift   => 16#1_0000#,
                       Control => 16#2_0000#,
                       Alt     => 16#4_0000#,
                       Meta    => 16#8_0000#);

   function Modifier
     (Shift   : Boolean       := False;
      Control : Boolean       := False;
      Alt     : Boolean       := False;
      Meta    : Boolean       := False)
      return Modifier_Keys
   is (Shift, Control, Alt, Meta);

   function Shift   (Modifier : Modifier_Keys) return Boolean
   is (Modifier (Shift));

   function Control (Modifier : Modifier_Keys) return Boolean
   is (Modifier (Control));

   function Alt     (Modifier : Modifier_Keys) return Boolean
   is (Modifier (Alt));

   function Meta    (Modifier : Modifier_Keys) return Boolean
   is (Modifier (Meta));

   function Is_Character (Key  : Komnenos_Key) return Boolean
   is ((Key and not Modifier_Masks (Shift)) in 32 .. 126);

   function Character_Key (Ch : Character) return Komnenos_Key
   is (Character'Pos (Ch));

   function Get_Character (Key : Komnenos_Key) return Character
   is (Character'Val (Key and Key_Mask));

   function Raw_Key (Key : Komnenos_Key) return Komnenos_Key
   is (Key and Key_Mask);

   function Is_Function  (Key : Komnenos_Key) return Boolean
   is (Key in Function_Keys);

end Komnenos.Keys;
