{-# OPTIONS_GHC -w #-}
module Parser (parseProg, parseType, parseExpr) where
import Data.Char (isDigit, isSpace, isAlpha)
import Prelude hiding (LT, GT, EQ)
import Declare
import Tokens
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn6 (Program)
	| HappyAbsSyn7 ([(String, Function)])
	| HappyAbsSyn8 ((String, Function))
	| HappyAbsSyn9 ([(String, Type)])
	| HappyAbsSyn10 ([(Label, Exp)])
	| HappyAbsSyn11 (Type)
	| HappyAbsSyn12 (Exp)
	| HappyAbsSyn15 ([Exp])

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104 :: () => Prelude.Int -> ({-HappyReduction (Either String) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Either String) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Either String) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Either String) HappyAbsSyn)

happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46 :: () => ({-HappyReduction (Either String) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Either String) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Either String) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Either String) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,246) ([0,0,0,0,24624,0,0,37432,1665,2,0,0,0,9088,26649,288,0,0,0,0,15,2552,0,32768,4096,0,1,0,0,1024,0,0,0,0,0,18624,6150,8,9088,26649,32,1024,0,0,14336,33170,518,0,64,0,0,0,0,0,0,0,0,37424,1537,2,0,0,0,0,0,0,0,0,0,4096,0,0,0,1539,0,0,0,0,0,32768,0,0,16384,16,0,0,32,0,0,0,0,36352,41060,129,0,0,0,0,18432,0,0,1024,1,0,0,16,0,8192,0,0,0,0,0,9088,26649,32,0,4096,0,4096,0,0,49152,1600,24,0,6435,8288,0,25740,33152,0,37424,1537,2,18624,6150,8,8960,24601,32,35840,32868,129,12288,402,518,49152,1608,2072,0,6435,8288,0,25740,33152,0,37424,1537,2,0,0,0,0,0,0,1024,0,0,0,16,0,0,60,2016,0,240,3968,0,960,7680,0,3840,0,0,15360,0,0,61440,0,0,49152,3,0,0,0,0,0,0,0,0,192,0,0,768,0,0,0,0,0,0,0,0,9088,26649,32,0,1032,0,0,0,0,57344,1608,2074,0,0,0,0,4,0,0,0,0,0,18656,6662,8,0,2,0,12288,96,0,0,0,0,16384,0,0,0,0,0,0,0,2,0,0,0,0,18656,6662,8,0,0,0,0,4096,0,0,0,0,0,0,0,0,16384,0,0,4,0,0,8192,16,0,18656,6662,8,9088,26649,32,0,256,0,49152,384,0,0,0,0,0,0,16,0,0,0,0,0,0,0,0,2,0,9088,26649,32,36352,41060,129,0,0,0,0,256,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parserProg","%start_parserType","%start_parserExpr","Program","Functions","Function","ids","Fields","typ","Exp","Oper","App","Elements","var","id","int","Int","Bool","'+'","'-'","'*'","'/'","'('","')'","'}'","'{'","'['","']'","';'","':'","','","'.'","'='","if","else","true","false","'<'","'<='","'>'","'>='","'=='","'&&'","'!'","'!!'","'||'","fun","%eof"]
        bit_start = st Prelude.* 50
        bit_end = (st Prelude.+ 1) Prelude.* 50
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..49]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (6) = happyGoto action_24
action_0 (7) = happyGoto action_4
action_0 _ = happyReduce_5

action_1 (19) = happyShift action_20
action_1 (20) = happyShift action_21
action_1 (28) = happyShift action_22
action_1 (29) = happyShift action_23
action_1 (11) = happyGoto action_19
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (16) = happyShift action_8
action_2 (17) = happyShift action_9
action_2 (18) = happyShift action_10
action_2 (22) = happyShift action_11
action_2 (25) = happyShift action_12
action_2 (28) = happyShift action_13
action_2 (29) = happyShift action_14
action_2 (36) = happyShift action_15
action_2 (38) = happyShift action_16
action_2 (39) = happyShift action_17
action_2 (46) = happyShift action_18
action_2 (12) = happyGoto action_5
action_2 (13) = happyGoto action_6
action_2 (14) = happyGoto action_7
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (7) = happyGoto action_4
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (16) = happyShift action_8
action_4 (17) = happyShift action_9
action_4 (18) = happyShift action_10
action_4 (22) = happyShift action_11
action_4 (25) = happyShift action_12
action_4 (28) = happyShift action_13
action_4 (29) = happyShift action_14
action_4 (36) = happyShift action_15
action_4 (38) = happyShift action_16
action_4 (39) = happyShift action_17
action_4 (46) = happyShift action_18
action_4 (49) = happyShift action_53
action_4 (8) = happyGoto action_51
action_4 (12) = happyGoto action_52
action_4 (13) = happyGoto action_6
action_4 (14) = happyGoto action_7
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (50) = happyAccept
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (21) = happyShift action_40
action_6 (22) = happyShift action_41
action_6 (23) = happyShift action_42
action_6 (24) = happyShift action_43
action_6 (40) = happyShift action_44
action_6 (41) = happyShift action_45
action_6 (42) = happyShift action_46
action_6 (43) = happyShift action_47
action_6 (44) = happyShift action_48
action_6 (45) = happyShift action_49
action_6 (48) = happyShift action_50
action_6 _ = happyReduce_19

action_7 (34) = happyShift action_38
action_7 (47) = happyShift action_39
action_7 _ = happyReduce_33

action_8 (17) = happyShift action_37
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (25) = happyShift action_36
action_9 _ = happyReduce_39

action_10 _ = happyReduce_40

action_11 (17) = happyShift action_9
action_11 (18) = happyShift action_10
action_11 (22) = happyShift action_11
action_11 (25) = happyShift action_12
action_11 (28) = happyShift action_13
action_11 (29) = happyShift action_14
action_11 (38) = happyShift action_16
action_11 (39) = happyShift action_17
action_11 (46) = happyShift action_18
action_11 (13) = happyGoto action_35
action_11 (14) = happyGoto action_7
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (16) = happyShift action_8
action_12 (17) = happyShift action_9
action_12 (18) = happyShift action_10
action_12 (22) = happyShift action_11
action_12 (25) = happyShift action_12
action_12 (28) = happyShift action_13
action_12 (29) = happyShift action_14
action_12 (36) = happyShift action_15
action_12 (38) = happyShift action_16
action_12 (39) = happyShift action_17
action_12 (46) = happyShift action_18
action_12 (12) = happyGoto action_34
action_12 (13) = happyGoto action_6
action_12 (14) = happyGoto action_7
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (17) = happyShift action_33
action_13 (10) = happyGoto action_32
action_13 _ = happyReduce_12

action_14 (16) = happyShift action_8
action_14 (17) = happyShift action_9
action_14 (18) = happyShift action_10
action_14 (22) = happyShift action_11
action_14 (25) = happyShift action_12
action_14 (28) = happyShift action_13
action_14 (29) = happyShift action_14
action_14 (36) = happyShift action_15
action_14 (38) = happyShift action_16
action_14 (39) = happyShift action_17
action_14 (46) = happyShift action_18
action_14 (12) = happyGoto action_30
action_14 (13) = happyGoto action_6
action_14 (14) = happyGoto action_7
action_14 (15) = happyGoto action_31
action_14 _ = happyReduce_46

action_15 (25) = happyShift action_29
action_15 _ = happyFail (happyExpListPerState 15)

action_16 _ = happyReduce_41

action_17 _ = happyReduce_42

action_18 (17) = happyShift action_9
action_18 (18) = happyShift action_10
action_18 (22) = happyShift action_11
action_18 (25) = happyShift action_12
action_18 (28) = happyShift action_13
action_18 (29) = happyShift action_14
action_18 (38) = happyShift action_16
action_18 (39) = happyShift action_17
action_18 (46) = happyShift action_18
action_18 (13) = happyGoto action_28
action_18 (14) = happyGoto action_7
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (50) = happyAccept
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_13

action_21 _ = happyReduce_14

action_22 (17) = happyShift action_27
action_22 (9) = happyGoto action_26
action_22 _ = happyReduce_9

action_23 (19) = happyShift action_20
action_23 (20) = happyShift action_21
action_23 (28) = happyShift action_22
action_23 (29) = happyShift action_23
action_23 (11) = happyGoto action_25
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (50) = happyAccept
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (30) = happyShift action_80
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (27) = happyShift action_78
action_26 (33) = happyShift action_79
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (32) = happyShift action_77
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_32

action_29 (16) = happyShift action_8
action_29 (17) = happyShift action_9
action_29 (18) = happyShift action_10
action_29 (22) = happyShift action_11
action_29 (25) = happyShift action_12
action_29 (28) = happyShift action_13
action_29 (29) = happyShift action_14
action_29 (36) = happyShift action_15
action_29 (38) = happyShift action_16
action_29 (39) = happyShift action_17
action_29 (46) = happyShift action_18
action_29 (12) = happyGoto action_76
action_29 (13) = happyGoto action_6
action_29 (14) = happyGoto action_7
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_45

action_31 (30) = happyShift action_74
action_31 (33) = happyShift action_75
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (27) = happyShift action_72
action_32 (33) = happyShift action_73
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (35) = happyShift action_71
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (26) = happyShift action_70
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_31

action_36 (16) = happyShift action_8
action_36 (17) = happyShift action_9
action_36 (18) = happyShift action_10
action_36 (22) = happyShift action_11
action_36 (25) = happyShift action_12
action_36 (28) = happyShift action_13
action_36 (29) = happyShift action_14
action_36 (36) = happyShift action_15
action_36 (38) = happyShift action_16
action_36 (39) = happyShift action_17
action_36 (46) = happyShift action_18
action_36 (12) = happyGoto action_30
action_36 (13) = happyGoto action_6
action_36 (14) = happyGoto action_7
action_36 (15) = happyGoto action_69
action_36 _ = happyReduce_46

action_37 (35) = happyShift action_68
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (17) = happyShift action_67
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (17) = happyShift action_9
action_39 (18) = happyShift action_10
action_39 (25) = happyShift action_12
action_39 (28) = happyShift action_13
action_39 (29) = happyShift action_14
action_39 (38) = happyShift action_16
action_39 (39) = happyShift action_17
action_39 (14) = happyGoto action_66
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (17) = happyShift action_9
action_40 (18) = happyShift action_10
action_40 (22) = happyShift action_11
action_40 (25) = happyShift action_12
action_40 (28) = happyShift action_13
action_40 (29) = happyShift action_14
action_40 (38) = happyShift action_16
action_40 (39) = happyShift action_17
action_40 (46) = happyShift action_18
action_40 (13) = happyGoto action_65
action_40 (14) = happyGoto action_7
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (17) = happyShift action_9
action_41 (18) = happyShift action_10
action_41 (22) = happyShift action_11
action_41 (25) = happyShift action_12
action_41 (28) = happyShift action_13
action_41 (29) = happyShift action_14
action_41 (38) = happyShift action_16
action_41 (39) = happyShift action_17
action_41 (46) = happyShift action_18
action_41 (13) = happyGoto action_64
action_41 (14) = happyGoto action_7
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (17) = happyShift action_9
action_42 (18) = happyShift action_10
action_42 (22) = happyShift action_11
action_42 (25) = happyShift action_12
action_42 (28) = happyShift action_13
action_42 (29) = happyShift action_14
action_42 (38) = happyShift action_16
action_42 (39) = happyShift action_17
action_42 (46) = happyShift action_18
action_42 (13) = happyGoto action_63
action_42 (14) = happyGoto action_7
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (17) = happyShift action_9
action_43 (18) = happyShift action_10
action_43 (22) = happyShift action_11
action_43 (25) = happyShift action_12
action_43 (28) = happyShift action_13
action_43 (29) = happyShift action_14
action_43 (38) = happyShift action_16
action_43 (39) = happyShift action_17
action_43 (46) = happyShift action_18
action_43 (13) = happyGoto action_62
action_43 (14) = happyGoto action_7
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (17) = happyShift action_9
action_44 (18) = happyShift action_10
action_44 (22) = happyShift action_11
action_44 (25) = happyShift action_12
action_44 (28) = happyShift action_13
action_44 (29) = happyShift action_14
action_44 (38) = happyShift action_16
action_44 (39) = happyShift action_17
action_44 (46) = happyShift action_18
action_44 (13) = happyGoto action_61
action_44 (14) = happyGoto action_7
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (17) = happyShift action_9
action_45 (18) = happyShift action_10
action_45 (22) = happyShift action_11
action_45 (25) = happyShift action_12
action_45 (28) = happyShift action_13
action_45 (29) = happyShift action_14
action_45 (38) = happyShift action_16
action_45 (39) = happyShift action_17
action_45 (46) = happyShift action_18
action_45 (13) = happyGoto action_60
action_45 (14) = happyGoto action_7
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (17) = happyShift action_9
action_46 (18) = happyShift action_10
action_46 (22) = happyShift action_11
action_46 (25) = happyShift action_12
action_46 (28) = happyShift action_13
action_46 (29) = happyShift action_14
action_46 (38) = happyShift action_16
action_46 (39) = happyShift action_17
action_46 (46) = happyShift action_18
action_46 (13) = happyGoto action_59
action_46 (14) = happyGoto action_7
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (17) = happyShift action_9
action_47 (18) = happyShift action_10
action_47 (22) = happyShift action_11
action_47 (25) = happyShift action_12
action_47 (28) = happyShift action_13
action_47 (29) = happyShift action_14
action_47 (38) = happyShift action_16
action_47 (39) = happyShift action_17
action_47 (46) = happyShift action_18
action_47 (13) = happyGoto action_58
action_47 (14) = happyGoto action_7
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (17) = happyShift action_9
action_48 (18) = happyShift action_10
action_48 (22) = happyShift action_11
action_48 (25) = happyShift action_12
action_48 (28) = happyShift action_13
action_48 (29) = happyShift action_14
action_48 (38) = happyShift action_16
action_48 (39) = happyShift action_17
action_48 (46) = happyShift action_18
action_48 (13) = happyGoto action_57
action_48 (14) = happyGoto action_7
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (17) = happyShift action_9
action_49 (18) = happyShift action_10
action_49 (22) = happyShift action_11
action_49 (25) = happyShift action_12
action_49 (28) = happyShift action_13
action_49 (29) = happyShift action_14
action_49 (38) = happyShift action_16
action_49 (39) = happyShift action_17
action_49 (46) = happyShift action_18
action_49 (13) = happyGoto action_56
action_49 (14) = happyGoto action_7
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (17) = happyShift action_9
action_50 (18) = happyShift action_10
action_50 (22) = happyShift action_11
action_50 (25) = happyShift action_12
action_50 (28) = happyShift action_13
action_50 (29) = happyShift action_14
action_50 (38) = happyShift action_16
action_50 (39) = happyShift action_17
action_50 (46) = happyShift action_18
action_50 (13) = happyGoto action_55
action_50 (14) = happyGoto action_7
action_50 _ = happyFail (happyExpListPerState 50)

action_51 _ = happyReduce_4

action_52 _ = happyReduce_3

action_53 (17) = happyShift action_54
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (25) = happyShift action_89
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (21) = happyShift action_40
action_55 (22) = happyShift action_41
action_55 (23) = happyShift action_42
action_55 (24) = happyShift action_43
action_55 (40) = happyShift action_44
action_55 (41) = happyShift action_45
action_55 (42) = happyShift action_46
action_55 (43) = happyShift action_47
action_55 (44) = happyShift action_48
action_55 (45) = happyShift action_49
action_55 _ = happyReduce_20

action_56 (21) = happyShift action_40
action_56 (22) = happyShift action_41
action_56 (23) = happyShift action_42
action_56 (24) = happyShift action_43
action_56 (40) = happyShift action_44
action_56 (41) = happyShift action_45
action_56 (42) = happyShift action_46
action_56 (43) = happyShift action_47
action_56 (44) = happyShift action_48
action_56 _ = happyReduce_21

action_57 (21) = happyShift action_40
action_57 (22) = happyShift action_41
action_57 (23) = happyShift action_42
action_57 (24) = happyShift action_43
action_57 (40) = happyShift action_44
action_57 (41) = happyShift action_45
action_57 (42) = happyShift action_46
action_57 (43) = happyShift action_47
action_57 (44) = happyFail []
action_57 _ = happyReduce_22

action_58 (21) = happyShift action_40
action_58 (22) = happyShift action_41
action_58 (23) = happyShift action_42
action_58 (24) = happyShift action_43
action_58 (40) = happyFail []
action_58 (41) = happyFail []
action_58 (42) = happyFail []
action_58 (43) = happyFail []
action_58 _ = happyReduce_26

action_59 (21) = happyShift action_40
action_59 (22) = happyShift action_41
action_59 (23) = happyShift action_42
action_59 (24) = happyShift action_43
action_59 (40) = happyFail []
action_59 (41) = happyFail []
action_59 (42) = happyFail []
action_59 (43) = happyFail []
action_59 _ = happyReduce_24

action_60 (21) = happyShift action_40
action_60 (22) = happyShift action_41
action_60 (23) = happyShift action_42
action_60 (24) = happyShift action_43
action_60 (40) = happyFail []
action_60 (41) = happyFail []
action_60 (42) = happyFail []
action_60 (43) = happyFail []
action_60 _ = happyReduce_25

action_61 (21) = happyShift action_40
action_61 (22) = happyShift action_41
action_61 (23) = happyShift action_42
action_61 (24) = happyShift action_43
action_61 (40) = happyFail []
action_61 (41) = happyFail []
action_61 (42) = happyFail []
action_61 (43) = happyFail []
action_61 _ = happyReduce_23

action_62 _ = happyReduce_30

action_63 _ = happyReduce_29

action_64 (23) = happyShift action_42
action_64 (24) = happyShift action_43
action_64 _ = happyReduce_28

action_65 (23) = happyShift action_42
action_65 (24) = happyShift action_43
action_65 _ = happyReduce_27

action_66 _ = happyReduce_38

action_67 _ = happyReduce_36

action_68 (16) = happyShift action_8
action_68 (17) = happyShift action_9
action_68 (18) = happyShift action_10
action_68 (22) = happyShift action_11
action_68 (25) = happyShift action_12
action_68 (28) = happyShift action_13
action_68 (29) = happyShift action_14
action_68 (36) = happyShift action_15
action_68 (38) = happyShift action_16
action_68 (39) = happyShift action_17
action_68 (46) = happyShift action_18
action_68 (12) = happyGoto action_88
action_68 (13) = happyGoto action_6
action_68 (14) = happyGoto action_7
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (26) = happyShift action_87
action_69 (33) = happyShift action_75
action_69 _ = happyFail (happyExpListPerState 69)

action_70 _ = happyReduce_43

action_71 (16) = happyShift action_8
action_71 (17) = happyShift action_9
action_71 (18) = happyShift action_10
action_71 (22) = happyShift action_11
action_71 (25) = happyShift action_12
action_71 (28) = happyShift action_13
action_71 (29) = happyShift action_14
action_71 (36) = happyShift action_15
action_71 (38) = happyShift action_16
action_71 (39) = happyShift action_17
action_71 (46) = happyShift action_18
action_71 (12) = happyGoto action_86
action_71 (13) = happyGoto action_6
action_71 (14) = happyGoto action_7
action_71 _ = happyFail (happyExpListPerState 71)

action_72 _ = happyReduce_35

action_73 (17) = happyShift action_85
action_73 _ = happyFail (happyExpListPerState 73)

action_74 _ = happyReduce_37

action_75 (16) = happyShift action_8
action_75 (17) = happyShift action_9
action_75 (18) = happyShift action_10
action_75 (22) = happyShift action_11
action_75 (25) = happyShift action_12
action_75 (28) = happyShift action_13
action_75 (29) = happyShift action_14
action_75 (36) = happyShift action_15
action_75 (38) = happyShift action_16
action_75 (39) = happyShift action_17
action_75 (46) = happyShift action_18
action_75 (12) = happyGoto action_84
action_75 (13) = happyGoto action_6
action_75 (14) = happyGoto action_7
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (26) = happyShift action_83
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (19) = happyShift action_20
action_77 (20) = happyShift action_21
action_77 (28) = happyShift action_22
action_77 (29) = happyShift action_23
action_77 (11) = happyGoto action_82
action_77 _ = happyFail (happyExpListPerState 77)

action_78 _ = happyReduce_16

action_79 (17) = happyShift action_81
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_15

action_81 (32) = happyShift action_94
action_81 _ = happyFail (happyExpListPerState 81)

action_82 _ = happyReduce_8

action_83 (16) = happyShift action_8
action_83 (17) = happyShift action_9
action_83 (18) = happyShift action_10
action_83 (22) = happyShift action_11
action_83 (25) = happyShift action_12
action_83 (28) = happyShift action_13
action_83 (29) = happyShift action_14
action_83 (36) = happyShift action_15
action_83 (38) = happyShift action_16
action_83 (39) = happyShift action_17
action_83 (46) = happyShift action_18
action_83 (12) = happyGoto action_93
action_83 (13) = happyGoto action_6
action_83 (14) = happyGoto action_7
action_83 _ = happyFail (happyExpListPerState 83)

action_84 _ = happyReduce_44

action_85 (35) = happyShift action_92
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_11

action_87 _ = happyReduce_34

action_88 (31) = happyShift action_91
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (17) = happyShift action_27
action_89 (9) = happyGoto action_90
action_89 _ = happyReduce_9

action_90 (26) = happyShift action_99
action_90 (33) = happyShift action_79
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (16) = happyShift action_8
action_91 (17) = happyShift action_9
action_91 (18) = happyShift action_10
action_91 (22) = happyShift action_11
action_91 (25) = happyShift action_12
action_91 (28) = happyShift action_13
action_91 (29) = happyShift action_14
action_91 (36) = happyShift action_15
action_91 (38) = happyShift action_16
action_91 (39) = happyShift action_17
action_91 (46) = happyShift action_18
action_91 (12) = happyGoto action_98
action_91 (13) = happyGoto action_6
action_91 (14) = happyGoto action_7
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (16) = happyShift action_8
action_92 (17) = happyShift action_9
action_92 (18) = happyShift action_10
action_92 (22) = happyShift action_11
action_92 (25) = happyShift action_12
action_92 (28) = happyShift action_13
action_92 (29) = happyShift action_14
action_92 (36) = happyShift action_15
action_92 (38) = happyShift action_16
action_92 (39) = happyShift action_17
action_92 (46) = happyShift action_18
action_92 (12) = happyGoto action_97
action_92 (13) = happyGoto action_6
action_92 (14) = happyGoto action_7
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (31) = happyShift action_96
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (19) = happyShift action_20
action_94 (20) = happyShift action_21
action_94 (28) = happyShift action_22
action_94 (29) = happyShift action_23
action_94 (11) = happyGoto action_95
action_94 _ = happyFail (happyExpListPerState 94)

action_95 _ = happyReduce_7

action_96 (37) = happyShift action_101
action_96 _ = happyFail (happyExpListPerState 96)

action_97 _ = happyReduce_10

action_98 _ = happyReduce_17

action_99 (28) = happyShift action_100
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (16) = happyShift action_8
action_100 (17) = happyShift action_9
action_100 (18) = happyShift action_10
action_100 (22) = happyShift action_11
action_100 (25) = happyShift action_12
action_100 (28) = happyShift action_13
action_100 (29) = happyShift action_14
action_100 (36) = happyShift action_15
action_100 (38) = happyShift action_16
action_100 (39) = happyShift action_17
action_100 (46) = happyShift action_18
action_100 (12) = happyGoto action_103
action_100 (13) = happyGoto action_6
action_100 (14) = happyGoto action_7
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (16) = happyShift action_8
action_101 (17) = happyShift action_9
action_101 (18) = happyShift action_10
action_101 (22) = happyShift action_11
action_101 (25) = happyShift action_12
action_101 (28) = happyShift action_13
action_101 (29) = happyShift action_14
action_101 (36) = happyShift action_15
action_101 (38) = happyShift action_16
action_101 (39) = happyShift action_17
action_101 (46) = happyShift action_18
action_101 (12) = happyGoto action_102
action_101 (13) = happyGoto action_6
action_101 (14) = happyGoto action_7
action_101 _ = happyFail (happyExpListPerState 101)

action_102 _ = happyReduce_18

action_103 (27) = happyShift action_104
action_103 _ = happyFail (happyExpListPerState 103)

action_104 _ = happyReduce_6

happyReduce_3 = happySpecReduce_2  6 happyReduction_3
happyReduction_3 (HappyAbsSyn12  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (Program happy_var_1 happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  7 happyReduction_4
happyReduction_4 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 ++ [happy_var_2]
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_0  7 happyReduction_5
happyReduction_5  =  HappyAbsSyn7
		 ([]
	)

happyReduce_6 = happyReduce 8 8 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ((happy_var_2, Function happy_var_4 happy_var_7)
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 5 9 happyReduction_7
happyReduction_7 ((HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (happy_var_1 ++ [(happy_var_3, happy_var_5)]
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_3  9 happyReduction_8
happyReduction_8 (HappyAbsSyn11  happy_var_3)
	_
	(HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn9
		 ([(happy_var_1, happy_var_3)]
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_0  9 happyReduction_9
happyReduction_9  =  HappyAbsSyn9
		 ([]
	)

happyReduce_10 = happyReduce 5 10 happyReduction_10
happyReduction_10 ((HappyAbsSyn12  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (happy_var_1 ++ [(happy_var_3, happy_var_5)]
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_3  10 happyReduction_11
happyReduction_11 (HappyAbsSyn12  happy_var_3)
	_
	(HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn10
		 ([(happy_var_1, happy_var_3)]
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_0  10 happyReduction_12
happyReduction_12  =  HappyAbsSyn10
		 ([]
	)

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn11
		 (TInt
	)

happyReduce_14 = happySpecReduce_1  11 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn11
		 (TBool
	)

happyReduce_15 = happySpecReduce_3  11 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (TArray happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  11 happyReduction_16
happyReduction_16 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (TRecord happy_var_2
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happyReduce 6 12 happyReduction_17
happyReduction_17 ((HappyAbsSyn12  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Decl happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 8 12 happyReduction_18
happyReduction_18 ((HappyAbsSyn12  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (If happy_var_3 happy_var_5 happy_var_8
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_1  12 happyReduction_19
happyReduction_19 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  13 happyReduction_20
happyReduction_20 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Bin Or happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  13 happyReduction_21
happyReduction_21 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Bin And happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  13 happyReduction_22
happyReduction_22 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Bin EQ happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  13 happyReduction_23
happyReduction_23 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Bin LT happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  13 happyReduction_24
happyReduction_24 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Bin GT happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  13 happyReduction_25
happyReduction_25 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Bin LE happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  13 happyReduction_26
happyReduction_26 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Bin GE happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  13 happyReduction_27
happyReduction_27 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Bin Add happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  13 happyReduction_28
happyReduction_28 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Bin Sub happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  13 happyReduction_29
happyReduction_29 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Bin Mult happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  13 happyReduction_30
happyReduction_30 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Bin Div happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_2  13 happyReduction_31
happyReduction_31 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (Unary Neg happy_var_2
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_2  13 happyReduction_32
happyReduction_32 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (Unary Not happy_var_2
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  13 happyReduction_33
happyReduction_33 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happyReduce 4 14 happyReduction_34
happyReduction_34 (_ `HappyStk`
	(HappyAbsSyn15  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (Call happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_35 = happySpecReduce_3  14 happyReduction_35
happyReduction_35 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (Record happy_var_2
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  14 happyReduction_36
happyReduction_36 (HappyTerminal (TokenSym happy_var_3))
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Proj happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  14 happyReduction_37
happyReduction_37 _
	(HappyAbsSyn15  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (Array happy_var_2
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  14 happyReduction_38
happyReduction_38 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (Index happy_var_1 happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  14 happyReduction_39
happyReduction_39 (HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn12
		 (Var happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  14 happyReduction_40
happyReduction_40 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn12
		 (Lit (IntV happy_var_1)
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  14 happyReduction_41
happyReduction_41 _
	 =  HappyAbsSyn12
		 (Lit (BoolV True)
	)

happyReduce_42 = happySpecReduce_1  14 happyReduction_42
happyReduction_42 _
	 =  HappyAbsSyn12
		 (Lit (BoolV False)
	)

happyReduce_43 = happySpecReduce_3  14 happyReduction_43
happyReduction_43 _
	(HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn12
		 (happy_var_2
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  15 happyReduction_44
happyReduction_44 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  15 happyReduction_45
happyReduction_45 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn15
		 ([happy_var_1]
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_0  15 happyReduction_46
happyReduction_46  =  HappyAbsSyn15
		 ([]
	)

happyNewToken action sts stk [] =
	action 50 50 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenVar -> cont 16;
	TokenSym happy_dollar_dollar -> cont 17;
	TokenInt happy_dollar_dollar -> cont 18;
	TokenTInt -> cont 19;
	TokenTBool -> cont 20;
	TokenPlus -> cont 21;
	TokenMinus -> cont 22;
	TokenTimes -> cont 23;
	TokenDiv -> cont 24;
	TokenLParen -> cont 25;
	TokenRParen -> cont 26;
	TokenRBrace -> cont 27;
	TokenLBrace -> cont 28;
	TokenLBracket -> cont 29;
	TokenRBracket -> cont 30;
	TokenSemiColon -> cont 31;
	TokenColon -> cont 32;
	TokenComma -> cont 33;
	TokenPeriod -> cont 34;
	TokenEq -> cont 35;
	TokenIf -> cont 36;
	TokenElse -> cont 37;
	TokenTrue -> cont 38;
	TokenFalse -> cont 39;
	TokenLT -> cont 40;
	TokenLE -> cont 41;
	TokenGT -> cont 42;
	TokenGE -> cont 43;
	TokenComp -> cont 44;
	TokenAnd -> cont 45;
	TokenNot -> cont 46;
	TokenIndex -> cont 47;
	TokenOr -> cont 48;
	TokenFunc -> cont 49;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 50 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Either String a -> (a -> Either String b) -> Either String b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> Either String a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Either String a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Either String a
happyError' = (\(tokens, _) -> parseError tokens)
parserProg tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn6 z -> happyReturn z; _other -> notHappyAtAll })

parserType tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn11 z -> happyReturn z; _other -> notHappyAtAll })

parserExpr tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn12 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> Either String a
parseError _ = Left "Parse error"

parseProg :: String -> Either String Program
parseProg = parserProg . scanTokens

parseType :: String -> Either String Type
parseType = parserType . scanTokens

parseExpr :: String -> Either String Exp
parseExpr = parserExpr . scanTokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
