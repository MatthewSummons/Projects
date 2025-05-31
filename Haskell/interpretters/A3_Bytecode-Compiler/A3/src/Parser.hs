{-# OPTIONS_GHC -w #-}
module Parser (parseExpr) where
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
	| HappyAbsSyn5 (Exp)
	| HappyAbsSyn6 ([(Ident, Type, Exp)])
	| HappyAbsSyn8 (Type)

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
 action_85 :: () => Prelude.Int -> ({-HappyReduction (HappyIdentity) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_2,
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
 happyReduce_30 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Prelude.Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,264) ([24576,0,0,4662,41064,0,0,256,16384,0,0,120,764,0,1,0,2,0,1024,0,0,0,0,0,0,0,9324,16592,55297,41032,641,32768,0,0,0,0,0,0,32768,1165,10266,0,0,128,0,0,0,0,0,768,0,0,0,0,9056,1665,10,1504,3056,0,0,0,32772,0,0,512,0,0,4,55296,41032,641,37296,832,24581,33059,2566,18112,3330,32788,1165,10266,6912,13321,80,4662,41064,27648,53284,320,18648,33184,45058,16529,1283,9056,1665,49162,582,5133,1024,0,0,0,1,0,32783,31,7680,7936,0,60,30,30720,0,0,240,0,57344,1,0,960,0,0,0,0,0,0,0,24,0,12288,0,0,376,764,3072,0,0,24,0,1024,0,0,2331,20532,0,0,0,24064,48896,0,0,1024,37296,832,5,240,1528,0,64,0,0,16385,0,512,128,0,0,32768,1,0,32768,0,45060,16529,1283,9056,1665,10,24,0,49152,57411,23,0,8,0,1024,256,7680,48896,0,1084,382,0,4,24576,33059,2566,18112,3330,32788,1165,10266,6912,13321,80,3840,24448,0,30,191,15360,32256,1,632,764,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parserType","%start_parserExpr","Exp","Vars","App","typ","var","vars","and","id","int","Int","Bool","'+'","'-'","'*'","'/'","'('","')'","'}'","'{'","';'","':'","'='","if","else","true","false","'<'","'<='","'>'","'>='","'=='","'&&'","'!'","'||'","fun","'->'","%eof"]
        bit_start = st Prelude.* 41
        bit_end = (st Prelude.+ 1) Prelude.* 41
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..40]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (14) = happyShift action_17
action_0 (15) = happyShift action_18
action_0 (8) = happyGoto action_16
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (9) = happyShift action_6
action_1 (10) = happyShift action_7
action_1 (12) = happyShift action_8
action_1 (13) = happyShift action_9
action_1 (17) = happyShift action_10
action_1 (20) = happyShift action_11
action_1 (27) = happyShift action_12
action_1 (29) = happyShift action_13
action_1 (30) = happyShift action_14
action_1 (37) = happyShift action_15
action_1 (39) = happyShift action_3
action_1 (5) = happyGoto action_4
action_1 (7) = happyGoto action_5
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (39) = happyShift action_3
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (20) = happyShift action_39
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (16) = happyShift action_28
action_4 (17) = happyShift action_29
action_4 (18) = happyShift action_30
action_4 (19) = happyShift action_31
action_4 (31) = happyShift action_32
action_4 (32) = happyShift action_33
action_4 (33) = happyShift action_34
action_4 (34) = happyShift action_35
action_4 (35) = happyShift action_36
action_4 (36) = happyShift action_37
action_4 (38) = happyShift action_38
action_4 (41) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (20) = happyShift action_27
action_5 _ = happyReduce_19

action_6 (12) = happyShift action_26
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (12) = happyShift action_25
action_7 (6) = happyGoto action_24
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_23

action_9 _ = happyReduce_25

action_10 (9) = happyShift action_6
action_10 (10) = happyShift action_7
action_10 (12) = happyShift action_8
action_10 (13) = happyShift action_9
action_10 (17) = happyShift action_10
action_10 (20) = happyShift action_11
action_10 (27) = happyShift action_12
action_10 (29) = happyShift action_13
action_10 (30) = happyShift action_14
action_10 (37) = happyShift action_15
action_10 (39) = happyShift action_3
action_10 (5) = happyGoto action_23
action_10 (7) = happyGoto action_5
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (9) = happyShift action_6
action_11 (10) = happyShift action_7
action_11 (12) = happyShift action_8
action_11 (13) = happyShift action_9
action_11 (17) = happyShift action_10
action_11 (20) = happyShift action_11
action_11 (27) = happyShift action_12
action_11 (29) = happyShift action_13
action_11 (30) = happyShift action_14
action_11 (37) = happyShift action_15
action_11 (39) = happyShift action_3
action_11 (5) = happyGoto action_22
action_11 (7) = happyGoto action_5
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (20) = happyShift action_21
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_26

action_14 _ = happyReduce_27

action_15 (9) = happyShift action_6
action_15 (10) = happyShift action_7
action_15 (12) = happyShift action_8
action_15 (13) = happyShift action_9
action_15 (17) = happyShift action_10
action_15 (20) = happyShift action_11
action_15 (27) = happyShift action_12
action_15 (29) = happyShift action_13
action_15 (30) = happyShift action_14
action_15 (37) = happyShift action_15
action_15 (39) = happyShift action_3
action_15 (5) = happyGoto action_20
action_15 (7) = happyGoto action_5
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (40) = happyShift action_19
action_16 (41) = happyAccept
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_28

action_18 _ = happyReduce_29

action_19 (14) = happyShift action_17
action_19 (15) = happyShift action_18
action_19 (8) = happyGoto action_59
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_18

action_21 (9) = happyShift action_6
action_21 (10) = happyShift action_7
action_21 (12) = happyShift action_8
action_21 (13) = happyShift action_9
action_21 (17) = happyShift action_10
action_21 (20) = happyShift action_11
action_21 (27) = happyShift action_12
action_21 (29) = happyShift action_13
action_21 (30) = happyShift action_14
action_21 (37) = happyShift action_15
action_21 (39) = happyShift action_3
action_21 (5) = happyGoto action_58
action_21 (7) = happyGoto action_5
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (16) = happyShift action_28
action_22 (17) = happyShift action_29
action_22 (18) = happyShift action_30
action_22 (19) = happyShift action_31
action_22 (21) = happyShift action_57
action_22 (31) = happyShift action_32
action_22 (32) = happyShift action_33
action_22 (33) = happyShift action_34
action_22 (34) = happyShift action_35
action_22 (35) = happyShift action_36
action_22 (36) = happyShift action_37
action_22 (38) = happyShift action_38
action_22 _ = happyFail (happyExpListPerState 22)

action_23 _ = happyReduce_17

action_24 (11) = happyShift action_55
action_24 (24) = happyShift action_56
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (25) = happyShift action_54
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (25) = happyShift action_53
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (9) = happyShift action_6
action_27 (10) = happyShift action_7
action_27 (12) = happyShift action_8
action_27 (13) = happyShift action_9
action_27 (17) = happyShift action_10
action_27 (20) = happyShift action_11
action_27 (27) = happyShift action_12
action_27 (29) = happyShift action_13
action_27 (30) = happyShift action_14
action_27 (37) = happyShift action_15
action_27 (39) = happyShift action_3
action_27 (5) = happyGoto action_52
action_27 (7) = happyGoto action_5
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (9) = happyShift action_6
action_28 (10) = happyShift action_7
action_28 (12) = happyShift action_8
action_28 (13) = happyShift action_9
action_28 (17) = happyShift action_10
action_28 (20) = happyShift action_11
action_28 (27) = happyShift action_12
action_28 (29) = happyShift action_13
action_28 (30) = happyShift action_14
action_28 (37) = happyShift action_15
action_28 (39) = happyShift action_3
action_28 (5) = happyGoto action_51
action_28 (7) = happyGoto action_5
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (9) = happyShift action_6
action_29 (10) = happyShift action_7
action_29 (12) = happyShift action_8
action_29 (13) = happyShift action_9
action_29 (17) = happyShift action_10
action_29 (20) = happyShift action_11
action_29 (27) = happyShift action_12
action_29 (29) = happyShift action_13
action_29 (30) = happyShift action_14
action_29 (37) = happyShift action_15
action_29 (39) = happyShift action_3
action_29 (5) = happyGoto action_50
action_29 (7) = happyGoto action_5
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (9) = happyShift action_6
action_30 (10) = happyShift action_7
action_30 (12) = happyShift action_8
action_30 (13) = happyShift action_9
action_30 (17) = happyShift action_10
action_30 (20) = happyShift action_11
action_30 (27) = happyShift action_12
action_30 (29) = happyShift action_13
action_30 (30) = happyShift action_14
action_30 (37) = happyShift action_15
action_30 (39) = happyShift action_3
action_30 (5) = happyGoto action_49
action_30 (7) = happyGoto action_5
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (9) = happyShift action_6
action_31 (10) = happyShift action_7
action_31 (12) = happyShift action_8
action_31 (13) = happyShift action_9
action_31 (17) = happyShift action_10
action_31 (20) = happyShift action_11
action_31 (27) = happyShift action_12
action_31 (29) = happyShift action_13
action_31 (30) = happyShift action_14
action_31 (37) = happyShift action_15
action_31 (39) = happyShift action_3
action_31 (5) = happyGoto action_48
action_31 (7) = happyGoto action_5
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (9) = happyShift action_6
action_32 (10) = happyShift action_7
action_32 (12) = happyShift action_8
action_32 (13) = happyShift action_9
action_32 (17) = happyShift action_10
action_32 (20) = happyShift action_11
action_32 (27) = happyShift action_12
action_32 (29) = happyShift action_13
action_32 (30) = happyShift action_14
action_32 (37) = happyShift action_15
action_32 (39) = happyShift action_3
action_32 (5) = happyGoto action_47
action_32 (7) = happyGoto action_5
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (9) = happyShift action_6
action_33 (10) = happyShift action_7
action_33 (12) = happyShift action_8
action_33 (13) = happyShift action_9
action_33 (17) = happyShift action_10
action_33 (20) = happyShift action_11
action_33 (27) = happyShift action_12
action_33 (29) = happyShift action_13
action_33 (30) = happyShift action_14
action_33 (37) = happyShift action_15
action_33 (39) = happyShift action_3
action_33 (5) = happyGoto action_46
action_33 (7) = happyGoto action_5
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (9) = happyShift action_6
action_34 (10) = happyShift action_7
action_34 (12) = happyShift action_8
action_34 (13) = happyShift action_9
action_34 (17) = happyShift action_10
action_34 (20) = happyShift action_11
action_34 (27) = happyShift action_12
action_34 (29) = happyShift action_13
action_34 (30) = happyShift action_14
action_34 (37) = happyShift action_15
action_34 (39) = happyShift action_3
action_34 (5) = happyGoto action_45
action_34 (7) = happyGoto action_5
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (9) = happyShift action_6
action_35 (10) = happyShift action_7
action_35 (12) = happyShift action_8
action_35 (13) = happyShift action_9
action_35 (17) = happyShift action_10
action_35 (20) = happyShift action_11
action_35 (27) = happyShift action_12
action_35 (29) = happyShift action_13
action_35 (30) = happyShift action_14
action_35 (37) = happyShift action_15
action_35 (39) = happyShift action_3
action_35 (5) = happyGoto action_44
action_35 (7) = happyGoto action_5
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (9) = happyShift action_6
action_36 (10) = happyShift action_7
action_36 (12) = happyShift action_8
action_36 (13) = happyShift action_9
action_36 (17) = happyShift action_10
action_36 (20) = happyShift action_11
action_36 (27) = happyShift action_12
action_36 (29) = happyShift action_13
action_36 (30) = happyShift action_14
action_36 (37) = happyShift action_15
action_36 (39) = happyShift action_3
action_36 (5) = happyGoto action_43
action_36 (7) = happyGoto action_5
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (9) = happyShift action_6
action_37 (10) = happyShift action_7
action_37 (12) = happyShift action_8
action_37 (13) = happyShift action_9
action_37 (17) = happyShift action_10
action_37 (20) = happyShift action_11
action_37 (27) = happyShift action_12
action_37 (29) = happyShift action_13
action_37 (30) = happyShift action_14
action_37 (37) = happyShift action_15
action_37 (39) = happyShift action_3
action_37 (5) = happyGoto action_42
action_37 (7) = happyGoto action_5
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (9) = happyShift action_6
action_38 (10) = happyShift action_7
action_38 (12) = happyShift action_8
action_38 (13) = happyShift action_9
action_38 (17) = happyShift action_10
action_38 (20) = happyShift action_11
action_38 (27) = happyShift action_12
action_38 (29) = happyShift action_13
action_38 (30) = happyShift action_14
action_38 (37) = happyShift action_15
action_38 (39) = happyShift action_3
action_38 (5) = happyGoto action_41
action_38 (7) = happyGoto action_5
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (12) = happyShift action_40
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (25) = happyShift action_66
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (16) = happyShift action_28
action_41 (17) = happyShift action_29
action_41 (18) = happyShift action_30
action_41 (19) = happyShift action_31
action_41 (31) = happyShift action_32
action_41 (32) = happyShift action_33
action_41 (33) = happyShift action_34
action_41 (34) = happyShift action_35
action_41 (35) = happyShift action_36
action_41 (36) = happyShift action_37
action_41 _ = happyReduce_6

action_42 (16) = happyShift action_28
action_42 (17) = happyShift action_29
action_42 (18) = happyShift action_30
action_42 (19) = happyShift action_31
action_42 (31) = happyShift action_32
action_42 (32) = happyShift action_33
action_42 (33) = happyShift action_34
action_42 (34) = happyShift action_35
action_42 (35) = happyShift action_36
action_42 _ = happyReduce_7

action_43 (16) = happyShift action_28
action_43 (17) = happyShift action_29
action_43 (18) = happyShift action_30
action_43 (19) = happyShift action_31
action_43 (31) = happyShift action_32
action_43 (32) = happyShift action_33
action_43 (33) = happyShift action_34
action_43 (34) = happyShift action_35
action_43 (35) = happyFail []
action_43 _ = happyReduce_8

action_44 (16) = happyShift action_28
action_44 (17) = happyShift action_29
action_44 (18) = happyShift action_30
action_44 (19) = happyShift action_31
action_44 (31) = happyFail []
action_44 (32) = happyFail []
action_44 (33) = happyFail []
action_44 (34) = happyFail []
action_44 _ = happyReduce_12

action_45 (16) = happyShift action_28
action_45 (17) = happyShift action_29
action_45 (18) = happyShift action_30
action_45 (19) = happyShift action_31
action_45 (31) = happyFail []
action_45 (32) = happyFail []
action_45 (33) = happyFail []
action_45 (34) = happyFail []
action_45 _ = happyReduce_10

action_46 (16) = happyShift action_28
action_46 (17) = happyShift action_29
action_46 (18) = happyShift action_30
action_46 (19) = happyShift action_31
action_46 (31) = happyFail []
action_46 (32) = happyFail []
action_46 (33) = happyFail []
action_46 (34) = happyFail []
action_46 _ = happyReduce_11

action_47 (16) = happyShift action_28
action_47 (17) = happyShift action_29
action_47 (18) = happyShift action_30
action_47 (19) = happyShift action_31
action_47 (31) = happyFail []
action_47 (32) = happyFail []
action_47 (33) = happyFail []
action_47 (34) = happyFail []
action_47 _ = happyReduce_9

action_48 _ = happyReduce_16

action_49 _ = happyReduce_15

action_50 (18) = happyShift action_30
action_50 (19) = happyShift action_31
action_50 _ = happyReduce_14

action_51 (18) = happyShift action_30
action_51 (19) = happyShift action_31
action_51 _ = happyReduce_13

action_52 (16) = happyShift action_28
action_52 (17) = happyShift action_29
action_52 (18) = happyShift action_30
action_52 (19) = happyShift action_31
action_52 (21) = happyShift action_65
action_52 (31) = happyShift action_32
action_52 (32) = happyShift action_33
action_52 (33) = happyShift action_34
action_52 (34) = happyShift action_35
action_52 (35) = happyShift action_36
action_52 (36) = happyShift action_37
action_52 (38) = happyShift action_38
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (14) = happyShift action_17
action_53 (15) = happyShift action_18
action_53 (8) = happyGoto action_64
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (14) = happyShift action_17
action_54 (15) = happyShift action_18
action_54 (8) = happyGoto action_63
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (12) = happyShift action_62
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (9) = happyShift action_6
action_56 (10) = happyShift action_7
action_56 (12) = happyShift action_8
action_56 (13) = happyShift action_9
action_56 (17) = happyShift action_10
action_56 (20) = happyShift action_11
action_56 (27) = happyShift action_12
action_56 (29) = happyShift action_13
action_56 (30) = happyShift action_14
action_56 (37) = happyShift action_15
action_56 (39) = happyShift action_3
action_56 (5) = happyGoto action_61
action_56 (7) = happyGoto action_5
action_56 _ = happyFail (happyExpListPerState 56)

action_57 _ = happyReduce_24

action_58 (16) = happyShift action_28
action_58 (17) = happyShift action_29
action_58 (18) = happyShift action_30
action_58 (19) = happyShift action_31
action_58 (21) = happyShift action_60
action_58 (31) = happyShift action_32
action_58 (32) = happyShift action_33
action_58 (33) = happyShift action_34
action_58 (34) = happyShift action_35
action_58 (35) = happyShift action_36
action_58 (36) = happyShift action_37
action_58 (38) = happyShift action_38
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (40) = happyShift action_19
action_59 _ = happyReduce_30

action_60 (9) = happyShift action_6
action_60 (10) = happyShift action_7
action_60 (12) = happyShift action_8
action_60 (13) = happyShift action_9
action_60 (17) = happyShift action_10
action_60 (20) = happyShift action_11
action_60 (27) = happyShift action_12
action_60 (29) = happyShift action_13
action_60 (30) = happyShift action_14
action_60 (37) = happyShift action_15
action_60 (39) = happyShift action_3
action_60 (5) = happyGoto action_71
action_60 (7) = happyGoto action_5
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (16) = happyShift action_28
action_61 (17) = happyShift action_29
action_61 (18) = happyShift action_30
action_61 (19) = happyShift action_31
action_61 (31) = happyShift action_32
action_61 (32) = happyShift action_33
action_61 (33) = happyShift action_34
action_61 (34) = happyShift action_35
action_61 (35) = happyShift action_36
action_61 (36) = happyShift action_37
action_61 (38) = happyShift action_38
action_61 _ = happyReduce_4

action_62 (25) = happyShift action_70
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (26) = happyShift action_69
action_63 (40) = happyShift action_19
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (26) = happyShift action_68
action_64 (40) = happyShift action_19
action_64 _ = happyFail (happyExpListPerState 64)

action_65 _ = happyReduce_22

action_66 (14) = happyShift action_17
action_66 (15) = happyShift action_18
action_66 (8) = happyGoto action_67
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (21) = happyShift action_76
action_67 (40) = happyShift action_19
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (9) = happyShift action_6
action_68 (10) = happyShift action_7
action_68 (12) = happyShift action_8
action_68 (13) = happyShift action_9
action_68 (17) = happyShift action_10
action_68 (20) = happyShift action_11
action_68 (27) = happyShift action_12
action_68 (29) = happyShift action_13
action_68 (30) = happyShift action_14
action_68 (37) = happyShift action_15
action_68 (39) = happyShift action_3
action_68 (5) = happyGoto action_75
action_68 (7) = happyGoto action_5
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (9) = happyShift action_6
action_69 (10) = happyShift action_7
action_69 (12) = happyShift action_8
action_69 (13) = happyShift action_9
action_69 (17) = happyShift action_10
action_69 (20) = happyShift action_11
action_69 (27) = happyShift action_12
action_69 (29) = happyShift action_13
action_69 (30) = happyShift action_14
action_69 (37) = happyShift action_15
action_69 (39) = happyShift action_3
action_69 (5) = happyGoto action_74
action_69 (7) = happyGoto action_5
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (14) = happyShift action_17
action_70 (15) = happyShift action_18
action_70 (8) = happyGoto action_73
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (16) = happyShift action_28
action_71 (17) = happyShift action_29
action_71 (18) = happyShift action_30
action_71 (19) = happyShift action_31
action_71 (24) = happyShift action_72
action_71 (31) = happyShift action_32
action_71 (32) = happyShift action_33
action_71 (33) = happyShift action_34
action_71 (34) = happyShift action_35
action_71 (35) = happyShift action_36
action_71 (36) = happyShift action_37
action_71 (38) = happyShift action_38
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (28) = happyShift action_80
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (26) = happyShift action_79
action_73 (40) = happyShift action_19
action_73 _ = happyFail (happyExpListPerState 73)

action_74 (16) = happyShift action_28
action_74 (17) = happyShift action_29
action_74 (18) = happyShift action_30
action_74 (19) = happyShift action_31
action_74 (31) = happyShift action_32
action_74 (32) = happyShift action_33
action_74 (33) = happyShift action_34
action_74 (34) = happyShift action_35
action_74 (35) = happyShift action_36
action_74 (36) = happyShift action_37
action_74 (38) = happyShift action_38
action_74 _ = happyReduce_20

action_75 (16) = happyShift action_28
action_75 (17) = happyShift action_29
action_75 (18) = happyShift action_30
action_75 (19) = happyShift action_31
action_75 (24) = happyShift action_78
action_75 (31) = happyShift action_32
action_75 (32) = happyShift action_33
action_75 (33) = happyShift action_34
action_75 (34) = happyShift action_35
action_75 (35) = happyShift action_36
action_75 (36) = happyShift action_37
action_75 (38) = happyShift action_38
action_75 _ = happyFail (happyExpListPerState 75)

action_76 (23) = happyShift action_77
action_76 _ = happyFail (happyExpListPerState 76)

action_77 (9) = happyShift action_6
action_77 (10) = happyShift action_7
action_77 (12) = happyShift action_8
action_77 (13) = happyShift action_9
action_77 (17) = happyShift action_10
action_77 (20) = happyShift action_11
action_77 (27) = happyShift action_12
action_77 (29) = happyShift action_13
action_77 (30) = happyShift action_14
action_77 (37) = happyShift action_15
action_77 (39) = happyShift action_3
action_77 (5) = happyGoto action_84
action_77 (7) = happyGoto action_5
action_77 _ = happyFail (happyExpListPerState 77)

action_78 (9) = happyShift action_6
action_78 (10) = happyShift action_7
action_78 (12) = happyShift action_8
action_78 (13) = happyShift action_9
action_78 (17) = happyShift action_10
action_78 (20) = happyShift action_11
action_78 (27) = happyShift action_12
action_78 (29) = happyShift action_13
action_78 (30) = happyShift action_14
action_78 (37) = happyShift action_15
action_78 (39) = happyShift action_3
action_78 (5) = happyGoto action_83
action_78 (7) = happyGoto action_5
action_78 _ = happyFail (happyExpListPerState 78)

action_79 (9) = happyShift action_6
action_79 (10) = happyShift action_7
action_79 (12) = happyShift action_8
action_79 (13) = happyShift action_9
action_79 (17) = happyShift action_10
action_79 (20) = happyShift action_11
action_79 (27) = happyShift action_12
action_79 (29) = happyShift action_13
action_79 (30) = happyShift action_14
action_79 (37) = happyShift action_15
action_79 (39) = happyShift action_3
action_79 (5) = happyGoto action_82
action_79 (7) = happyGoto action_5
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (9) = happyShift action_6
action_80 (10) = happyShift action_7
action_80 (12) = happyShift action_8
action_80 (13) = happyShift action_9
action_80 (17) = happyShift action_10
action_80 (20) = happyShift action_11
action_80 (27) = happyShift action_12
action_80 (29) = happyShift action_13
action_80 (30) = happyShift action_14
action_80 (37) = happyShift action_15
action_80 (39) = happyShift action_3
action_80 (5) = happyGoto action_81
action_80 (7) = happyGoto action_5
action_80 _ = happyFail (happyExpListPerState 80)

action_81 (16) = happyShift action_28
action_81 (17) = happyShift action_29
action_81 (18) = happyShift action_30
action_81 (19) = happyShift action_31
action_81 (31) = happyShift action_32
action_81 (32) = happyShift action_33
action_81 (33) = happyShift action_34
action_81 (34) = happyShift action_35
action_81 (35) = happyShift action_36
action_81 (36) = happyShift action_37
action_81 (38) = happyShift action_38
action_81 _ = happyReduce_5

action_82 (16) = happyShift action_28
action_82 (17) = happyShift action_29
action_82 (18) = happyShift action_30
action_82 (19) = happyShift action_31
action_82 (31) = happyShift action_32
action_82 (32) = happyShift action_33
action_82 (33) = happyShift action_34
action_82 (34) = happyShift action_35
action_82 (35) = happyShift action_36
action_82 (36) = happyShift action_37
action_82 (38) = happyShift action_38
action_82 _ = happyReduce_21

action_83 (16) = happyShift action_28
action_83 (17) = happyShift action_29
action_83 (18) = happyShift action_30
action_83 (19) = happyShift action_31
action_83 (31) = happyShift action_32
action_83 (32) = happyShift action_33
action_83 (33) = happyShift action_34
action_83 (34) = happyShift action_35
action_83 (35) = happyShift action_36
action_83 (36) = happyShift action_37
action_83 (38) = happyShift action_38
action_83 _ = happyReduce_3

action_84 (16) = happyShift action_28
action_84 (17) = happyShift action_29
action_84 (18) = happyShift action_30
action_84 (19) = happyShift action_31
action_84 (22) = happyShift action_85
action_84 (31) = happyShift action_32
action_84 (32) = happyShift action_33
action_84 (33) = happyShift action_34
action_84 (34) = happyShift action_35
action_84 (35) = happyShift action_36
action_84 (36) = happyShift action_37
action_84 (38) = happyShift action_38
action_84 _ = happyFail (happyExpListPerState 84)

action_85 _ = happyReduce_2

happyReduce_2 = happyReduce 9 5 happyReduction_2
happyReduction_2 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Fun (happy_var_3, happy_var_5) happy_var_8
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 8 5 happyReduction_3
happyReduction_3 ((HappyAbsSyn5  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Decl happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 4 5 happyReduction_4
happyReduction_4 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (MultDecl happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 8 5 happyReduction_5
happyReduction_5 ((HappyAbsSyn5  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (If happy_var_3 happy_var_5 happy_var_8
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Bin Or happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  5 happyReduction_7
happyReduction_7 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Bin And happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  5 happyReduction_8
happyReduction_8 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Bin EQ happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  5 happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Bin LT happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  5 happyReduction_10
happyReduction_10 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Bin GT happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  5 happyReduction_11
happyReduction_11 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Bin LE happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  5 happyReduction_12
happyReduction_12 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Bin GE happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  5 happyReduction_13
happyReduction_13 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Bin Add happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  5 happyReduction_14
happyReduction_14 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Bin Sub happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  5 happyReduction_15
happyReduction_15 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Bin Mult happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  5 happyReduction_16
happyReduction_16 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Bin Div happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  5 happyReduction_17
happyReduction_17 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Unary Neg happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  5 happyReduction_18
happyReduction_18 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Unary Not happy_var_2
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  5 happyReduction_19
happyReduction_19 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happyReduce 5 6 happyReduction_20
happyReduction_20 ((HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 ([(happy_var_1, happy_var_3, happy_var_5)]
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 7 6 happyReduction_21
happyReduction_21 ((HappyAbsSyn5  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenSym happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 ((happy_var_3, happy_var_5, happy_var_7):happy_var_1
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 4 7 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Call happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_1  7 happyReduction_23
happyReduction_23 (HappyTerminal (TokenSym happy_var_1))
	 =  HappyAbsSyn5
		 (Var happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  7 happyReduction_24
happyReduction_24 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  7 happyReduction_25
happyReduction_25 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn5
		 (Lit (IntV happy_var_1)
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  7 happyReduction_26
happyReduction_26 _
	 =  HappyAbsSyn5
		 (Lit (BoolV True)
	)

happyReduce_27 = happySpecReduce_1  7 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn5
		 (Lit (BoolV False)
	)

happyReduce_28 = happySpecReduce_1  8 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn8
		 (TInt
	)

happyReduce_29 = happySpecReduce_1  8 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn8
		 (TBool
	)

happyReduce_30 = happySpecReduce_3  8 happyReduction_30
happyReduction_30 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (TFun happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 41 41 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenVar -> cont 9;
	TokenVars -> cont 10;
	TokenAAnd -> cont 11;
	TokenSym happy_dollar_dollar -> cont 12;
	TokenInt happy_dollar_dollar -> cont 13;
	TokenTInt -> cont 14;
	TokenTBool -> cont 15;
	TokenPlus -> cont 16;
	TokenMinus -> cont 17;
	TokenTimes -> cont 18;
	TokenDiv -> cont 19;
	TokenLParen -> cont 20;
	TokenRParen -> cont 21;
	TokenRB -> cont 22;
	TokenLB -> cont 23;
	TokenSemiColon -> cont 24;
	TokenColon -> cont 25;
	TokenEq -> cont 26;
	TokenIf -> cont 27;
	TokenElse -> cont 28;
	TokenTrue -> cont 29;
	TokenFalse -> cont 30;
	TokenLT -> cont 31;
	TokenLE -> cont 32;
	TokenGT -> cont 33;
	TokenGE -> cont 34;
	TokenComp -> cont 35;
	TokenAnd -> cont 36;
	TokenNot -> cont 37;
	TokenOr -> cont 38;
	TokenFunc -> cont 39;
	TokenArrow -> cont 40;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 41 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parserType tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn8 z -> happyReturn z; _other -> notHappyAtAll })

parserExpr tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn5 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

parseExpr :: String -> Exp
parseExpr = parserExpr . scanTokens

parseType :: String -> Type
parseType = parserType . scanTokens
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
