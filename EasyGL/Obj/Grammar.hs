{-# OPTIONS_GHC -w #-}
module EasyGL.Obj.Grammar where
import qualified EasyGL.Obj.Lexer as L
import EasyGL.Obj.ObjData
import Graphics.UI.GLUT
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10
	= HappyTerminal (L.Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10

action_0 (11) = happyShift action_3
action_0 (12) = happyShift action_7
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_5
action_0 (6) = happyGoto action_6
action_0 (9) = happyGoto action_2
action_0 _ = happyFail

action_1 (11) = happyShift action_3
action_1 (9) = happyGoto action_2
action_1 _ = happyFail

action_2 (12) = happyShift action_7
action_2 (5) = happyGoto action_14
action_2 (6) = happyGoto action_6
action_2 _ = happyReduce_1

action_3 _ = happyReduce_14

action_4 (20) = happyAccept
action_4 _ = happyFail

action_5 _ = happyReduce_2

action_6 (13) = happyShift action_10
action_6 (14) = happyShift action_11
action_6 (15) = happyShift action_12
action_6 (16) = happyShift action_13
action_6 (7) = happyGoto action_8
action_6 (8) = happyGoto action_9
action_6 _ = happyFail

action_7 _ = happyReduce_4

action_8 (16) = happyShift action_13
action_8 (8) = happyGoto action_21
action_8 _ = happyReduce_3

action_9 _ = happyReduce_8

action_10 (17) = happyShift action_17
action_10 (18) = happyShift action_18
action_10 (10) = happyGoto action_20
action_10 _ = happyFail

action_11 (17) = happyShift action_17
action_11 (18) = happyShift action_18
action_11 (10) = happyGoto action_19
action_11 _ = happyFail

action_12 (17) = happyShift action_17
action_12 (18) = happyShift action_18
action_12 (10) = happyGoto action_16
action_12 _ = happyFail

action_13 (18) = happyShift action_15
action_13 _ = happyFail

action_14 _ = happyReduce_15

action_15 (18) = happyShift action_25
action_15 (19) = happyShift action_26
action_15 _ = happyFail

action_16 (17) = happyShift action_17
action_16 (18) = happyShift action_18
action_16 (10) = happyGoto action_24
action_16 _ = happyFail

action_17 _ = happyReduce_16

action_18 _ = happyReduce_17

action_19 (17) = happyShift action_17
action_19 (18) = happyShift action_18
action_19 (10) = happyGoto action_23
action_19 _ = happyFail

action_20 (17) = happyShift action_17
action_20 (18) = happyShift action_18
action_20 (10) = happyGoto action_22
action_20 _ = happyFail

action_21 _ = happyReduce_9

action_22 (17) = happyShift action_17
action_22 (18) = happyShift action_18
action_22 (10) = happyGoto action_31
action_22 _ = happyFail

action_23 _ = happyReduce_6

action_24 (17) = happyShift action_17
action_24 (18) = happyShift action_18
action_24 (10) = happyGoto action_30
action_24 _ = happyFail

action_25 (18) = happyShift action_29
action_25 _ = happyFail

action_26 (18) = happyShift action_27
action_26 (19) = happyShift action_28
action_26 _ = happyFail

action_27 (18) = happyShift action_33
action_27 (19) = happyShift action_34
action_27 _ = happyFail

action_28 (18) = happyShift action_32
action_28 _ = happyFail

action_29 _ = happyReduce_10

action_30 _ = happyReduce_7

action_31 _ = happyReduce_5

action_32 (18) = happyShift action_37
action_32 _ = happyFail

action_33 (19) = happyShift action_36
action_33 _ = happyFail

action_34 (18) = happyShift action_35
action_34 _ = happyFail

action_35 (18) = happyShift action_40
action_35 _ = happyFail

action_36 (18) = happyShift action_39
action_36 _ = happyFail

action_37 (19) = happyShift action_38
action_37 _ = happyFail

action_38 (19) = happyShift action_43
action_38 _ = happyFail

action_39 (18) = happyShift action_42
action_39 _ = happyFail

action_40 (19) = happyShift action_41
action_40 _ = happyFail

action_41 (18) = happyShift action_46
action_41 _ = happyFail

action_42 (19) = happyShift action_45
action_42 _ = happyFail

action_43 (18) = happyShift action_44
action_43 _ = happyFail

action_44 (18) = happyShift action_49
action_44 _ = happyFail

action_45 (18) = happyShift action_48
action_45 _ = happyFail

action_46 (19) = happyShift action_47
action_46 _ = happyFail

action_47 (18) = happyShift action_51
action_47 _ = happyFail

action_48 _ = happyReduce_11

action_49 (19) = happyShift action_50
action_49 _ = happyFail

action_50 (19) = happyShift action_53
action_50 _ = happyFail

action_51 (18) = happyShift action_52
action_51 _ = happyFail

action_52 (19) = happyShift action_55
action_52 _ = happyFail

action_53 (18) = happyShift action_54
action_53 _ = happyFail

action_54 _ = happyReduce_13

action_55 (18) = happyShift action_56
action_55 _ = happyFail

action_56 (19) = happyShift action_57
action_56 _ = happyFail

action_57 (18) = happyShift action_58
action_57 _ = happyFail

action_58 _ = happyReduce_12

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ((emptyObj "") {groups=[happy_var_1]}
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1{indexes=iindices happy_var_2,normalsIndex=vindices happy_var_2,textureCoordIndex=tindices happy_var_2}
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyTerminal (L.Group happy_var_1))
	 =  HappyAbsSyn6
		 (emptyGroup happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happyReduce 5 6 happyReduction_5
happyReduction_5 ((HappyAbsSyn10  happy_var_5) `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (happy_var_1{vertices=(Vertex3 happy_var_3 happy_var_4 happy_var_5):vertices happy_var_1}
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 4 6 happyReduction_6
happyReduction_6 ((HappyAbsSyn10  happy_var_4) `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (happy_var_1{textureCoord=(Vector2 happy_var_3 happy_var_4):textureCoord happy_var_1}
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 5 6 happyReduction_7
happyReduction_7 ((HappyAbsSyn10  happy_var_5) `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	(HappyAbsSyn10  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (happy_var_1{normals=(Vector3 happy_var_3 happy_var_4 happy_var_5):normals happy_var_1}
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_1  7 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  7 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_2:happy_var_1
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 4 8 happyReduction_10
happyReduction_10 ((HappyTerminal (L.TInt happy_var_4)) `HappyStk`
	(HappyTerminal (L.TInt happy_var_3)) `HappyStk`
	(HappyTerminal (L.TInt happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (([happy_var_2-1,happy_var_3-1,happy_var_4-1],[],[])
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 10 8 happyReduction_11
happyReduction_11 ((HappyTerminal (L.TInt happy_var_10)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.TInt happy_var_8)) `HappyStk`
	(HappyTerminal (L.TInt happy_var_7)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.TInt happy_var_5)) `HappyStk`
	(HappyTerminal (L.TInt happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.TInt happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (([happy_var_2-1,happy_var_5-1,happy_var_8-1],[],[happy_var_4-1,happy_var_7-1,happy_var_10-1])
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 16 8 happyReduction_12
happyReduction_12 ((HappyTerminal (L.TInt happy_var_16)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.TInt happy_var_14)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.TInt happy_var_12)) `HappyStk`
	(HappyTerminal (L.TInt happy_var_11)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.TInt happy_var_9)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.TInt happy_var_7)) `HappyStk`
	(HappyTerminal (L.TInt happy_var_6)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.TInt happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.TInt happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (([happy_var_2-1,happy_var_7-1,happy_var_12-1],[happy_var_6-1,happy_var_11-1,happy_var_16-1],[happy_var_4-1,happy_var_9-1,happy_var_14-1])
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 13 8 happyReduction_13
happyReduction_13 ((HappyTerminal (L.TInt happy_var_13)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.TInt happy_var_10)) `HappyStk`
	(HappyTerminal (L.TInt happy_var_9)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.TInt happy_var_6)) `HappyStk`
	(HappyTerminal (L.TInt happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (L.TInt happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (([happy_var_2-1,happy_var_6-1,happy_var_10-1],[happy_var_5-1,happy_var_9-1,happy_var_13-1],[])
	) `HappyStk` happyRest

happyReduce_14 = happySpecReduce_1  9 happyReduction_14
happyReduction_14 (HappyTerminal (L.Object happy_var_1))
	 =  HappyAbsSyn9
		 (emptyObj happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  9 happyReduction_15
happyReduction_15 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1{groups=happy_var_2:groups happy_var_1}
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  10 happyReduction_16
happyReduction_16 (HappyTerminal (L.TFloat happy_var_1))
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  10 happyReduction_17
happyReduction_17 (HappyTerminal (L.TInt happy_var_1))
	 =  HappyAbsSyn10
		 (fromIntegral happy_var_1 :: GLfloat
	)
happyReduction_17 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 20 20 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	L.Object happy_dollar_dollar -> cont 11;
	L.Group happy_dollar_dollar -> cont 12;
	L.Vertex -> cont 13;
	L.VertexTexture -> cont 14;
	L.VertexNormal -> cont 15;
	L.Face -> cont 16;
	L.TFloat happy_dollar_dollar -> cont 17;
	L.TInt happy_dollar_dollar -> cont 18;
	L.TDiv -> cont 19;
	_ -> happyError' (tk:tks)
	}

happyError_ 20 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(L.Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parseObj tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


--errStrPut = hPutStrLn stderr

--printError = lift.errStrPut

--sprint = lift.putStrLn

iindices l = foldr help [] l
  where
    help (x,_,_) acc = x++acc

vindices l = foldr help [] l
  where
    help (_,x,_) acc = x++acc

tindices l = foldr help [] l
  where
    help (_,_,x) acc = x++acc

parseError :: [L.Token] -> b
parseError t = error ("Parse error: " ++ show t)
{-# LINE 1 "templates\GenericTemplate.hs" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "G:\\GitHub\\haskell-platform\\build\\ghc-bindist\\local\\lib/include\\ghcversion.h" #-}

















{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "C:\\Users\\randy\\AppData\\Local\\Temp\\ghc4548_0\\ghc_2.h" #-}
















































































































































{-# LINE 9 "<command-line>" #-}
{-# LINE 1 "templates\\GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates\\GenericTemplate.hs" #-}

{-# LINE 46 "templates\\GenericTemplate.hs" #-}








{-# LINE 67 "templates\\GenericTemplate.hs" #-}

{-# LINE 77 "templates\\GenericTemplate.hs" #-}

{-# LINE 86 "templates\\GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates\\GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
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
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates\\GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates\\GenericTemplate.hs" #-}
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
