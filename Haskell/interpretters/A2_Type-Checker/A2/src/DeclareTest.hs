module DeclareTest where
import Parser

-- due to cyclic module dependency limitation, so tests have to be moved into a separate file
-- |
-- >>> parseType "{a:Int,b:[{c:{},d:Bool}]}"
-- Right { a: Int, b: [{ c: {  }, d: Bool }] }
--
-- >>> parseExpr "[[1,2]]!!0!!(0+1) + 2*{a=1,b=2}.b"
-- Right ([[1, 2]] !! (0) !! (0 + 1)) + 2 * ({ a = 1, b = 2 }.b)
