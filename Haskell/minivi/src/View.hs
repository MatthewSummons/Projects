module View where

import App
import System.IO (hFlush, stdout)
import Text.Printf (printf)
import Util

-- | 'render' is the main view function for the program
render :: App -> IO ()
render app = do
  putStr "\ESC[?25l" -- hide cursor
  case dirty app of
    FFull -> renderContent app *> renderStatus app
    FStatus -> renderStatus app
    FNone -> pure ()
  restoreCursorPos app
  putStr "\ESC[?25h" -- show cursor
  hFlush stdout

-- | 'replaceTabs' replace each tab with a space in a string
replaceTabs :: String -> String
replaceTabs ""          = ""
replaceTabs ('\t' : xs) = ' ' : replaceTabs xs
replaceTabs (x : xs)    = x : replaceTabs xs

-- | 'bufferToContent' renders the buffer at the corret offset
--
-- It converts the buffer to a list of strings with $r-1$ lines, where each line has a length of $c$.
-- It should also replace all the tabs in the string to spaces.
bufferToContent :: App -> [String]
bufferToContent app =
  [ replaceTabs $ textInView line | line <- linesInView]
  where
    (r, c) = termSize app
    (Pos dx dy) = offset app
    -- Content after the view-port offset
    afterOffset :: [String] -> [String]
    afterOffset buf = map (drop dy) (drop dx buf)
    -- Take c chars from line, pad with spaces if shorter
    textInView :: String -> String
    textInView line = take c line ++ spaces
      where spaces = replicate (c - length line) ' '
    linesInView :: [String]
    linesInView = take (r - 1) (afterOffset (buffer app) ++ repeat emptyLine)
      where emptyLine = '~' : replicate (c - 1) ' '



-- | Render the content buffer
renderContent :: App -> IO ()
renderContent app = setCursorPos 0 0 *> mapM_ putStrLn (bufferToContent app)

-- | Render the status bar
renderStatus :: App -> IO ()
renderStatus (App md buf cp rp d m (r, c) f) = do
  let status = case md of
        Normal -> printf "\"%s\" %dL" f (length buf)
        Insert -> "-- Insert --"
        Command c -> printf ":%s" (reverse (replaceTabs c))
        Message m -> "[INFO] " <> m
  setCursorPos (r - 1) 0
  -- show some debug info (cursor and offset positions)
  let dbg = printf "(%d:%d);(%d:%d)" (row cp) (col cp) (row rp) (col rp)
  putStr $ take (c - length dbg) (status <> repeat ' ') <> dbg

-- | Restore console cursor position according to the cursor position stored in App
restoreCursorPos :: App -> IO ()
restoreCursorPos app@(App md buf (Pos x y) rp u m (r, c) f) = case md of
  Command s -> setCursorPos (r - 1) (length s + 1)
  _ -> setCursorPos x y
