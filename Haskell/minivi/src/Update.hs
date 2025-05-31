module Update where

import App
import System.Exit (exitSuccess)
import Util

-- | 'update' is the main update function for the program
update :: App -> Key -> IO App
update app k = case mode app of
  Normal -> pure $ handleNormal app k
  Insert -> pure $ handleInsert app k
  Command s -> handleCommand app s k
  Message m -> pure $ handleNormal (app {mode = Normal, dirty = FStatus}) k

-- | 'moveCursor' moves cursor by 1 to the corresponding direction
moveCursor :: App -> Dir -> App
moveCursor app d =
  updateCursor app $ case d of
    DUp -> Pos (x - 1) y
    DDown -> Pos (x + 1) y
    DLeft -> Pos x (y - 1)
    DRight -> Pos x (y + 1)
  where
    Pos x y = contentPos app

-- | 'updateCursor'
--
-- given the new content cursor position, update cursor position and offset
updateCursor :: App -> Pos -> App
updateCursor app pos = 
  app {cursor = Pos vx vy, offset = Pos x_offset y_offset, dirty = dirtyFlag}
  where
    Pos dx dy  = offset app
    Pos i  j   = pos
    (r, c)     = termSize app
    
    -- Reserve one row for status bar
    usableRows = r - 1
    
    -- Calculate the bounds of the buffer
    numLines = length (buffer app)
    lineLength
      | i < numLines && i >= 0 = length (buffer app !! i)
      | otherwise              = 0
                 
    -- Clamp the target position to valid bounds
    i' = max 0 (min (numLines - 1) i)
    j' = max 0 (min lineLength j)
    
    -- Calculate new cursor and offset values
    -- For x-direction (rows)
    x_offset
      | i' < dx           = i'     -- Can't scroll above the first line
      | i' >= dx + usableRows =    
        i' - usableRows + 1        -- Scroll down when cursor would go below screen
      | otherwise         = dx     -- Keep current scroll position
      
    -- For y-direction (columns)
    y_offset
      | j' < dy      = j'          -- Can't scroll left of first column
      | j' >= dy + c = j' - c + 1  -- Scroll right when cursor would go off screen
      | otherwise    = dy          -- Keep current scroll position
    
    -- Calculate screen cursor position from content position and offset
    vx = min (i' - x_offset) (usableRows - 1)  -- Ensure cursor doesn't go to status bar
    vy = j' - y_offset
    
    -- Determine the dirty flag
    offsetChanged = x_offset /= dx || y_offset /= dy
    dirtyFlag
      | offsetChanged || dirty app == FFull = FFull
      | otherwise                           = FStatus

-- | 'handleNormal' handles update for normal mode
handleNormal :: App -> Key -> App
handleNormal app k = case k of
  KArrow d -> moveCursor app d
  KChar 'h' -> moveCursor app DLeft
  KChar 'j' -> moveCursor app DDown
  KChar 'k' -> moveCursor app DUp
  KChar 'l' -> moveCursor app DRight
  KChar 'i' -> app {mode = Insert, dirty = FStatus}
  KChar ':' -> app {mode = Command "", dirty = FStatus}
  _ -> app {dirty = FNone} -- ignore other key inputs in normal mode

-- 'handleInsert' handles update for insert mode
handleInsert :: App -> Key -> App
handleInsert app k = case k of
  KEsc -> app {mode = Normal, dirty = FStatus}
  KArrow d -> moveCursor app d
  KDel ->
    let (buf', pos') = bufDel (contentPos app) (buffer app)
     in updateCursor (app {buffer = buf', dirty = FFull, modified = True}) pos'
  KChar c ->
    -- mover cursor by 1 to the right after inserting a character
    moveCursor (app {buffer = bufIns c (contentPos app) (buffer app), dirty = FFull, modified = True}) DRight
  KRet ->
    let (buf', pos') = bufRet (contentPos app) (buffer app)
     in updateCursor (app {buffer = buf', dirty = FFull, modified = True}) pos'

-- | 'bufIns' handles insertion of a single character
--
-- Given the character to be inserted and buffer, return the updated buffer
bufIns :: Char -> Pos -> [String] -> [String]
bufIns c (Pos x y) buf = case splitAt x buf of
  (before, [])           -> before ++ [c : ""]
  (before, line : after) -> before ++ line'' : after 
    where
      (line', after') = splitAt y line
      line'' = line' ++ c : after'

-- | 'bufDel' handles delete (backspace)
--
-- Given content cursor position and buffer, return the buffer and updated content cursor position
bufDel :: Pos -> [String] -> ([String], Pos)
bufDel (Pos x y) buf = case splitAt x buf of  
  (above, line : below) -> case splitAt y line of
    -- Cursor at the start of a line
    ([], rest) -> case splitAt (length above - 1 ) above of
      -- No line above
      (pre, []) -> (buf, Pos x y)
      -- Line above exists
      (pre, [lastLine]) -> (newBuf, Pos (x - 1) (length lastLine))
        where newBuf = pre ++ ((lastLine ++ rest) : below)
      _ -> error "Error [bufDel] -> Impossible inputs"
    -- Cursor in the middle of a line
    (leftLine, rightLine) -> (above ++ (modifiedLine : below), Pos x (y-1))
      where modifiedLine = init leftLine ++ rightLine
  (above, []) -> error "Error [bufDel] -> Malformed cursor position"

-- | 'bufRet' handles return
--
-- Given content cursor position and buffer, return the buffer and updated content cursor position
bufRet :: Pos -> [String] -> ([String], Pos)
bufRet (Pos x y) buf = case splitAt x buf of
  (above, line : below) -> (newBuf, newPos)
    where
      (left, right) = splitAt y line
      newBuf = above ++ (left : right : below)
      newPos = Pos (x + 1) 0
  _ -> error "Error [bufRet] -> Malformed cursor position"

-- | 'handleCommand' handles update for command mode
handleCommand :: App -> String -> Key -> IO App
handleCommand app s k = case (s, k) of
  (_, KEsc) -> pure app {mode = Normal, dirty = FStatus}
  (s, KDel) -> pure $ case s of
    [] -> app {mode = Normal, dirty = FStatus}
    (_ : s') -> app {mode = Command s', dirty = FStatus}
  (s, KChar c) -> pure app {mode = Command (c : s), dirty = FStatus}
  (s, KRet) -> case reverse s of
    ['q']  -> if modified app
      then pure app {
        mode = Message "Buffer modified, use ! to force quit", dirty = FStatus
      }
      else exitSuccess
    "q!" -> exitSuccess
    -- Check if filepath is null (happens when new file created)
    ['w']  -> if null (file app)
      then 
        pure app {mode = Message "No file name specified", dirty = FStatus}
      else do
        writeFile (file app) (unlines $ buffer app)
        pure app {mode = Message "File Written", dirty = FStatus, modified = False}
    -- Writing content to a file
    ('w':' ':filename) -> do
      writeFile filename (unlines $ buffer app)
      pure app {
        mode = Message "File Written", dirty = FStatus,
        modified = False, file = filename
      }
    _ -> pure app {mode = Message "Unrecognized command", dirty = FStatus}
  _ -> pure app {dirty = FNone}