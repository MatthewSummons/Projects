module App where

import Util

-- | Editor mode
data Mode
  = Normal -- normal mode
  | Insert -- insertion mode
  | Command String -- command mode (with command buffer)
  | Message String -- display message
  deriving (Show, Eq)

-- | Dirty flags
data Flag
  = FNone -- no refresh
  | FStatus -- refresh status bar only
  | FFull -- refresh all
  deriving (Show, Eq)

-- | The model (app state)
data App = App
  { mode :: Mode, -- current editor mode
    buffer :: [String], -- text buffer
    cursor :: Pos, -- cursor position
    offset :: Pos, -- view port offset
    dirty :: Flag, -- flag for refreshing terminal buffer
    modified :: Bool, -- buffer modified
    termSize :: (Int, Int), -- terminal size
    file :: FilePath -- file path
  }
  deriving (Show)

-- | 'newApp' initialize a new app
newApp :: FilePath -> String -> (Int, Int) -> App
newApp path buffer ts =
  App
    { mode = Normal,
      buffer = lines buffer,
      cursor = Pos 0 0,
      offset = Pos 0 0,
      dirty = FFull,
      modified = False,
      termSize = ts,
      file = path
    }

-- | 'contentPos' returns the content cursor position $(i, j)$
--
-- It should be cursor position + offset,
-- so you can either directly index the buffer or "off by one" (useful for editing)
contentPos :: App -> Pos
contentPos (App md buf (Pos x y) (Pos dx dy) u m ts f) = Pos (x + dx) (y + dy)