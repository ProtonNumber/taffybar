-- | This is a simple static image widget, and a polling image widget that
-- updates its contents by calling a callback at a set interval.
module System.Taffybar.Widget.ImageCommandButton
  ( imageCommandButtonNew
  , imageCommandButtonNewFromName
  ) where

import Control.Concurrent ( forkIO, threadDelay )
import qualified Data.Text as T
import Control.Exception as E
import Control.Monad ( forever )
import Control.Monad.IO.Class
import GI.Gtk
import System.Taffybar.Util

-- | Create a new widget that displays a static image
--
-- > iconImageWidgetNew path
--
-- returns a widget with icon at @path@.
imageCommandButtonNew :: MonadIO m => FilePath -> m Widget
imageCommandButtonNew path = liftIO $ imageNewFromFile path >>= putInBox

-- | Create a new widget that displays a static image
--
-- > iconWidgetNewFromName name
--
-- returns a widget with the icon named @name@. Icon
-- names are sourced from the current GTK theme. 
imageCommandButtonNewFromName :: MonadIO m => T.Text -> m Widget
imageCommandButtonNewFromName name = liftIO $ 
  imageNewFromIconName (Just name) (fromIntegral $ fromEnum IconSizeMenu) 
  >>= putInBox

putInBox :: IsWidget child => child -> IO Widget
putInBox icon = do
  box <- boxNew OrientationHorizontal 0
  boxPackStart box icon False False 0
  widgetShowAll box
  toWidget box

