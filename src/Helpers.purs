module Helpers (classes) where

import Prelude
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

classes :: forall r t. Array String -> HH.IProp ( "class" :: String | r ) t
classes = HP.classes <<< (HH.ClassName <$> _)
