module Util (class_) where

import AppPrelude
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML (ClassName(..))

class_ :: forall r t. Array String -> HH.IProp ( "class" :: String | r ) t
class_ = HP.classes <<< map ClassName

