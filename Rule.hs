module Rule where

import SyntaxCIPU
import MHashDynamic
import Data.Word
import Data.Map qualified as M

newtype Ig a = Ig a

data Rule name value tok 
   = Many     (Rule name value tok)
   | Seq      [Rule name value tok]
   | Alt      [Rule name value tok]
   | And      [Rule name value tok]
   | Not      (Rule name value tok)
   | Ignore   (Rule name value tok)
   | Then     (Rule name value tok) (Rule name value tok)
   | ManyThen (Rule name value tok) (Rule name value tok)
   | Apply    (Iso  Dynamic Dynamic) (Rule name value tok)
   | Count    (Lens Dynamic Int) (Rule name value tok) (Rule name value tok)
   | Pure     Dynamic
   | Try      (Rule name value tok)  
   | AnyToken 
   | Token    tok  
   | Range    tok    tok
   | Let      (M.Map name value) (Rule name value tok)
   | Get      name 
   | Set      name (Rule name value tok)  
   | Name     String (Rule name value tok)  
