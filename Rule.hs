module Rule where

import SyntaxCIPU
import MHashDynamic
import Data.Word

newtype Ig a = Ig a

data Rule name tok =
   Many       (Rule name tok)
   | Seq      [Rule name tok]
   | Alt      [Rule name tok]
   | And      [Rule name tok]
   | Not      (Rule name tok)
   | Then     (Rule name tok) (Rule name tok)
   | ManyThen (Rule name tok) (Rule name tok)
   | Apply    (Iso  Dynamic Dynamic) (Rule name tok)
   | Count    (Lens Dynamic Int) (Rule name tok) (Rule name tok)
   | Pure     Dynamic
   | Try      (Rule name tok)  
   | AnyToken 
   | Token    tok  
   | Range    tok    tok
   | Let      name
   | Get      name 
   | Set      name (Rule name tok)  
   | Name     String (Rule name tok)  
