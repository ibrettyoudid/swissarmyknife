module Rule where

import SyntaxCIPU
import MHashDynamic
import Data.Word

newtype Ig a = Ig a

data Rule tok =
   Many       (Rule tok)
   | Seq      [Rule tok]
   | Alt      [Rule tok]
   | And      [Rule tok]
   | Not      (Rule tok)
   | Then     (Rule tok) (Rule tok)
   | ManyThen (Rule tok) (Rule tok)
   | Apply    (Iso  Dynamic Dynamic) (Rule tok)
   | Count    (Lens Dynamic Int) (Rule tok) (Rule tok)
   | Pure     Dynamic
   | Try      (Rule tok)  
   | AnyToken 
   | Token    tok  
   | Range    tok    tok
   | Get      String 
   | Set      String (Rule tok)  
   | Name     String (Rule tok)  
