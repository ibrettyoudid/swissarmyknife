module HTTPTypes where

import Parser6Types
import NewTuple


data ProtocolK      = ProtocolK      deriving (Eq, Ord, Show)
data HostK          = HostK          deriving (Eq, Ord, Show)
data PortK          = PortK          deriving (Eq, Ord, Show)
data AbsPathK       = AbsPathK       deriving (Eq, Ord, Show)   
data QueryK         = QueryK         deriving (Eq, Ord, Show) 
data MethodK        = MethodK        deriving (Eq, Ord, Show)  
data UrlK           = UrlK           deriving (Eq, Ord, Show)
data HeadersK       = HeadersK       deriving (Eq, Ord, Show)   
data Headers1K      = Headers1K      deriving (Eq, Ord, Show)    
data BodyK          = BodyK          deriving (Eq, Ord, Show)
data HeaderNameK    = HeaderNameK    deriving (Eq, Ord, Show)      
data ValueK         = ValueK         deriving (Eq, Ord, Show) 
data ContentLengthK = ContentLengthK deriving (Eq, Ord, Show)         
data HTTPVersionK   = HTTPVersionK   deriving (Eq, Ord, Show)       
data StatusCodeK    = StatusCodeK    deriving (Eq, Ord, Show)      
data ReasonPhraseK  = ReasonPhraseK  deriving (Eq, Ord, Show)        

data Url = Url { protocol :: String, host :: String, port :: Maybe Int, absPath :: [String], query :: Maybe String, httpVersion1 :: String } deriving (Eq, Ord, Show)
data Message = Message { method :: String, url :: Url, httpVersion :: String, statusCode :: Int, reasonPhrase :: String, headers :: [(String :- String)], contentLength :: Int, body :: String } deriving (Eq, Ord, Show)

data HVar = HVar deriving (Eq, Ord, Show)

instance Frame ProtocolK String Url where
   myget1 ProtocolK = protocol
   myset1 ProtocolK value frame = frame { protocol = value }

instance Frame HostK String Url where
   myget1 HostK = host
   myset1 HostK value frame = frame { host = value }

instance Frame PortK (Maybe Int) Url where
   myget1 PortK = port
   myset1 PortK value frame = frame { port = value }

instance Frame AbsPathK [String] Url where
   myget1 AbsPathK = absPath
   myset1 AbsPathK value frame = frame { absPath = value }

instance Frame QueryK (Maybe String) Url where
   myget1 _ = query
   myset1 _ value frame = frame { query = value }

instance Frame HTTPVersionK String Url where
   myget1 _ = httpVersion1
   myset1 _ value frame = frame { httpVersion1 = value }

instance Frame MethodK String Message where
   myget1 MethodK = method
   myset1 MethodK value frame = frame { method = value }

instance Frame UrlK Url Message where
   myget1 UrlK = url
   myset1 UrlK value frame = frame { url = value }

instance Frame StatusCodeK Int Message where
   myget1 StatusCodeK = statusCode
   myset1 StatusCodeK value frame = frame { statusCode = value }

instance Frame ReasonPhraseK String Message where
   myget1 ReasonPhraseK = reasonPhrase
   myset1 ReasonPhraseK value frame = frame { reasonPhrase = value }

instance Frame HTTPVersionK String Message where
   myget1 HTTPVersionK = httpVersion
   myset1 HTTPVersionK value frame = frame { httpVersion = value }

instance Frame HeadersK [String :- String] Message where
   myget1 HeadersK = headers
   myset1 HeadersK value frame = frame { headers = value }

instance Frame ContentLengthK Int Message where
   myget1 _ = contentLength
   myset1 _ value frame = frame { contentLength = value }

instance Frame BodyK String Message where
   myget1 BodyK = body
   myset1 BodyK value frame = frame { body = value }
