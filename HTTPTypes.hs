module HTTPTypes where

data HVar = Protocol | Host | Port | AbsPath | Query | Method | Url | Headers | Headers1 | Body | HeaderName | Value | ContentLength | HTTPVersion | StatusCode | ReasonPhrase deriving (Eq, Ord, Show)