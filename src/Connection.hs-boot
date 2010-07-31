module Connection (Connection) 
where 

import Xmpp
import Control.Concurrent
import Control.Concurrent.STM

data ElementType = Preamble
                 | StreamStart
                 | Any
--                 deriving (Eq, Show)

data Response = Expect ElementType
              | Pipeline Bool
              | Quit
              | Ok
--              deriving (Show)

type ResponseVar = MVar [Response]

data ConnMsg = StreamStartRx
             | InboundXmpp Stanza ResponseVar
             | OutboundXmpp Stanza
             | ConnectionLost
             | ProtocolError
             | Close

type ConnMsgQ = TChan ConnMsg

data Connection = Conn Integer ConnMsgQ