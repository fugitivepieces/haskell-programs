import Parser
import Control.Applicative

comment :: Parser ()
comment = do symbol "--"
             many (sat (/= '\n'))
             char '\n'
             return ()
