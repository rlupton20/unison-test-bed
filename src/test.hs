import Control.Applicative
import UnisonRemote

example :: Node -> Remote ()
example node2 = do
    node1 <- remote_here
    remote_transfer node2
    remote_sleep $ duration_seconds 5
    remote_transfer node1
    pure ()
