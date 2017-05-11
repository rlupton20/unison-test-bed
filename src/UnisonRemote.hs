-- things needed to typecheck the Remote API definition
data Remote a = MkRemote a
(>>=) :: Remote a -> (a -> Remote b) -> Remote b
(>>=) = undefined

type Task = ()
type Name a = ()
type Encrypted a = ()
type Box a = ()
type Durable a = ()
type Vector a = ()
type Foreign a = ()

type Text = String

type Boolean = Bool
type Unit = ()

data Optional a = Some a | None

type Node = ()
type Duration = ()
type Number = ()
type CPU = ()
type Key = ()
type Memory = ()
type Storage = ()

infixl 0 <|
(<|) :: (a -> b) -> a -> b
f <| x = f x

-- copy/paste of the remote API definition with the following changes::
--  - 'type' -> 'data'
--  - '∀ a' removed
--  - invalid characters removed (CPU%, spawn-sandboxed -> spawn_sandboxed etc)
--  - commented out keywords node/ephemeral/durable/foreign
--  - reordering to put data declarations before their uses

-- this is TBD
data Cause = Error Text Node | Completed | Cancelled | Unresponsive Node

-- TBD
data Sandbox = MkSandbox CPU Memory Storage ()

-- TBD
data DecryptionFailure = WrongKey | AlgorithmMismatch | IntegrityFailure

-- Promote a pure value to `Remote`
remote_pure :: a -> Remote a
remote_pure = undefined

-- Sequencing of remote computations
remote_bind :: (a -> Remote b) -> Remote a -> Remote b
remote_bind = undefined

-- The current node where the computation is executing
remote_here :: Remote Node
remote_here = undefined

-- Transfer control of remainder of computation to target node
remote_transfer :: Node -> Remote Unit
remote_transfer = undefined

-- Explicitly fail a computation for the provided reason
remote_fail :: Text -> Remote a
remote_fail = undefined

-- Sleep the current computation for the given duration
remote_sleep :: Duration -> Remote Unit
remote_sleep = undefined

-- Start running a remote computation asynchronously, returning
-- a `Task` value that can be used for supervision
remote_fork :: Remote a -> Remote Task
remote_fork = undefined

-- Halt a running task (and any running subtasks) using the provided `Cause`
task_stop :: Cause -> Task -> Remote Unit
task_stop = undefined

-- Obtain the `Cause` that caused a running task to complete
task_supervise :: Task -> Remote (Remote Cause)
task_supervise = undefined

-- Create a duration from a number of seconds
duration_seconds :: Number -> Duration
duration_seconds = undefined

-- Like `remote_spawn`, but create the node inside a fresh sandbox
remote_spawn_sandboxed :: Sandbox -> Remote Node
remote_spawn_sandboxed = undefined

-- Like `remote_spawn-sandboxed`, but use the provided symmetric key
-- to communicate with the returned `Node`
remote_spawn_sandboxed' :: Key -> Sandbox -> Remote Node
remote_spawn_sandboxed' = undefined

-- Create a new node 'in the same location' as the current node, sharing
-- current sandbox resources
remote_spawn :: Remote Node
remote_spawn = undefined

-- Like `remote_spawn`, but use the provided symmetric key
-- to communicate with the returned `Node`.
remote_spawn' :: Key -> Remote Node
remote_spawn' = undefined

-- Statically provision a `personal-info :: Node`
--node personal-info -- layout block starts here
--  Sandbox 5% 10MB 3GB accept-from

-- Encrypt a value, requires `Remote` since we use random IV / nonce
encrypt :: Key -> a -> Remote (Encrypted a)
encrypt = undefined

-- Decrypt a value, or return `None` if key is incorrect
decrypt :: Key -> Encrypted a -> Either DecryptionFailure a
decrypt = undefined

-- `Key` is just a symmetric encryption key. We might generate keys via::

aes256_key :: Remote Key
aes256_key = undefined

blowfish_key :: Remote Key
blowfish_key = undefined
-- etc

-- Create an ephemeral `Box` on the current node; just a (GUID, Node) at runtime
box_empty :: Remote (Box a)
box_empty = undefined

-- Put a value into the box, or if the box is full,
-- wait until a `box_take` empties the box_
box_put :: a -> Box a -> Remote Unit
box_put = undefined

-- Remove and return the value in the box, or if the box is empty,
-- wait until a `box_put` fills the box_
box_take :: Box a -> Remote a
box_take = undefined

-- Like `box_take`, but leaves the value inside the box
box_read :: Box a -> Remote a
box_read = undefined

-- Read the current value inside the box or return `None` immediately.
-- Also returns a setter which returns `True` if the set was successful.
-- The `set` is successful only if the value inside the box has not
-- otherwise changed since the read, so this can be used to implement
-- "optimistic" atomic modifies.
box_access :: Box a -> Remote (Optional a, a -> Remote Bool)
box_access = undefined

-- Create a `Name`, which is a typed reference to a node-local value.
name_make :: Remote (Name a)
name_make = undefined

-- Lookup the node-local value associated with the `Name`.
name_resolve :: Name a -> Remote (Box a)
name_resolve = undefined

-- Declare `bob :: Name Number` statically. The value bound to
-- the `Name` does not survive node restarting.
--ephemeral name bob :: Number

-- Declare `cluster-peers :: Name (Vector Node)` statically. The current
-- value of `cluster-peers` survives node restarting.
--durable name cluster-peers :: Vector Node

-- Move any value from RAM to local durable storage
durable_store :: a -> Remote (Durable a)
durable_store = undefined

-- Synchronize any value AND ALL TRANSITIVE DEPENDENCIES
-- to local durable storage, returning `True` if the given `Node`
-- has that `Durable a` locally and the sync was successful.
durable_sync_from :: Node -> Durable a -> Remote Boolean
durable_sync_from = undefined

-- Load a durable value into RAM, assuming it exists on the given node
durable_load_from :: Node -> Durable a -> Remote (Optional a)
durable_load_from = undefined

-- Returns a list of nodes that the Unison runtime believes could
-- successfully `durable_load-from` or `durable_sync-from` for the
-- given `Durable`.
durable_peers :: Durable a -> Remote (Vector Node)
durable_peers = undefined

-- Declare `my-fn :: Foreign (Number -> Remote Number)` statically
-- Bindings for some of these foreign declarations would be done
-- in some implementation-dependent way on Unison node container startup.
--foreign my-fn :: Number -> Remote Number

-- Ask the current node if it has a binding for a `Foreign a`
foreign_ask :: Foreign a -> Remote (Optional a)
foreign_ask = undefined
