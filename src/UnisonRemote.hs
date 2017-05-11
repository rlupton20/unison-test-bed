-- sketch lambda API

-- most of this is a port of the Remote API - skip over that down to START

-- #! idris

-- things needed to typecheck the Remote API definition
data Remote a = MkRemote a
(>>=) : Remote a -> (a -> Remote b) -> Remote b

Task : Type
Name : Type -> Type
Sandbox : Type
Encrypted : Type -> Type
Box : Type -> Type
Durable : Type -> Type
Vector : Type -> Type
Foreign : Type -> Type

Text : Type
Text = String

Boolean : Type
Boolean = Bool

data Optional a = Some a | None

Node : Type
Duration : Type
Number : Type
CPU : Type
Key : Type
Memory : Type
Storage : Type

infixl 0 <|
(<|) : (a -> b) -> a -> b
f <| x = f x

-- copy/paste of the remote API definition with the following changes:
--  - 'type' -> 'data'
--  - '∀ a' removed
--  - invalid characters removed (CPU%, spawn-sandboxed -> spawn_sandboxed etc)
--  - commented out keywords node/ephemeral/durable/foreign
--  - reordering to put data declarations before their uses

-- this is TBD
data Cause = Error Text Node | Completed | Cancelled | Unresponsive Node

-- TBD
data Sandbox = MkSandbox CPU Memory Storage ({a : Type} -> Node -> Remote a -> Remote a)

-- TBD
data DecryptionFailure = WrongKey | AlgorithmMismatch | IntegrityFailure

-- Promote a pure value to `Remote`
Remote.pure : a -> Remote a

-- Sequencing of remote computations
Remote.bind : (a -> Remote b) -> Remote a -> Remote b

-- The current node where the computation is executing
Remote.here : Remote Node

-- Transfer control of remainder of computation to target node
Remote.transfer : Node -> Remote Unit

-- Explicitly fail a computation for the provided reason
Remote.fail : Text -> Remote a

-- Sleep the current computation for the given duration
Remote.sleep : Duration -> Remote Unit

-- Start running a remote computation asynchronously, returning
-- a `Task` value that can be used for supervision
Remote.fork : Remote a -> Remote Task

-- Halt a running task (and any running subtasks) using the provided `Cause`
Task.stop : Cause -> Task -> Remote Unit

-- Obtain the `Cause` that caused a running task to complete
Task.supervise : Task -> Remote (Remote Cause)

-- Create a duration from a number of seconds
Duration.seconds : Number -> Duration

-- Like `Remote.spawn`, but create the node inside a fresh sandbox
Remote.spawn_sandboxed : Sandbox -> Remote Node

-- Like `Remote.spawn-sandboxed`, but use the provided symmetric key
-- to communicate with the returned `Node`
Remote.spawn_sandboxed' : Key -> Sandbox -> Remote Node

-- Create a new node 'in the same location' as the current node, sharing
-- current sandbox resources
Remote.spawn : Remote Node

-- Like `Remote.spawn`, but use the provided symmetric key
-- to communicate with the returned `Node`.
Remote.spawn' : Key -> Remote Node

-- Statically provision a `personal-info : Node`
--node personal-info -- layout block starts here
--  Sandbox 5% 10MB 3GB accept-from

-- Encrypt a value, requires `Remote` since we use random IV / nonce
encrypt : Key -> a -> Remote (Encrypted a)

-- Decrypt a value, or return `None` if key is incorrect
decrypt : Key -> Encrypted a -> Either DecryptionFailure a

-- `Key` is just a symmetric encryption key. We might generate keys via:

AES256.key : Remote Key
Blowfish.key : Remote Key
-- etc

-- Create an ephemeral `Box` on the current node; just a (GUID, Node) at runtime
Box.empty : Remote (Box a)

-- Put a value into the box, or if the box is full,
-- wait until a `Box.take` empties the box.
Box.put : a -> Box a -> Remote Unit

-- Remove and return the value in the box, or if the box is empty,
-- wait until a `Box.put` fills the box.
Box.take : Box a -> Remote a

-- Like `Box.take`, but leaves the value inside the box
Box.read : Box a -> Remote a

-- Read the current value inside the box or return `None` immediately.
-- Also returns a setter which returns `True` if the set was successful.
-- The `set` is successful only if the value inside the box has not
-- otherwise changed since the read, so this can be used to implement
-- "optimistic" atomic modifies.
Box.access : Box a -> Remote (Optional a, a -> Remote Bool)

-- Create a `Name`, which is a typed reference to a node-local value.
Name.make : Remote (Name a)

-- Lookup the node-local value associated with the `Name`.
Name.resolve : Name a -> Remote (Box a)

-- Declare `bob : Name Number` statically. The value bound to
-- the `Name` does not survive node restarting.
--ephemeral name bob : Number

-- Declare `cluster-peers : Name (Vector Node)` statically. The current
-- value of `cluster-peers` survives node restarting.
--durable name cluster-peers : Vector Node

-- Move any value from RAM to local durable storage
Durable.store : a -> Remote (Durable a)

-- Synchronize any value AND ALL TRANSITIVE DEPENDENCIES
-- to local durable storage, returning `True` if the given `Node`
-- has that `Durable a` locally and the sync was successful.
Durable.sync_from : Node -> Durable a -> Remote Boolean

-- Load a durable value into RAM, assuming it exists on the given node
Durable.load_from : Node -> Durable a -> Remote (Optional a)

-- Returns a list of nodes that the Unison runtime believes could
-- successfully `Durable.load-from` or `Durable.sync-from` for the
-- given `Durable`.
Durable.peers : Durable a -> Remote (Vector Node)

-- Declare `my-fn : Foreign (Number -> Remote Number)` statically
-- Bindings for some of these foreign declarations would be done
-- in some implementation-dependent way on Unison node container startup.
--foreign my-fn : Number -> Remote Number

-- Ask the current node if it has a binding for a `Foreign a`
Foreign.ask : Foreign a -> Remote (Optional a)

-- START

interface Submit Permit (Job : Type -> Type) | Permit where

  -- Submit a computation to be run on some arbitrary node in the pool.
  --
  -- Similar to Remote.fork except it returns a `Job a` instead of a Task, and
  -- it takes a Permit argument, representing the compute pool and our right
  -- to use it.
  --
  -- You can think of a `Job a` as a (Task, Box a) - a task representing the
  -- running computation, and a Box on the local node where the result ends up.
  Pool.submit : Permit -> Remote a -> Remote (Job a)

  -- Get the Task representing a submitted job.  Use `Task.supervise` on this
  -- to hear when the job is complete.
  Job.task : Job a -> Task

  -- Get the result of the job (or None if the job failed or hasn't completed
  -- yet.)
  Job.result : Job a -> Remote (Optional a)

interface PoolPublicInterface Pool Permit User (Job : Type -> Type) | Pool where
  user : String -> User

  -- Connect to a compute pool, establish the user's right to use it, and
  -- declare a sandbox representing the maximum amount of resources we are
  -- prepared to use.  Returns a permit with which we can submit compute jobs.
  --
  -- The resource maximum is applied across all jobs submitted against the
  -- permit (not against each job individually).
  --
  -- Similar to Remote.spawn-sandboxed.
  --
  -- (Probably should have a variant that takes a Key for communication.)
  Pool.createPermit : User -> Pool -> Sandbox -> Remote (Optional Permit)

  -- Tell the compute pool what we are no longer willing to resource
  -- compute jobs under this permit.  Kills jobs in progress.
  Pool.revokePermit : Permit -> Remote Unit

namespace Example
  myPics : List Int
  convertToJpg : Int -> Int

  cpu : Int -> CPU
  mem : Int -> Memory
  storage : Int -> Storage

  data Pool = MkPool
  data Permit = MkPermit
  data User = MkUser
  data Job a = MkJob

  PoolPublicInterface Example.Pool Example.Permit Example.User Example.Job where
    user = ?u
    Pool.createPermit = ?cp
    Pool.revokePermit = ?rp

  convertMyPics : PoolPublicInterface Example.Pool Example.Permit Example.User Example.Job => Pool -> Remote (List Int)
  convertMyPics pool = do let sandbox = MkSandbox (cpu 10) (mem 10) (storage 100) ?etc
                          Some permit <- Pool.createPermit (user "Bob") pool sandbox
                              | Nothing => pure ()  -- not real Unison syntax
                          job <- Pool.submit permit (pure $ map convertToJpg myPics)
                          Completed <- Task.supervise (Job.task job)
                              | err => pure ()
                          pure (Job.result job)

namespace trivialPoolExample

  data TrivialPool = MkTrivialPool Node
  data TrivialJob a = MkTrivialJob Task (Box a)
  data TrivialPermit = MkTrivialPermit Node

  Submit (TrivialPermit) (TrivialJob) where
    Pool.submit permit computation = do let MkTrivialPermit node = permit
                                        box <- Box.empty
                                        task <- Remote.fork <| (do
                                          origin <- Remote.here
                                          Remote.transfer node
                                          result <- computation
                                          Remote.transfer origin
                                          Box.put result box)
                                        pure (MkTrivialJob task box)
    Job.task (MkTrivialJob task box) = task
    Job.result (MkTrivialJob task box) = do (result, _) <- (Box.access box)
                                            pure result

  PoolPublicInterface TrivialPool TrivialPermit String TrivialJob where
    user = id
    Pool.createPermit user pool sandbox = let MkTrivialPool poolNode = pool in
                                          do origin <- Remote.here
                                             Remote.transfer poolNode
                                             node <- Remote.spawn_sandboxed sandbox
                                             Remote.transfer origin
                                             pure (Some (MkTrivialPermit node))
    Pool.revokePermit permit = pure ()


-- cluster API footprint of pool; feeding load usage into scale-out decision
-- database API footprint of usage DB   > when to give out permits, when to create sandboxes, where to store them
   -- is DB hard or soft state?  recreate?
-- footprint on user DB
-- permit allows contact in; where is load balancing happening, how does failure redistribution work?  scale out?
  -- if that's just transfer in then how is that sandboxed? a node with sandboxed open transfers?

-- billing  >> would require some sort of Node usage report primitive, and a Node clearup kick
-- transport unreliability  >> submit retries on failure, from the source node
