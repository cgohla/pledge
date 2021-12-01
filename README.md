`pledge(2)` is an OpenBSD system call that allows a user space process
to reduce the set of available system calls. OpenBSD defines a set of
capabilities called _promises_. The argument to the `pledge` call is a
space separated string containing the names of the promises the
process wishes to retain; any promise not mentioned is
disabled. Importantly, once dropped, a promise can not be reinstated
during the lifetime of the process, the set of promises can however be
reduced in successive steps.

The purpose of this mechanism is to reduce the potential for exploits.

When using this API in C, one has to carefully analyze the system
calls the program will need, to find the minimal required set of
promises. Naturally, the compiler can not help in checking the
minimality or sufficiency of the result.

We present here a type level API in Haskell that can help automate
this process. There are two steps to using it.

Consider

```haskell
Data.Text.IO.putStrLn :: Text -> IO ()
```

from the `text` package. This will print the given text to the
standard output, ultimately using the `write(2)` system call. Using
`write(2)` on an already open file (stdout in this case) is allowed
under the `stdio` promise. Using the `Pledge` higher type we can lift
`putStrLn` to a type that tracks the need for this promise as follows:

```haskell
import qualified Data.Text.IO as T (putStrLn)

putStrLn :: Text -> Pledge IO '[ 'Stdio ] ()
putStrLn = Pledge . T.putStrLn
```

If we have another `IO` action, say, to list the contents of a
directory (which requires the `rpath` promise, to enable reading from
the file system):

```haskell
import qualified System.Directory as D (getDirectoryContents)

getDirectoryContents :: FilePath -> Pledge IO '[ 'Rpath ] [FilePath]
getDirectoryContents = Pledge . D.getDirectoryContents

```

we need a way to combine the two. But in doing that, the promise
annotations should be joined, since clearly, using these two actions
will require both promises. Usually we might combine `IO` actions
using a `do` block (which desugars to applications of the monadic
`>>=` operator):

```haskell
main = do
	putStrLn "Hello"
	fs <- getDirectoryContents "."
	traverse_ putStrLn fs
```

However, `Pledge IO` is not a monad, but what's called a _graded
monad_, (graded over the `'[ Promise ]` data kind in this case, which
is a type level monoid). The `Effect` type class from the
`effect-monad` package provides an interface for graded monads. This
type class conveniently defines a `>>=` operator, and with the help of
the `RebindableSyntax` extension we can actually get GHC to desugar a
`do` block into applications of `Control.Effect.>>=`. So the above
`do` block can be written using the `Pledge` annotated actions, and
yield:

```haskell
main :: Pledge IO '[ 'Rpath, 'Stdio] ()
```

The second step consists in discharging the accumulated promises,
apply them and run the actions. This is done using :

```haskell
runPledge :: (CollectPromise ps) => Pledge IO ps a -> IO a
```

All uses of `IO` need to be lifted to the `Pledge` type. This presents
us with the question of which level to define these annotations at;
i.e., one could make wrapper packages annotating and reexporting the
entire API of a given package using `IO`, such as `base` or `text`.

One can define the annotations of just the functions being used in an
application. This would be more ad-hoc, but is probably the easiest
way to get started.

Lastly, one could dig down into `base`, annotate the basic FFI calls
and propagate that type information upwards to the exposed API. This
would amount to a substantial rewrite of `base`.

We emphasize that the correctness (i.e., minimality and sufficiency)
of the promise set for the whole program depends on correctly
annotating the constituent actions.
