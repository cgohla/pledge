# Introduction #

[`pledge(2)`](https://man.openbsd.org/pledge.2) is an OpenBSD system
call that allows a user space process to reduce the set of available
system calls. OpenBSD defines a set of capabilities called
_promises_. The argument to the `pledge` call is a space separated
string containing the names of the promises the process wishes to
retain; any promise not mentioned is disabled. Importantly, once
dropped, a promise can not be reinstated during the lifetime of the
process, the set of promises can however be reduced in successive
steps.

The purpose of this mechanism is to reduce the potential for exploits.

When using this API in C, one has to carefully analyze the system
calls the program will need, to find the minimal required set of
promises. Naturally, the compiler can not help in checking the
minimality or sufficiency of the result.

We present here a type level API in Haskell that can help automate
this process. There are two steps to using it.

# Prototypes #

My first attempt in
[System.OpenBSD.Pledge](https://github.com/cgohla/pledge/blob/main/src/System/OpenBSD/Pledge.hs)
uses the machinery of [graded
monads](https://ncatlab.org/nlab/show/graded+monad) to collect
promises and do a pledge call at the start of the program. This is not
really satisfactory of course.

My second attempt in
[System.OpenBSD.MultiPledge](https://github.com/cgohla/pledge/blob/main/src/System/OpenBSD/MultiPledge.hs)
uses an action presentation of indexed monads and it will make sure to
run pledge before every elementary action, and thus incrementally
decrease promises, always shrinking it to the set needed by the rest
of the program.

See
[multipledge](https://github.com/cgohla/pledge/blob/main/examples/multipledge/Main.hs)
for a simple usage example. Note how after lifting some functions, the
`do` blocks look very ordinary, and all the additional information is
in the type signatures.

# Examples #

[Examples](https://github.com/cgohla/pledge/tree/main/examples)
contains small executables that demonstrate the functioning of
different prototypes.

# Outlook #

Among other things we still need to study what to do in a
multithreaded environment; and how to deal with inversion of control.

# Caveats #
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

As mentioned, for now this package targets OpenBSD specifically. Linux
has a similar facility called
[`seccomp`](https://www.man7.org/linux/man-pages//man2/seccomp.2.html),
which is much more complex, and hence potentially more difficult to
use (correctly).

There is a [compatibility layer](https://justine.lol/pledge/) that can
apparently translate OpenBSD promises to eBPF for seccomp on
Linux. This might be a way to make this package somewhat portable.

Note that this package uses a version of
[`effect-monad`](https://github.com/dorchard/effect-monad) that is as
of yet unavailable on hackage. So if you want to use it, you will have
to make your own fork, and add it to your cabal project file.

Note furthermore that this design has not actually been used in a
serious application.
