.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

.. highlight:: haskell

Private Dependencies
==============

Private dependencies allow soundly allow adding new deps without breaking any downstream build plan.
This is especially useful for GHC, so it can begin leveraging the ecosystem at large.

Motivation
----------

The Haskell ecosystem is very good at code reuse---proof of the language's capability for writing robust, widely-applicable abstractions.
But a downside remains in that more dependencies increase the risk of a version conflict, because only one version of a library can currently be used in a build plan.
This is especially bad for popular libraries, as they can end up in a trap where taking on any additional dependency is likely to break a downstream consumer.

For many years, restrictions in GHC meant that lacking multiple versions of the same package could not be installed or linked together.
Now those artificial restrictions have been lifted, but a fundamental problem remains that it is in general unsound to resolve dependencies to different versions.
This is because our current dependencies can be viewed as *public* dependencies---the interface of a package is allowed to vary based on what dependencies were chosen.
Adding a new notion of *private* dependencies whose existing and exact resolution is invisible downstream will solve this.

No package is caught more in the popular library trap than GHC.
Currently, GHC restricts itself to only using a small set of libraries that are installed with it.
This means it must duplicate existing libraries to achieve the same functionally, in order to only depend on that small set.
An example of this would be the `LLVM Bitcode Backend`_ proposal.

.. _`LLVM Bitcode Backend`: https://github.com/ghc-proposals/ghc-proposals/pull/25

By using private dependencies, GHC gains the opportunity to finally do away with this restriction without breakage.
Doing so is good both for GHC, making it more modular, and also good for external libraries, as GHC being one of the largest and most complex Haskell programs makes it an excellent proving ground.

Proposed Change
---------------

Cabal File Syntax
~~~~~~~~~~~~~~~~~

As today, public dependencies are listed under ``build-depends``.
Everywhere a ``build-depends`` stanza exists a ``private-depends`` stanza listing private dependencies is allowed.
The syntax of each stanza type of stanza will be identical.

Cabal Build Plan Solving
~~~~~~~~~~~~~~~~~~~~~~~~

For some preliminary definitions.
The *immediate dependencies* of a package (given a build plan) are those resolving reach entry in ``build-depends`` or ``private-depends``.
In formulas below, we will use ``IPub`` for the relation of immediate public dependencies, and ``IPriv`` for relation of immediate private dependencies.
The *transitive dependencies*, or just plan *dependencies*, of a package are defined by viewing this and the above as relations, and defining this as the transitive closure of the 2above.
A build plan solves chooses dependencies for a set of *root packages* satisfying some criteria.
This can be reformulated as imagining there is an imaginary single *virtual root package*, which has an immediate public dependency on each of the actual root packages.

As today, a valid Cabal build plan must never include multiple versions of the same package in the set of public dependencies of the virtual root package.

.. code-block:: none

   uniqueVersions(IPub*(r,-))

Trivially, this property holds also for each public dependency of the virtual root and its dependencies examined in isolation, since public the dependency relation is transitive.

.. code-block:: none

    ∀ (p ∈ IPub*(r,-)). uniqueVersions(IPub*(r,-))

In fact, we want this constraint on public dependencies to hold for every package in the build plan, even those only reached via private dependencies,

.. code-block:: none

    ∀ (p ∈ (IPub ∪ IPriv)*(r,-)). uniqueVersions(IPub*(p,-))

Finally, we want the public dependencies of immediate private dependencies to agree.
Otherwise, a package wouldn't be able to make much use of its private dependencies!

.. code-block:: none

    ∀ (p ∈ (IPub ∪ IPriv)*(r,-)). uniqueVersions((IPriv ∪ Id) · IPub*)(p,-))

Notice that the only private dependencies we care about for the ``uniqueVersions`` restriction are immediate.
That we never care about transitive private dependencies is what gives private dependencies their utility.
The last formula is implies all the ones before it, and is thus the only one the implementation need worry about.

Language Semantics
~~~~~~~~~~~~~~~~~~

From GHC's perspective, the crux of dealing with multiple packages is deciding when definitions from different packages (and expressions involving them) are equal.
I am going to ignore about any dependent Haskell plans, as I don't believe it is intended that GHC provide proof of the equality of (term-level) functions and downstream reexports of them.
This means we only need to worry about the unification of types.

Since there are no restrictions on transitive private dependencies, it is important that packages cannot observe the resolution or even existence of their transitive closures.
First, lets imagine a restricted Haskell without type classes.
Upholding this principle in such a language is easy and intuitive---every package's public interface must not include types defined in its immediate private dependencies.

What about reexports---does it count if the public interface uses an upstream type that it also itself reexports?
Its perfectly sound to just prohibit this, but allowing this segues into the solution for type classes.
Since similar immediate private deps constraints of different packages (or a private dep of one package and a public dep of another) may or may not resolve to the same package, the only solution is to pretend that they *never* do.
This means that any reexported type must be "freshened", and act as if it was in fact defined in the reexporting package.
Note that this is different than abstracted, in that it is fine to expose constructors.
The only restriction then is any private type mentioned in the constructors must likewise be also exported and freshened.

Now we tackle type classes.
Because type classes instances are, "always exported", due to global coherence constraints, they necessitate the same freshening mechanism as reexports.
Any instance defined in a private dep must never affect downstream packages, so either the type classes or its parameters must be freshened.
But we can only freshen private definitions, so this means we must disallow orphan instances.
[Technically, we could allow orphan instances that refer to at least one public dep that is not a public transitive dep of the virtual root, but this is more complexity, and burdens the package developer with considering all the ways a package might be used as a private dep, rather than simply whether it can used as a private in all cases or none.]
Disallowing orphan instances seems to me at least to be a small, and strictly opt-in, cost for private dependencies.

Drawbacks
---------

- "Freshening" could be confusing, and is the sort of thing that turns out to be subtly unsound.

Alternatives
------------

- A major reasons for dependencies on GHC is to doc tests.
  Technically, the doc testing infrastructure just needs to be able to *load* the tests, not link with them---it is a only a compile-time dep.
  We currently cannot express this very easily, but if we could vastly fewer packages would need a normal dependency on their GHC API.
  Then GHC might be free to take on regular public dependencies without worrying about breakage.

Unresolved Questions
--------------------

- How does this interact with Dependent Haskell?

- How does this interact with Backpack?

- Do safe coercions pose any extra headaches beyond normal type classes?
  (I don't think so but...)
