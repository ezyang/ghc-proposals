.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

Backpack (implementation)
=========================

This is a companion document to the `Backpack
<https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>`_
proposal describing how backpack is implemented in GHC, Cabal, and
cabal-install.

GHC
---

The most recent patchset for GHC with Backpack is located at
the `ghc-backpack branch on ezyang/ghc <https://github.com/ezyang/ghc/tree/ghc-backpack>`_.

Overview
~~~~~~~~

At a high level, there are two modes GHC can run in:

1. It can **typecheck an uninstantiated library**, during which
   it first typechecks some unimplemented signatures, which
   we then typecheck the rest of the modules (and libraries)
   against.

2. It can **compile an instantiated library**, during which
   it compiles modules against a known set of implementations
   (specified by the instantiation.)

Before typechecking an uninstantiated library, we must fulfill
the following precondition:

* Every dependency on a definite library must be compiled
  and registered in the installed
  library database.

* Every dependency on an indefinite library (even if it is
  fully instantiated) must be have been typechecked as a fully
  uninstantiated library (we never typecheck partially instantiated
  libraries; instead, their interfaces are computed on-the-fly) and
  registered as an uninstantiated, indefinite library in the installed
  library database.

Given that these hold, to typecheck an uninstantiated library GHC must
first typecheck the signatures and dependencies.  This is a fairly major
operation (which we will describe in more detail later), but at the end
we have the following guarantees:

* Every required signature of the library being typechecked has
  an ``hi`` interface file written for it.  (Thus, the process of
  type checking an import of a signature in a module works *exactly* the
  same way as an import of a module.)

* Every dependency has been verified to be well-typed, in the sense
  that the implementation of every requirement (possibly containing references
  to the local required signatures of our library, think
  ``-unit-id p[A=<A>]``) *matches* the required signature of the
  dependency (in the sense described in the Backpack specification
  of signatures.)

At the end of typechecking all signatures and modules in an
uninstantiated, indefinite library, we install these interfaces so that
they can be used to help typecheck other indefinite libraries.

Compiling an instantiated library is a much simpler matter. Given that:

* Every instantiated dependency has been compiled and registered
  in the installed library database.

* The uninstantiated version of the library *we are instantiating*
  has been typechecked and registered in the installed library database.

Then GHC simply verifies that the implementations fulfill the
required signatures of the package (read off from the uninstantiated
version), writes out a trivial ``hi`` file which simply reexports
all of the necessary entities, and then compiles the rest of the modules
as normal.

The details of how this is all done are the subject of the rest
of this document.

Alternate designs:

Don't require the indefinite library to have been typechecked before compiling it
    One observation you could make about Backpack is that the
    typechecking products of an indefinite library aren't really used
    for anything, besides checking that the implementations really do
    match the required signatures.  Since these products are not used
    in an essential way, one might seek to avoid requiring a user to
    have typechecked and registered the uninstantiated variant of
    a package.

    For a period of time, our implementation did precisely this. It is
    only necessary, when compiling, to retypecheck and remerge the
    ``hsig`` to generate the necessary ``hi`` files for compiling
    the rest of the modules.  This leads to some extra complexity
    in the renamer of GHC.

    However, one could reasonably say that uninstantiated libraries
    would play an essential role in compiling mutually recursive libraries
    (since we need to typecheck and build against *something* to break
    the loop), so it is not a big deal to require it always.

Compile instantiated dependencies on indefinite libraries?
    A reasonable, stronger precondition to typechecking an indefinite
    library is to require all fully instantiated dependencies to
    be compiled.  GHC would not derive much benefit from this
    precondition, except that it could avoid renaming interfaces
    on the fly in some cases; thus, I opted for the weaker precondition.

Identifiers (`Module <https://github.com/ezyang/ghc/blob/ghc-backpack/compiler/basicTypes/Module.hs>`_)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The core data types which represent unit identifiers and module
identifiers in GHC have been adjusted to encode the extra structure
imposed by Backpack::

    newtype ComponentId = ComponentId FastString
    type ShFreeHoles = UniqDSet ModuleName
    data UnitId = UnitId {
            unitIdFS          :: FastString,
            unitIdKey         :: Unique,
            unitIdComponentId :: ComponentId,
            unitIdInsts       :: [(ModuleName, Module)],
            unitIdFreeHoles   :: ShFreeHoles
        }
    data Module = Module {
            moduleUnitId :: UnitId,
            moduleName   :: ModuleName
        }

These types closely resemble their `semantic counterparts <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst#identifiers>`_, except for one
difference: there is no distinct ADT representing module variables.
Instead, module variables are representing using a distinguished
``hole`` unit identifier ``holeUnitId``.  This is mostly for backwards
compatibility in GHC, as ``moduleUnitId`` is used pervasively throughout
the compiler (and adding an extra case for module variables would
necessarily make this function partial.)  Similarly, name variables
``{m.n}`` are represented as ``<m>.n`` to avoid adding another case
to the ``Name`` constructor.

This punning requires some care when defining substitutions on
module variables.  Module variables get two substituting functions
which should not be called on ``nameModule`` (except in special
circumstances involving signature merging)::

    renameHoleModule :: ShHoleSubst -> Module -> Module
    renameHoleUnitId :: ShHoleSusbt -> UnitId -> UnitId

Some other things to note about the representation:

* A string representation of a ``UnitId`` (``unitIdFS``) is needed to be
  used for symbol names.  We generate this string representation by
  recursively hashing the contents of a ``UnitId`` (the hashes of sub
  ``UnitId``\s is hashed Merkel tree style):  this algorithm implemented
  by ``hashUnitId``.

* We also need a ``Unique`` (``unitIdKey``) to support fast equality.
  We derive the ``Unique`` from the string representation
  (``unitIdFS``).

* We cache the free module variables (``unitIdFreeHoles``) since we
  frequently need to consult this field, and would like to avoid
  having to walk the entire ``UnitId`` structure to find it.

Alternative designs:

Directly allocate uniques for unit identifiers
    To compute the ``Unique`` for a ``UnitId``, we have to hash
    the unit identifier and then intern that string.  We could intern
    unit identifiers more directly by recording them in a trie
    (ala ``TrieMap``).  However, it's unclear if this would be a
    performance win.

Defer hashing to Cabal
    Cabal must also be able to hash a ``UnitId`` into a flat string,
    which it uses for file system paths.  In the current implementation,
    Cabal and GHC implement these hashing algorithms separately, so
    there is not necessarily any correspondence between Cabal's hash
    and GHC's hash.  An alternative design would be to request Cabal
    to allocate a hash for every definite unit which it compiles
    (e.g., through a flag ``-this-unit-id-hash``).  Occurrences of
    unit identifiers in definite units in the installed unit database
    would be obligated to also record this hash.

    Unfortunately, even under this scheme, Cabal's provided hash cannot be
    used to allocate uniques for equality testing: what if we check
    for equality between an identifier equipped with a hash, and one
    without it?  See below for more on how to avoid this problem.

Hashed unit identifiers
    The current design represents ``UnitId``\s as a tree data structure
    in all situations.  It would be nice to avoid loading these trees
    into memory when they are not necessary, e.g., when compiling
    a definite library (where we do not ever need to perform
    substitutions over the unit identifier); in those cases, we
    simply use the ``Unique`` from the abbreviated unit identifier
    string.

    However, a similar difficulty arises to deferred hashing: what
    if we need to compare an abbreviated unit identifier with a full
    one.  The solution is to always consult the package database
    to promote instantiated unit identifiers into their hashed
    representation, when a substitution over unit identifiers
    is performed.  This substitution must be done even when
    typechecking uninstantiated libraries, since a hashed unit
    identifier may be mentioned by one of our definite dependencies.

Identity modules versus semantic modules
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Suppose that we typecheck the signature file ``A.hsig``, inside the unit
``p[A=<A>]``.  What is its *module identity*?  There are two possible
ways to answer this question:

1. We might say that its module identity is ``p[A=<A>]:A``, since
   module identities for modules are computed by combining the
   current unit identity with the name of the module.  Indeed,
   this module identity uniquely *identifies* the ``A.hi`` produced
   by typechecking ``A.hsig``, thus we call it the **identity module**.

2. Alternately, we might say its module identity is ``<A>``, since
   any entity ``T`` which is declared in this signature should be given
   the original name ``<A>.T`` (recall that by punning, this is really
   the name variable ``{A.T}``).  Since this identity is what would be
   used to compute the original names of entities declared in the
   signature, we call this the **semantic module**.

A semantic module can be computed from an identity module by
a process called **canonicalization** (``canonicalizeModule :: Module ->
Module``).  This distinction influences GHC in the following ways:

* In the desugarer and later phases of the compilation
  pipeline, we can assume semantic and identity modules
  are always the same, since we never compile signatures (to
  appease the build system, we generate blank object files,
  but this is done simply by building a blank stub C file.)

* For any code that involves ``Name``\s, we obviously want
  the semantic module when computing the name.  Examples
  include ``lookupIfaceTop`` in IfaceEnv, ``mkIface`` and
  ``addFingerprints`` in MkIface and ``tcLookupGlobal`` in
  TcEnv.

* When reading interfaces, we want the identity module to
  identify the specific interface we want (such interfaces
  should never be loaded into the EPS).  However, if a
  hole module ``<A>`` is requested, we look for ``A.hi``
  in the *current* unit being compiled.  (See LoadIface.)
  Similarly, in ``RnNames`` we check for self-imports using
  identity modules, to allow signatures to import their implementor.

Name substitutions (`NameShape <https://github.com/ezyang/ghc/blob/ghc-backpack/compiler/backpack/NameShape.hs>`_)
~~~~~~~~~~~~~~~~~~~~~

When we write a declaration in a signature, e.g., ``data T``, we
ascribe to it a **name variable**, e.g., ``{m.T}``.  This
name variable may be substituted with an actual original
name when the signature is implemented (or even if we
merge the signature with one which reexports this entity
from another module).

When we instantiate a signature ``m`` with a module ``M``,
we also need to substitute over names.  To do so, we must
compute the **name substitution** induced by the *exports*
of the module in question.  A ``NameShape`` represents
such a name substitution for a single module instantiation.
The "shape" in the name comes from the fact that the computation
of a name substitution is essentially the *shaping pass* from
Backpack'14, but in a far more restricted form.

The name substitution for an export list is easy to explain.  If we are
filling the module variable ``<m>``, given an export ``N`` of the form
``M.n`` or ``{m'.n}`` (where ``n`` is an ``OccName``), the induced name
substitution is from ``{m.n}`` to ``N``.  So, for example, if we have
``A=impl:B``, and the exports of ``impl:B`` are ``impl:B.f`` and
``impl:C.g``, then our name substitution is ``{A.f}`` to ``impl:B.f``
and ``{A.g}`` to ``impl:C.g``.

The name substitution oriented interface for ``NameShape`` looks
like this::

    emptyNameShape   :: ModuleName -> NameShpe
    mkNameShape      :: ModuleName -> [AvailInfo] -> NameShape
    substNameShape   :: NameShape -> Name -> Name

``mkNameShape req_name as`` says, create a name substitution on
name variables ``{req_name.n}`` for all ``n``, according to the
exports ``as``.

There is a bit more in ``NameShape`` about merging name shapes,
but we will come back to that when we discuss signature merging.

Interface renaming (`RnModIface <https://github.com/ezyang/ghc/blob/ghc-backpack/compiler/backpack/RnModIface.hs>`_)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We only ever write out interface files for uninstantiated unit
identities or fully instantiated units identities.  However, over the
course of typecheck an indefinite unit, we may need to read out the
interface for a partially instantiated unit identifier.  To implement
this, when we request an interface at a unit identifier, we read the
interface for the generalized unit identifier (guaranteed to exist), and
then *rename* it according to the module substitution in the unit
identifier.  Renaming a ``ModIface`` is implemented by ``rnModIface`` in
RnModIface.

Interface renaming proceeds functorially on all occurrences of ``Name``
in ``ModIface``; ``rnIfaceGlobal`` is the workhorse.  The complexity
of this function stems two facts:

1. Morally (for example, we describe name substitutions in our ICFP'16
   submission in this way), the module variable substitution *induce*
   a name variable substitution.  We could go ahead and calculate the
   name variable substitution ahead of time. However, this is wasteful
   if there aren't actually occurrences of any of those name variables in
   the interface.  So we don't actually go and find out what the name
   substitution for a ``ModIface`` is until we actually encounter
   a name variable.

2. There are a two cases where we cannot read the name variable
   substitution directly off of the module substitution.

   The first case is when we are computing the shape of a
   signature prior to merging.  In this case, we might
   need to get the exports of a module ``p[H=himpl:H,A=<B>]:A``.
   ``H=himpl:H`` induces a name substitution on occurrences of
   ``<H>``, but ``A=<B>`` is undefined at this point, since
   we're trying to figure out what the correct exports of ``<B>``
   are going to be!

   The second case is after we have computed the shape of
   a signature we are merging, we need to feed in the correct
   name substitution, rather than load the (non-existent)
   interface for the signature we are type checking.

The algorithm for renaming an original name ``N`` handles all
of these cases as follows:

1. If we are renaming a signature to an indefinite module
   identity (this only occurs if we are about to merge it into the
   local merged signature), the corresponding module substitution
   is guaranteed to have the form ``m=<m'>``.  For a
   name of form ``{m.n}``, we first rename it to ``{m'.n}``,
   and then apply the provided name substitution ``sh_if_shape ::
   NameShape``, if it is provided.

2. For any name of the form ``M.n`` (i.e., not a name variable),
   it is sufficient to apply the module variable substitution to ``M``.

3. Otherwise, for ``{m.n}`` with a substitution from ``m=M``,
   compute the ``NameShape`` from the exports of ``M`` and
   apply it to the name.  (TODO: We never actually construct
   the ``NameShape``; maybe we should!)

The **precondition for interface renaming** is that the domain of all name
substitutions must cover all of the name variables that actually
occur in the interface.  For unit identities which occur from
interface files, this precondition is already fulfilled; however,
user specified unit identities (via ``-unit-id``) can violate this
invariant.  Thus, ``checkImplements`` in TcBackpack ensures this invariant is
upheld (it is called by ``checkUnitId`` and ``instantiateSignature``).
(To be discussed later.)

Note that renaming on ``ModIface`` is necessarily incomplete: top-level
declarations in a ``ModIface`` are identified only by ``OccName``
and cannot be substituted (possible refactor opportunity here). There is
one last renaming step that occurs when typechecking the interface (to
be discussed later) for handling these top level identifiers.

Given that interface typechecking must do renaming as well, why can't
renaming be deferred to typechecking entirely?  Immediate renaming
is extremely useful when merging signatures (to be discussed later),
where we must rename and merge interfaces with different instantiations
prior to typechecking. This algorithm would be very confusing if we
hadn't renamed by then.

Loading interfaces (LoadIface)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Interface renaming is a primitive used by the interface loader,
which asks, given a module identifier, what is the corresponding
``ModIface`` for it.
``computeInterface`` is a new function that handles this logic.
There are two primary code paths:

* The module identifier is fully instantiated, in which case
  (we hope) that there is a **pre-instantiated interface**
  on the file system which we can just read directly.

* The module identifier is partially instantiated (or not
  instantiated at all), in which case we read out the
  interface from the uninstantiated indefinite library,
  and then rename it according to our instantiation:
  an **on-the-fly interface**.

Pre-instantiated interfaces are fairly uninteresting, but there is a
**precondition for on-the-fly interface loading**.  To on-the-fly
load an interface for module ``P:m``, the **free module variables**
of ``P`` must be available, in the sense that there is a local
up-to-date ``hi`` file for each of them in our current project;
furthermore, ``P`` must be well-typed (in the sense that the
implementation matches the required signatures).
(There are more stringent requirements when we read signature interfaces
from our dependencies, but that is not handled by ``loadIface``.)

A big subtlety is what should occur when both the pre-instantiated
interface *as well as* the instantiated on-the-fly interface are
available.  The golden rule is this: **an on-the-fly interface may
be replaced with the compiled interface, but not vice versa!**

* If we are compiling a fully instantiated library, we *always*
  use pre-instantiated interfaces which arose from compilation.

* If we are typechecking an indefinite library, we should prefer
  the compiled interface if possible.  In some situations, this
  is mandatory.  Consider::

    library p
        signature A
    library q
        dependency p[A=<A>]
        signature B
    library a -- definite
        module A
    library r -- definite
        dependency p[A=a:A]
    library s
        dependency q[A=a:A,B=<B>]
        dependency r

  Here, the modules defined in ``r``, a definite library, were compiled against
  the pre-instantiated ``p[A=a:A]``.  Thus, according to the golden
  rule, it would be wrong to use the on-the-fly instantiated module
  (even though ``q`` was typechecked against just that!)

(TODO: the current impl plays fast and loose by just checking if we
can find the definite interface in the database at all, rather than if it
is transitively depended upon by some of our definite dependencies.)

(TODO: dictionary functions...)

Beyond this, there are three other things of note in ``LoadIface``:

1. If we are asked to load the interface for ``<M>`` (e.g., we are
   looking up the ``TyThing`` for ``{M.T}``), where do we
   look?  We should look in the local ``M.hi`` file for
   this module variable!  ``loadInterface`` has a special case for
   this.

2. There is a new function, ``moduleFreeHolesPrecise`` computes the
   *precise* free holes of a ``Module``.  This set is always a subset of
   the free module variables of the ``UnitId`` of the ``Module``.  This
   is used when computing the order we need to merge and typecheck
   signatures (see Signature merging).  This information is cached
   in ``eps_free_holes``.  ``loadInterface`` can't be used here because
   we need to call this function prior to establishing the invariant
   that all of the instantiating interfaces are loadable.

3. There is a hack to avoid loading external signatures into the EPS,
   the ``is_external_sig``.  Really, ``loadInterface`` shouldn't be
   called on these at all (assert!), but recompilation checking does do
   this on occasion: signatures from external packages need to be loaded
   and their ABI hashes checked to see if we need to re-merge.  This
   might be refactorable but probably not without adding another cache
   to the EPS.

Typechecking interfaces (IfaceEnv)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Signature merging (mergeSignatures in TcBackpack, typecheckIfacesForMerging in TcIface)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Before reading this section, make sure you are familiar with
GHC's use of `knot-tying <https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/TyingTheKnot>`_.

Merging signatures is subtle business, because it interferes
with how GHC does knot tying.  Consider these two signatures,
which we would like to merge::

    -- first signature
    signature A where
        data T
        x :: T

    -- second signature
    signature A where
        type T = Bool
        x :: Bool

When we typechecked these signatures individually, we formed
a graph for each of them; in particular, the type of ``x`` from
the first signature points to the *abstract* type constructor
``data T``.  If we directly compare these two graphs for
compatibility, we will not observe that this abstract type has
already been refined into ``Bool``.

As it turns out, this problem is solvable, by clever application
of knot tying.  Here's how we merge these signatures:

1. First, we syntactically merge all of the entities of the
   signatures, and then typecheck the merged ``IfaceDecl`` into a graph
   representation.  We call this the *merged type environment*.
   In the example above, this type environment might
   be::

        signature A where
            type T = Bool -- type is more precise than abstract data
            x :: T        -- arbitrarily picked first type

   There is no guarantee that this graph is well-typed or
   even well-kinded; it simply serves as a type computation
   pass (ala RMC).

2. Next, we typecheck each individual signature ``ModIface``,
   resolving all name references *to* the graph entities
   we computed in (1).  Thus, ``x :: T`` in the first
   signature gets compiled to a type with a pointer not
   to ``data T``, but ``type T = Bool`` from the merged
   environment.  This gives us a series of graph that look
   like this::

       .            +--------------+
              +---> |  merged env  | <---+
              |     +--------------+     |
              |                          |
       +------+------+            +------+------+
       | signature 1 |            | signature 2 |
       +-------------+            +-------------+

   Or with the individual entities in view::

       .               +--> type T = Bool  (merged env)
                       |    x :: o--\
                       +------------/
        (sig 1)        |                    (sig 2)
        data T         |                    type T = Bool
        x :: o---------+                    x :: Bool

3. Finally, for each signature, we compare each typechecked
   entity it declares with the corresponding entity
   from the *merged* type environment.  Now we can see that
   the merged ``x`` has the same type as the ``x`` from
   signature 2, since the merged ``x`` has a type synonym
   which unfolds to ``Bool``; in fact, *all* declarations
   of ``x`` now see the unfolding.

The initial syntactic merge motivates why renaming cannot
happen on the fly during interface typechecking: we can't
do a syntactic merge on signatures until after they have
been renamed.


Recompilation checking (Desugar, MkIface)
~~~~~~~~~~~~~~~~~~~~~~~~~

Pretty-printing
~~~~~~~~~~~~~~~

I experimented with various pretty-printing schemes, for both debugging
output and user-visible output.  The current printing scheme coincides
closely with our ICFP'16 submission:

* Names pretty-print as ``M.n``, unless ``M`` is a hole module ``<m>``,
  in which case they pretty-print as ``{m.n}``, UNLESS the name would
  be printed unqualified (in which case it just prints as ``n``.)
  (``pprExternal`` in Name)

* Unit identifiers pretty-print according to their `grammar <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst#identifiers>`_,
  however, in some circumstances, GHC will *abbreviate* the
  instantiation.  Entries in the module substitution are elided
  if (1) we would *not* have qualified the module name, and (2)
  the requirement name and the module name agree.  These cases
  typically indicate that the "default" instantiation was carried
  out.  The full unit identity can be printed using ``-dppr-debug``.
  (``pprUnitId`` in Module)

* Module variables, if they would be qualified, are pretty
  printed as ``<m>``. (``pprModule`` in Module)

Proposed Change
---------------

Here you should describe in precise terms what the proposal seeks to change.
This should cover several things,

* define the grammar and semantics of any new syntactic constructs
* define the interfaces for any new library interfaces
* discuss how the change addresses the points raised in the Motivation section
* discuss how the proposed approach might interact with existing features  

Note, however, that this section need not (but may) describe details of the
implementation of the feature. The proposal is merely intended to describe what
the new feature is and how it should behave.

Drawbacks
---------

What are the reasons for *not* adopting the proposed change. These might include
complicating the language grammar, poor interactions with other features, 

Alternatives
------------

Here is where you can describe possible variants to the approach described in
the Proposed Change section.

Unresolved Questions
--------------------

Are there any parts of the design that are still unclear? Hopefully this section
will be empty by the time the proposal is brought up for a final decision.
