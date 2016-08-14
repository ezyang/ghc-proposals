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

To typecheck an indefinite Backpack unit, GHC performs the following steps:

1. For each required signature, we typecheck the local ``hsig``
   file, and then merge the resulting interface with all of the
   inherited requirements of the dependencies at this module name.
   The resulting interface file produced for this module name records
   the *fully merged* signature (i.e., if you fulfill this signature,
   you will fulfill the local signature as well as all of the
   dependencies which this signature inherits from.)

2. Once signatures are processed, we typecheck all the modules.

Prior to typechecking a module or signature, we check that every
dependency that may be used by this module/signature is well-typed
(i.e., that our local merged signatures and other instantiating modules
match the requirements of the dependency.)

Typechecking produces a set of interface files for the fully generalized
unit identity of this unit, which the client installs to the installed
unit database as an entry for an indefinite unit.

To compile an definite Backpack unit for some definite unit identity we
assume that all of the appropriately instantiated dependencies have
already been compiled and installed to the package database.  Then GHC
does the following:

1. For each required signature, we read in the merged signature from
   the corresponding indefinite unit id, and instantiate it with the
   implementation recorded in the unit identity (checking if the
   implementation is sufficient).  The resulting interface file
   simply reexports all of the necessary entities from the
   implementing module. (TODO: Do we really want to require the
   indefinite unit to be installed to be able to compile? We
   have all the necessary information!)

2. Once signature are processed, we compile all the modules.

The resulting object files and interfaces are then to be installed
by the client to the installed unit database as an entry for a definite
unit.

We never install partially instantiated units to the installed unit
database; they are either indefinite units in most general form, or
fully instantiated definite units.

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

Name substitutions are handled by ``NameShape``, discussed in
a different section.

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

Flattened unit identifiers
    The current design represents ``UnitId``\s as a tree data structure
    in all situations.  It would be nice to avoid loading these trees
    into memory when they are not necessary, e.g., when compiling
    a definite library (where we do not ever need to perform
    substitutions over the unit identifier); in those cases, we
    simply use the ``Unique`` from the abbreviated unit identifier
    string.

    However, a similar difficulty arises to deferred hashing: what
    if we need to compare an abbreviated unit identifier with a full
    one.  Here are two non-solutions:

    1. If we deleted the hash entirely, we will need to consult
       the installed unit database to get the expanded form of the
       unit identifier.

    2. Another strategy is to load the tree structure
       *lazily*; if we never inspect the structure of a unit identifier,
       we avoid parsing the tree into memory (though we would still pay
       the cost of holding onto the unparsed string in case we *do*
       need to parse it.)

    Neither of these strategies work because we need to immediately
    generate uniques for unit identifiers, before we know if we
    are going to compare them to their abbreviated or un-abbreviated
    versions.

    A more promising approach is to *guarantee* that all unit
    identifiers handled by GHC are either entirely abbreviated,
    or entirely expanded.  Thus, when we read in interface files
    or the unit database, we must know if we are compiling
    a definite library or typechecking an indefinite library.
    When compiling a definite library, extreme care must be
    taken when handling interfaces from indefinite libraries.
    This has consequences for how we implement signature
    instantiation.

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
Module``).  This distinction influences GHC in the followng ways:

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

We only ever write out interface files for fully general unit identities
(indefinite units) or fully instantiated units (definite units).
However, over the course of typecheck an indefinite unit, we may
need to read out the interface for a partially instantiated unit
identifier.  To implement this, when we request an interface
at a unit identifier, we read the interface for the generalized
unit identifier (guaranteed to exist), and then *rename* it
according to the module substitution in the unit identifier.
Renaming a ``ModIface`` is implemented by ``rnModIface``
in RnModIface.

Interface renaming proceeds functorially on all occurrences of ``Name``
in ``ModIface``; ``rnIfaceGlobal`` is the workhorse.  The complexity
of this function stems two facts:

1. Morally (for example, we describe name substitutions in our ICFP'16
   submission in this way), the module variable substitution *induce*
   a name variable substitution.  We could go ahead and calculate the
   name variable substitution ahead of time. However, this is wasteful
   if there aren't actually occurrences of a those name variables in
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
   identity (this only occurs if we are about to merge it to form the
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

The precondition for interface renaming is that the domain of all name
substitutions must cover all of the name variables that actually
occur in the interface.  For unit identities which occur from
interface files, this precondition is already fulfilled; however,
user specified unit identities (via ``-unit-id``) can violate this
invariant.  Thus, ``checkImplements`` in TcBackpack ensures this invariant is
upheld (it is called by ``checkUnitId`` and ``instantiateSignature``).
(To be discussed later.)

Note that renaming on ``ModIface`` is necessarily incomplete: top-level
declarations in a ``ModIface`` are identified only by ``OccName``
and cannot be substituted. There is one last renaming step that occurs
when typechecking the interface (to be discussed later) for handling
these top level identifiers.

Given that interface typechecking must do renaming as well, why can't
renaming be deferred to typechecking entirely?  Immediate renaming
is extremely useful when merging signatures (to be discussed later),
where we must rename and merge interfaces with different instantiations
prior to typechecking. This algorithm would be very confusing if we
hadn't renamed by then.

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
