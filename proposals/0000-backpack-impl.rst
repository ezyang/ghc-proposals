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
circumstances involving signature merging):

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

RnModIface
~~~~~~~~~~

NameShape
~~~~~~~~~~

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
