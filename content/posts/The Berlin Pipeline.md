---
title: "The Berlin Pipeline"
date: 2022-11-30
draft: false
---

The Berlin Pipeline was originally an architecture proposal for a
compiler project written in the
[ML](https://en.wikipedia.org/wiki/ML_(programming_language)) style.
Which is to say the compiler is written as a series of passes and
layers from the frontend language all the way down to the
backend. With that said, The Berlin Pipeline should be of interest to
compiler writers regardless of backgrounds and can be applied to any
compiler that:

1. Has complex pipelining
2. Can not be self hosted.

Category `1.` tends to occur with any language that has a core that
can not host itself. Therefore languages like
[Lisp](https://en.wikipedia.org/wiki/Lisp_(programming_language)) and
[Smalltalk](https://en.wikipedia.org/wiki/Smalltalk) do not have this
issue, however languages in the
[ML](https://en.wikipedia.org/wiki/ML_(programming_language)) family
or the [Algol](https://en.wikipedia.org/wiki/ALGOL) family do have
these issues.

Category `2.` tends to occur with domain specific languages. For
example the [Alucard](https://github.com/anoma/juvix-circuits)
programming language that I worked on targets a `ZKP` language known
as [vamp-ir](https://github.com/anoma/vamp-ir). Since `ZKP` languages
typically are not powerful enough to create very general abstractions,
the core of `Alucard` could not extend itself, thus it relies on a
pipeline to get frontend code down to `vamp-ir`.

The Berlin Pipeline is a flexible architecture proposal, and has a
generic processing model that is ripe for further extension. So, the
base model can be tweaked to one's use case. In developing the
original `ML` compiler, some extensions were implemented, like
implementing passes that can hook `before`, `after`, and `around`
every pass (This feature was heavily inspired from
[CLOS](https://en.wikipedia.org/wiki/Common_Lisp_Object_System)).

The drawback of this generality is that the architecture takes a bit
of development time, so in more simple compilers it may be best to use
some of the ideas proposed here for inspiration rather than taking the
architecture wholesale.

The rest of this document is the original proposal without edit,
however there are a few key sections changed:

1. The `Examples` section has new examples added that show the use of
   the Berlin Pipeline in action in the real compiler it was written
   for.
2. The `Why Behind S-expressions` section has been added to give
   motivation on why processing should be on such a generic structure.
   - This section is a long analysis on the benefits of having a
     standardized and flexible format to work with inside a compiler.
   - This section can be read on its own, and the lesson provided by
     it can be taken to any compiler project.

With the preamble out of the way, we present the original Berlin
Pipeline proposal!

## The Berlin Pipeline Proposal

In this document we outline the Berlin Pipeline. The document is
broken up into three core chunks along with a miscellaneous section
that outlines helper structures which are integral to the smooth
operation of the pipeline.

These four sections are

1. The Berlin Pipeline Step
2. The Berlin Pipeline Proper
3. The Berlin Pipeline Automatisierung
4. The Berlin Pipeline Extensions

The fourth section covers extensions that have made it in the codebase
that may alter the shape of the first three sections that are written
about.

With the Following sections to prop up the understanding of the Model

0. The Why Behind S-expressions.
1. Motivation
2. Das Modell
3. Computational Meta Data A Plan
4. Cyclic Lists and Their Uses
5. A Note about Notation
6. Examples

Examples will show the API come together with a few examples taken
from a compiler which used the Berlin Pipeline. This section can be
seen after the whole model is shown.

The full code that backs the proposal can be found
[here](https://github.com/mariari/Misc-ML-Scripts/blob/master/OCaml/berlin-pipeline.mli).

## The Why Behind S-expressions.
[S-expressions](https://en.wikipedia.org/wiki/S-expression) were a
controversial choice, however something akin to them are a necessity
for achieving the Berlin Pipeline.

The key insight is that in a statically typed programming language one
would want:

1. The ability to stop anywhere in the pipeline and inspect the state generically
2. The ability to share the language context/environment between code
   that is already compiled and the newly compiling code.

Point `2.` is a nice feature to have, and is required if one is
writing an
[incremental-compiler](https://en.wikipedia.org/wiki/Dynamic_compilation#Incremental_compilation),
and point `1.` is needed to have a nice generic API like the Berlin
Pipeline.

What this means is that for statically typed language, you want to run
your passes and store them in the context/environment in some
`serialized` dynamic format. The format we have chosen for this is the
[S-expression](https://en.wikipedia.org/wiki/S-expression).

For dynamically typed languages this is not needed, as the context can
hold any type of structure, and passes can be run with no issue on
different formats.

However, I would argue that even for dynamically typed languages, the
concept of a serializable structure to do work on is a great benefit
and would severely reduce the amount of boilerplate written.

To give a feeling of this, let us imagine we are trying to move our
AST into ANF form. Instead of trying to match on every single kind of
object/structure that makes up our AST, we can instead turn the more
generic structures into the `serialized` format and run our
transformation there before deserializing it back.

An example of this can be seen in my very own [AST transformation for Alucard](https://github.com/anoma/juvix-circuits/blob/1769761e77cbce8c1f84e89554a415a5bbac7066/src/pass/anf.lisp#L101).

If the structure didn't have this, then the same boilerplate would
have to be implemented for every single variant listed in the `or`
listed there, with slightly different calling semantics that refer to
their particular field accessors and constructors.

Since the serialization format unifies the structure of the entire
compiler, a team or person writing tools on one part of the codebase,
can directly be used by another team/person working on the opposite
end of the codebase immediately. No issue of a different AST format,
forcing the bulk of the tooling to be rewritten is had.

I would go on to say this encourages:

1. Compiler tooling in the compiler to be made. Tooling is now high
   leverage and affects the entire codebase.
   - This is very reminiscent to what more iterative compilers like
     [Squeeks](https://en.wikipedia.org/wiki/Squeak) do, where since
     they are forced to use their language, it becomes high leverage
     [to extend the environment as they write](https://smalltalkzoo.thechm.org/papers/Back%20to%20the%20Future.PDF)
   - Further it falls inline with the Alan Perlis quote "It is better
     to have 100 functions operate on one data structure than to have
     10 functions operate on 10 data structures."
  - Tooling includes concepts like:
    1. Code Inspectors
    2. AST traversal tools (more on this later)
    3. Code Debugging Tooling
    4. Tracing Tooling
    5. Extensible Formatting tooling
2. Safer, flexible, and more resilient codebase

Point `2.` needs further clarification. To demonstrate this point I
will be using the `Haskell` programming language, and discussing
various ways of writing AST traversal functions.

- There are only a couple of ways we can do this in Haskell

    1.  Write out concrete data structure between each pass, and
        write a small pass between each structure
    2.  Use [`Tagless Final`](https://okmij.org/ftp/tagless-final/index.html)
        to make changing a big AST less cumbersome
    3.  Use `Trees that Grow` to simulate small changes within an AST
    4.  Give up on small changes, and bundle together `n` changes in
        one pass.
    5.  Use Generics to automate the structural changes.
    6.  Use S-expressions to change the structures

-   The issue with `1)` is that it incurs a code cost of the length of
    the structure you are working over. This means that for any
    unchanged part of the syntax, we must map it to itself recursing
    down manually. Further this means that any tool we made for this
    form of the syntax, it can not be used later, meaning that any
    safety tooling we've made now has to be remade or simply forgotten
    past this point.

-   The issue with `2)` is that it becomes `O(n)` to do any
    transformation which require matching on arguments (I believe
    [this](https://okmij.org/ftp/tagless-final/course/Boehm-Berarducci.html#tagless-final)
    link by Oleg outlines the issue).  Along with complications of
    forming the `observation` type in Haskell, make it a less
    compelling case than other options. Many such inspection tooling
    would benefit from a more concrete syntax, which can be done in
    `tagless final`, however it is more complicated and comes at a
    cost.

-   The issue with `3)` is that it only automates forming the
    data-type itself, not any pass. Thus to make a transformation, it
    takes as much code as `1)`. With `5)` the situation may improve
    but we will discuss it when we discuss `5)`.

-   The issue with `4)` is that we now have to bundle `n` individual
    passes into one macro pass. This greatly simplifies the issues
    with the other passes, as we have very fixed layers of data types
    that we can scrutinize, however this leaves any proving or
    reasoning about the passes themselves much harder. This also like
    the previous ways of processing removes any kind of sharing that
    is possible between the passes.

-   The issue with `5)` is that we are leveraging on a system that
    splits information evenly at the type level and the term level.
    We can demonstrate this phenomenon by using `to` and `from`
    directly to see the representation this would take even for some
    very basic terms.

    ```haskell
    -- Getting the term from a basic structure
    λ> Library.from (Other 3)
    M1 {unM1 = L1 (L1 (L1 (M1 {unM1 = M1 {unM1 = K1 {unK1 = 3}}})))}

    -- Checking the type information which allows us to get back to a
    -- concrete type

    λ> :t Library.from (Other 3)
    Library.from (Other 3)
      :: Num a =>
         D1
           ('MetaData "BinderPlus" "Contextify.Binders" "main" 'False)
           (((C1
             …

    λ> :t Library.to
    Library.to :: Generic a => Rep a x -> a

    λ> :t Library.from
    Library.from :: Generic a => a -> Rep a x
    ```

    Here we can see that for the constructor `Other`, we have 3 `L1`
    structures and two `M1` structures wee have to get rid of before
    we can get to the data. Worse if we look at the type level, we can
    see that the entire ADT is reserved through this. So if we wanted
    to create a pass that got rid of the boilerplate, translating from
    one ML data type to another, we can not just call `to` and `from`
    as the types are incompatible, even if we changed the subset of
    cases we wish to change. Thus we\'d have to reconstruct the `M1's`
    and `L1's` to get back a proper typed representation. This means
    that we have to compute on the `Rep` representation of the type
    during the pass, calling `to` once all cases have been run,
    effectively ending up with the same amount of boilerplate at 1.

    One approach that might work is combining this with `3)` to
    properly generate these transformations. However this would take
    more template Haskell to properly experiment with.

-   The issue with `6.` is that we've expressed the passes as a bona
    fide macro-expansion, therefore proving that they can indeed be
    done within a language. Thus instead of working on getting the
    language to a state where it can support such expressions and
    centralizing work upon this effort, we put extra complexity on
    getting a large frontend language to meetup with the small
    core. Thus, infrastructure becomes an issue as we need proper
    tools to properly conduct these transformations.

So it may seem like the situation is fairly rough. With approaches
`1.` and `4.` giving concrete definitions of the AST that over
constrain it, and approaches `2.` and `3.` having their own faults in
the extension mechanism, leaving them too constrained or complex in
practice. Approaches `5.` and `6.` completely unconstrain the
representation, removing any sense of type safety. However the main
benefit of `6.` in particular is that we can write generic tools
easily over the structure. So what if we can selectively harden the
`AST` for the forms that we care about and run our passes on those?

This would mean that we can implement generic tooling for traversing
`S-expressions`, allowing us to pass a function `f` that gets run on
every deserailzied form. For context free passes, we could run this
kind of traversal on individual parts of the AST safely and turn it
into a different kind of AST node before serializing it back.


```haskell
f : Cond -> If

condToIf : Sexp -> Sexp
condToIf = Sexp.runOnDeserialize @Cond f

data Cond = Cond [(Sexp, Sexp)] deriving Serialize

data If = If Sexp Sexp Sexp deriving Serialize
```

And for context sensitive passes, we can implement a deserializer that
talks about all forms of binding in the system. Meaning that we can
implement generic logic that respects binders throughout the
compiler. The `Infix` example in the `Examples` section has code that
does precisely this.

Further, in languages with good interface support like smalltalk (any
OO language would do) this can be abused for easily declaring that
certain types (or keywords that deserialize to a type) map to a binder
and that we can use our generic logic to handle it.

We can see that point `2.` expands into giving us:

1. Αn AST that can be as constrained as we need it to be
2. Tooling to enforce constraints throughout any part of the compiler
3. Tooling that leaves the AST unconstrained enough to handle radical
   change
4. Allowing independent AST changes to be re-organized into more
   affect AST traversals.

All of these benefits are not unique to `s-expressions`, but rather to
any conceptual model that allows easy serialization, deserialization,
partial deserialization, and traversing the serialized format.

Overall these properties are highly desirable when trying to make any
compiler. Languages that can have cores which can themselves do this
implicitly, and their power is required for achieving our motivations.

## Motivation
One may ask why we should even begin the Berlin Pipeline, afterall we
currently can get `LLVM`, `Michelson`, and `Plonk` a term to
compile. However what this does not take into account is the following
properties:

1. Having the ability to stop between any step to see the results of a
   certain phase
2. Proper feedback to the compiler through the steps
    - The pass simply returns an either, it does not have the capacity
      to return the types
3. A consistent environment to thread through all passes
4. A consistent environment to compile not just a term, but a list of
   definitions
5. A consistent environment between compilation passes.

Out of these issues, only issue `2.` is solvable with the current
architecture.

This is becuase the pipeline has the following structure:
```haskell
type Pipeline = Feedback.FeedbackT [] [Char] IO

class HasBackend b where
  type Ty b = ty | ty -> b
  type Val b = val | val -> b
  type Err b = e | e -> b

  stdlibs :: b -> [FilePath]
  stdlibs _ = []

  -- | Parse source code passing a set of libraries explicitly to have them in scope
  toML' :: [FilePath] -> b -> Text -> Pipeline [(NameSymbol.T, [Types.TopLevel])]

  toSexp :: b -> [(NameSymbol.T, [Types.TopLevel])] -> Pipeline (Context.T Sexp.T Sexp.T Sexp.T)

  toHR ::
    (Show (Ty b), Show (Val b)) =>
    Param.Parameterisation (Ty b) (Val b) ->
    Context.T Sexp.T Sexp.T Sexp.T ->
    Pipeline (Core.RawGlobals HR.T (Ty b) (Val b))

  toIR ::
    Core.RawGlobals HR.T (Ty b) (Val b) ->
    Pipeline (Core.PatternMap Core.GlobalName, Core.RawGlobals IR.T (Ty b) (Val b))

  toErased ::
    Constraints b =>
    Param.Parameterisation (Ty b) (Val b) ->
    (Core.PatternMap Core.GlobalName, Core.RawGlobals IR.T (Ty b) (Val b)) ->
    Pipeline (ErasedAnn.AnnTermT (Ty b) (Val b))


  typecheck :: Context.T Sexp.T Sexp.T Sexp.T -> Pipeline (ErasedAnn.AnnTermT (Ty b) (Val b))

  compile ::
    FilePath ->
    ErasedAnn.AnnTermT (Ty b) (Val b) ->
    Pipeline ()
```

Starting form `toSexp` we get the `Pipeline` type which is just an
alias for Feedback. Thus no environment could be shared. However, even
if we extend the `Pipeline` type with Context Information, which
context do we share? All the passes up to `toHR` are compatabile with
the `Context.T` type, however at that point core uses its own hash
table structure. Thus to properly fix this situation we need a
consistent type to thread between the backends.

Further we can see with this structure, that the details of each pass
are not expressed. in fact `toSexp` hides over 7 passes that we have
no way of stopping between unless we reconstruct the pipeline. Hence,
the `Easy` module explicitly repeats this code. Thus we need some kind
of structure that allows us to talk about passes and sub-passes.

The Berlin Pipeline seeks to unify both the type of a step and build
up the type of the pipeline itself. Through this structure we can get
all 5 of the missing properties and have a principled way of talking
about what a pass is. This unification also allows tools to be made
around a pass allowing easy creation of a new pass and the placement
in the pipeline.

## Das Modell

The model of the new pipeline is laid out in the following way:

1. There exists the Berlin Step. This notion will be carried out in
   the `Pipeline.Step` module and `Pipeline.Step.t` will serve as the
   type any compiler step inhabits.
2. The Berlin Pipeline Proper refers to the `Pipeline.Environment`
   module, with the `Pipeline.Environment.t` being the constructed
   compiler pipeline itself.
3. The Berlin Automatisierung concept refers to the
   `Pipeline.Automation` module which has functions to make inhabiting
   `Pipeline.Step` easier.

In this model `Pipeline.Environment` will give the pipline organizer
the tools to clearly lay out a straightforward Pipeline. It will also
give the caller of the pipeline the tools needed to introspect, end
the pipleine early etc.

The Pipeline pass writer will utilize the `Pipeline.Automation` module
to create passes for the pipeline, inhabiting `Pipeline.Step.t` and
finally naming the type giving the pipeline organizer the
`Pipeline.Step.named` type.

## Computational Meta Data: a Plan

The current pipeline already has a notion of Meta information
through the use of the `Feedback` monad. However, the current
`Feedback` monad conflates Feedback and the result of computation
along with not being easily modifiable to add extra meta information.

Thus we extend and break apart the `Feedback` monad as such.

```ocaml
module Meta : sig
  type t = { feedback : Feedback.t; trace : Trace.t }
end

module ComputationResult : sig
  type 'a t =
    | Success of {meta : Meta.t; result           : 'a}
    | Failure of {meta : Meta.t; partial_feedback : 'a option}
end
```

The `Meta.t` type is filled with the meta information from
computation. Namely `Feedback.t` and `Trace.t`. The `Feedback.t` is
the actual feedback part of the `Feedback` monad, and the `Trace.t` is
any traces we could have gotten from a piece of computation. The
`Meta.t` type could be extended with further information if further
information should be captured.

Further we have the new concept of `ComputationResult.t` stemming from
the `Failure` and `Success` case of the old `Feedback` monad. However
we have a twist in that if we fail computation, we may get a partial
result.

## Cyclic Lists and Their Uses

Another import structure in the Berlin Pipeline is concept of the
circular list

we give this type stub to represent the idea

```ocaml
(** A Recursive list represents the concept of a list with a chance to nest
 * This allows us to get a tree like structure,
 * where A pipeline step can contain more steps
 *)
module RecursiveList : sig
  type 'a t
end

(** A circular list represents the idea of steps that can be recursive
 * on each other
 *)
module CircularList : sig
  (* TODO : add a recursion function to the recursive bit, that tells
   * it how it's recursive. perhaps it uses a predicate function
   * on the 'a type to determine what function to go to next.
   *)
  type 'a recursive_schema =
    | Recursive    of 'a list
    | NonRecursive of 'a

  type 'a t =
    'a recursive_schema RecursiveList.t
end
```

The `CircularList.t` is broken down into two parts. The first is a
`RecursiveList.t` which represents a list which can recurse. This is
very similar to the `Sexp.t` concept except instead of holding
s-expressions it instead holds arbitrary data.

Using the recursive list, we are able to construct a circular list
which has the notion of cycles. The cycle behavior is determined by
the `recursive_schema` type which states how the contents are
recursive on each other. The `recursive_schema` type should be refined
in future specification.

## A Note about Notation

In this proposal we will be using OCaml code for signatures. OCaml as
opposed to Haskell was chosen as it has a notion of modules in
language so the specification can be more straightforwardly laid out.

This does mean that for many examples we will have `Sexp.t list`
instead of `List.t Sexp.t` or even `[Sexp.t]`.

Further when we convert the OCaml functions to Haskell, we will turn
`snake_case` into `camelCase`.

Since OCaml lacks notions of effects, modules may be given that
represent the Haskell effects and Haskell may be given to supplement
where the OCaml may be confusing.

## The Berlin Pipeline Step

### Purpose and Specification

The Step Module serves as the base interface for any compiler
step. The module is quite minimal, with only a few types along with a
small set of operations.

This main type of the module, `t`, is simply a function type that
takes a structure type that has more than it needs to do any pass
called `computational_input` and gives back the result of the
computation plus `Meta`-data.

```ocaml
  module Step : sig
    ...
    type t = computational_input -> working_environment ComputationResult.t
    ...
  end
```

We say that the pass takes more than it needs because not only is
`computational_input` composed of the `context` and the
`current_expression` we are trying to compile against:

```ocaml
(** [env_or_sexp] expresses data that may be already added to the environment
 * or are still in a base sexp form that requires no lookup
 *)
type env_or_sexp =
  | InEnv of NameSymbol.t
  | Sexp  of Sexp.t

type computational_input = {
   language_data    : working_environment;
   ...
}

type working_environment = {
  current_expression : env_or_sexp list;
  context            : Sexp.t Context.t
}
```

but also a surrounding environment, which contains information that
should be put in the passes working environment.

```ocaml
  type computational_input = {
      language_data    : working_environment;
      surrounding_data : surrounding_environment;
    }

  type surrounding_environment = {
     current_step_name : NameSymbol.t option;
     meta_information  : Meta.t
   }
```

This `surrounding_environment` is composed of `Meta` information that
is discussed in the [#Computational Meta Data section], along with the
name of the `current_step`. The usefulness of the name will be
discussed in a later section, but this environment should be able to
grow without effecting passes which don't touch the extra information.

The `Step` Module also has a notion of a `named` pass

```ocaml
   module Step : sig
    type t = computational_input -> working_environment ComputationResult.t

    type named = {
        name : NameSymbol.t;
        func : t
      }

    val name_pass : t -> NameSymbol.t -> named
  end
```

 The `named` type simply allows us to attach a name to any given
 Pipeline Step function. This will serve as the type that
 `Pipeline.Environment.register_step` will deal with. This type should
 be exported opaquely.

 Lastly, the `name_pass` function is a public constructor of this
 type.

### Rational

Overly non constraining types like the input type to `Step.t` should
be met with criticism, however in the case of the `Step` module it is
necessary to avoid the situation of trying to enforce the
`Pipeline.Environment`'s effect type upon the passes.

Since this is a needed separation, the `Step` module should not be
programmed against directly, but rather through the
`Pipeline.Automation` module that will be able to make a pass work
over a single `env_or_sexp` and a `Context.t Sexp.t` along with the
pass's custom environment.

## The Berlin Pipeline Proper

Now that we have the `Pipeline.Step` properly specified for the
passes, we can get to the infrastructure this allows the creation of.


### Specification

#### Reiteration

First before we get to the `Environemnt` in which the majority of the
compiler pipeline lives, we must first take a closer look at the
`surrounding_environment` the pass gets.

```ocaml
  (** [surrounding_environment] serves as the minimum surrounding information
   * in the environment of the Pipeline Steps.
   *)
  type surrounding_environment = {
    (* This will be taken as a read value *)
    (** The [current_step_name] represents the current running step name *)
    current_step_name : NameSymbol.t option;
    meta_information  : Meta.t
  }

  (** [computational_input] is the actual data passed into the [Step.t] type.
   * The [language_data] field serves as the direct data that the direct data
   * a pass wishes to take, while [surrounding_data] serves as extra environment
   * constraints that one wants to include.
   *)
  type computational_input = {
    language_data    : working_environment;
    surrounding_data : surrounding_environment;
  }
```

We can see the input type of the pass along with the
`surrounding_environment` it caries. For this proposal the
`surrounding_environment`code is quite simple, with having
`meta_information` which contains the `Trace.t` and `Feedback.t`
inside of it. This inclusion just lets Trace and Feedback information
be threaded through the passes, as the output type directly has new
versions of these values. It also has the `current_step_name`, which
reflects the current name of the pass running. We will see some
application of this in the ##Automation Section.

The important part of this `surrounding_environment` is that through
the output type of `Pipeline.Step.t`, we can have enough data to fill
the dynamic parts of the `Pipeline.Environment` itself.

#### Pipeline Environment

The Pipeline Type is defined as follows

```ocaml
module Environment : sig
  type t = {
    (** [information] serves as the public facing data to the environment
     * that passes can access
     *)
    information : computational_input;

    (* All other information is made to be exclusive with the Pipeline as
     * a whole that the Steps do not bother with.
     * This consists of information like how to deal with Traces between passes
     * to the entire pass infrastructure itself.
     *)

    (** [registered_pipeline] is the pipeline itself.
     * It features all the steps named and registered with the system *)
    registered_pipeline : Step.named CircularList.t;

    stopping_step : NameSymbol.t option
  }
  ...
end
```

It contains a few pieces of information.

The first piece is the `information` which contains the same type that
`Pipeline.Step.t` takes.

This is considered the "Dynamic" data of the pass and serves a few
purposes:

1. It contains enough information to support directly calling any
   pass.
    - Combining this with the output of the pass we can, with a little
      glue, directly construct a new `computation_input`.
2. It alleviates the Passes of having to use any capability regarding
   `Environment.t` directly, by feeding it directly as input.
    - This will, through `Automation` become part of the running
      environment of the pass directly, but we avoid the issue of the
      passes needing to use anything in `Environment.t` that would
      stop it being ran in it's own environment. This is Crucial as we
      can't run partial effects with Capabilities.
3. It clearly separates the concerns of the Pass, and the Concerns of
   the Pipeline itself.

The next few pieces of data relate to the pipeline itself and will be
constructed fully from functions that make up the pipeline or wish to
manipulate the execution of the pipeline.

The first piece of data in this list, is the `registered_pipeline`
which is what records the passes.

The type is a `CircularList.t` over the `Step.named` structure we
analyzed previously. The `CircularList.t` type was chosen because the
recursive nature gives us a few advantages over a normal List of
`Step.named`:

1. We get the same behavior as `Testy` with their `T.TestCase`
   ```haskell
   testGroup :: TestName -> [TestTree] -> TestTree
   ```
2. If we wish to support mutually recursive passes, then having the
   ability to explicitly state how these passes are circular is
   important.


We will state more on how `registered_pipeline` works when we get to
the API function portion.

Lastly for now, we will also have the value `stopping_step`, this
simply states at which step we should stop running the compiler and
return any working data that is left.

Although we only include two fields exclusive to the pipeline
environment, the design is in such a way that more fields and
behaviors should be easily added without much hassle.

- An example of an inclusion, is the ability to skip passes if a
  particular backend does not wish to have a pass or if it wants
  special treatment in some way.
- Another route is to have Alternative passes that are mutually
  exclusive but converge upon the same points.

#### API Functions

With the environment out of the way, let us consider how one is
supposed to use this environment and the functions on the environment.

The pipeline is intended to grow a single compile function, in which
we shall call `eval`. We do this by registering the
`Pipeline.step.named` into the environment. After we register all the
steps, we can chose if the pipeline stops early, introspect if some
pass fails etc. When it comes time to executing the pipeline a `run`
function will be given that takes the `Pipeline.Environment.t` value
and starts executing the registered passes, stopping either when a
pass fails, the `stopping_step` says we should stop at this step, or
if we have successfully compiled the given form.

```ocaml
(** [register_step] registers the pipeline function to the environment *)
val register_step : Step.named CircularList.t -> unit
```

`register_step` has the simple job of taking a named pass and added it
to the `registered_pipeline` type. Note that we take a
`CircularList.t` over this type to simply make the operation
monoidal. An example implementation could be something as simple as.

This will be a core way one interacts with the pipeline

```haskell
registerStep pass =
  modify @"registeredPipeline" (<> pass)
```

While the above function is useful, it has no way of helping us build
groups of steps. For that we have

```ocaml
(** [def_pipline_group] creates a named group of pipeline steps or a
 * nested grouping of pipeline steps *)
val def_pipline_group
    : NameSymbol.t
     -> Step.named CircularList.t list
     -> Step.named CircularList.t
```

This function should remind one of the `testGroup` function that is
used in `Testy` tests. It has the same behavior as that, we can then
register this function with `register_step`.

The next set of functions deal with talking about stopping the
pipeline early.

```ocaml
val stop_at : NameSymbol.t -> unit

val stop_at_nothing : unit
```

`stop_at`, tells the environment to simply stop when we reach the pass
with the given name. `stop_at_nothing` just resets the stopping
behavior to none.

The next function in the environment is the actual `eval` function
which is responsible for running the code.

```ocaml
(** [eval] is responsible for taking the environment, running it
 * to the desired  point and giving back what data is left.
 * The output type should be more carefully considered
 * and have many operations on it.
 *)
val eval : t -> computational_input
```

The `eval` function has a tough job, as that it is responsible for
taking the information in the `t` type and properly threading the
calls to the passes.

As mentioned in the doc comment above the function, the output type is
not fully considered. For this proposal we chose the
`computational_input` type.

This ends the core API of the `Pipeline`, more functions will be
needed in regards to introspecting the output type, but these are the
core functions to properly construct and change the core environment.

#### The Full API
```ocaml
module Pipeline : sig

  (** [env_or_sexp] expresses data that may be already added to the
  environment
   * or are still in a base sexp form that requires no lookup *) type
   env_or_sexp = | InEnv of NameSymbol.t | Sexp of Sexp.t

  (** [working_environment] serves as the input data relevant to any
   * pipeline step function. This is what is actively being processed
   * in the pipeline itself.  *) type working_environment = {
   current_expression : env_or_sexp list; context : Sexp.t Context.t }

  (** [surrounding_environment] serves as the minimum surrounding
  information
   * in the environment of the Pipeline Steps.  *) type
   surrounding_environment = { (* This will be taken as a read value
   *) (** The [current_step_name] represents the current running step
   name *) current_step_name : NameSymbol.t option; meta_information :
   Meta.t }

  (** [computational_input] is the actual data passed into the
  [Step.t] type.
   * The [language_data] field serves as the direct data that the
     direct data
   * a pass wishes to take, while [surrounding_data] serves as extra
     environment
   * constraints that one wants to include.  *) type
   computational_input = { language_data : working_environment;
   surrounding_data : surrounding_environment; }

  module Step : sig (** the pipeline step, takes the entire
    [computational_input] as input.
     * This is quite a wide type in that it has data that a pass does
       not
     * directly want to handle.
     * Thus it is expected one works with the [Automation] module, to
       make
     * fulfilling both the [ComputationResult.t] and the input form
     bearable.  *) (* May promote [working_environment] to
     [information_record] *) type t = computational_input ->
     working_environment ComputationResult.t

    type named = { name : NameSymbol.t; func : t }

    val name_pass : t -> NameSymbol.t -> named

    val name_of_pass : named -> NameSymbol.t end

  module Environment : sig
    type t = {
      (** [information] serves as the public facing data to the environment
       * that passes can access
       *)
      information : computational_input;

      (* All other information is made to be exclusive with the Pipeline as
       * a whole that the Steps do not bother with.
       * This consists of information like how to deal with Traces between passes
       * to the entire pass infrastructure itself.
       *)

      (** [registered_pipeline] is the pipeline itself.
       * It features all the steps named and registered with the system *)
      registered_pipeline : Step.named CircularList.t;

      stopping_step : NameSymbol.t option
    }

    (** [register_step] registers the pipeline function to the environment *)
    val register_step : Step.named CircularList.t -> unit

    (** [def_pipline_group] creates a named group of pipeline steps or a
     * nested grouping of pipeline steps.
     *)
    val def_pipline_group
        : NameSymbol.t
          -> Step.named CircularList.t list
          -> Step.named CircularList.t

    (** [stop_at] tells the environment to stop at a particular step when
     * running the environment.
     *)
    val stop_at : NameSymbol.t -> unit

    (** [stop_at_nothing] tells the environment to run the compiler fully.
     *)
    val stop_at_nothing : unit

    (** [eval] is responsible for taking the environment, running it
     * to the desired  point and giving back what data is left.
     * The output type should be more carefully considered
     * and have many operations on it.
     *)
    val eval : t -> computational_input

    (** [run] serves as the running point, the unit argument is a placeholder
     * for the monadic functions used to build up the environment
     * the [t] is the given pre-built environemnt
     *)
    val run : unit -> t -> computational_input

    val extract : unit -> t
  end
end
```
### Rational

This Structure was chosen to maximize the following:

1. Find a way to minimize the Haskell Hassle
2. Give a straight forward way to obsolete any `EasyPipeline` notions.
3. Give a flexible architecture which can satisfy our current and
   future needs.

With that said there may some questions one may have in regards to the
API

#### Why A Registration System?

This is brought up in regards to why not a more functional approach to
composition, indeed the following does not look functional!

```haskell
eval = Pipeline.extract $ do
  Pipeline.register_step
    (Pipeline.def_pipeline_group "Desugar"
                                 [ Desugar.condToIf
                                 , Desugar.ifToCond
                                 , ...])
  Pipeline.register_step
    (Pipeline.def_pipeline_group "Contextify"
                                 [ Contextify.resolveInfix
                                 , ...])
  ...
```

This was chosen over the more functional [approach given in the
idealized pipeline proposal](https://heliaxdev.github.io/juvix-docs/docs/overview/pipeline/idealized/),
because it allows us to name each step and stop in the middle of any
of them. Where as the more functional approach would leave that
structure ambiguous and behave much like a `tagless final` system
where the constructors fold onto itself leaving no room for
interpretation of the bare structure.

Further interest was given in programatically moving passes around to
properly see what makes the most sense. The given registration system
along with an extension to talk about dependencies in passes would
allow this experimentation.

### Examples

```haskell
module EasyPipeline where

passNames :: CircularList.t NameSymbol.t
passNames =
  fmap
    (Step.nameOfPass . Pipeline.registered_pipeline)
    Compiler.eval

prettyPassNames :: IO ()
prettyPassNames = CircularList.prettyPrintList passNmaes

runToCondWithTrace =
  Pipeline.stopAt "Desugar.if-to-case"
  -- Extra functions to trace what we care about
  Trace.enableRecursively ["Pass.cond-to-if"]

λ> Pipeline.run runToCond Compiler.eval >>| Pipeline.run-trace
```

## The Berlin Pipeline Automatisierung
 We have seen in the Step section that we have a general consistent
 data type for a Pipeline Step, however we also discovered that the
 type is:

 - Too large and has matters a pass should not worry about.
 - Has no environment effect, so the pass is expected to be in IO to
   get information.

Thus we propose the `Automation` module to alleviate these issues.

### Specification
#### Input and Output Types
To simplify matters to what a pass cares about we transform the
`computational_input` form below

```ocaml

type env_or_sexp =
  | InEnv of NameSymbol.t
  | Sexp  of Sexp.t

type working_environment = {
  current_expression : env_or_sexp list;
  context            : Sexp.t Context.t
}

type computational_input = {
  language_data    : working_environment;
  surrounding_data : surrounding_environment;
}

```

into

```ocaml
type pass_argument = {
  current : Pipeline.env_or_sexp;
  context : Sexp.t Context.t
}

type simplified_pass_argument = {
  current : Sexp.t;
  context : Sexp.t Context.t
}
```

Notice, how instead of working over a `Pipeline.env_or_sexp list` for
the current expression we instead worry about the current
expression. And we even have a more simplified version that removes
the `InEnv` potential of the `Pipeline.env_or_sexp` in the
`simplified_pass_argument`. This allows a pass to not worry about if a
`Sexp.t` is referenced in the environment, and allows all the
attention to be had on the transformation if the pass simply does not
care.

Further, the entire `surrounding_environemnt` is dropped from the
signature entirely.

This information is not completely gone, but instead we relegate it to
the environment surrounding the pass, rather than a direct input to
the pass.

The output of the pass also moves away from returning a
`working_environment ComputationResult.t`

```ocaml
type working_environment = {
  current_expression : env_or_sexp list;
  context            : Sexp.t Context.t
}

module ComputationResult : sig
  type 'a t =
    | Success of {meta : Meta.t; result           : 'a}
    | Failure of {meta : Meta.t; partial_feedback : 'a option}
end
```

into a more stream-lined type

```ocaml
type t =
  | ProcessJob of process_job_no_env
  | UpdateJob  of { new_context : Sexp.t Context.t; process : process_job }

type job = t

type stage
  = Current
  | FromTopToCurrent
  | Eval

type process_job = {
  current   :  Pipeline.env_or_sexp;
  new_forms : (stage * Pipeline.env_or_sexp) list
}

type process_job_no_env = {
  current : Sexp.t;
  new_forms : (stage * Sexp.t) list
}

```

What changes, that instead of giving back a list of
`current_expressions`, we instead have this idea of a `process_job` to
go along with the new input type of taking a single expression.

What this means, is that for any pass which defines a new top level
definitions to be consumed, instead of adding it to the
`current_expression` list, we instead add it to the `new_forms`,
letting the Automation handle how this commits back to
`current_expresions`. The current expression stays as the current
expression, thus the pass takes a `Pipeline.env_or_sexp` and gives
back a potentially new `Sexp.t`, with any new definitions as
`new_forms`.

Further we introduce the idea of an `UpdateJob` vs a `ProcessJob`,
meaning that instead of every pass that does not touch the `context`
just giving back the current `context`, we can instead denote, with
the `ProcessJob` that our pass has no changes to the context
itself. The `UpdateJob` indicates that we have changed the context, so
commit back the changed context.

This also explains the difference between `process_job_no_env` and
`process_job`, since the `ProcessJob` can not touch the `Context` it
ca not change any reference to the name which can be found in
`Pipeline.env_or_sexp`

Lastly, we introduce the concept of staging the newly defined
expressions through the `stage` type. This can take the form of
`Current`, `FromTopToCurrent`, or `Eval`.
- `Current` would act exactly like adding any new expression to the
  `current_expression` list.
- `FromTopToCurrent` acts almost like `Current` but runs all passes
  that have previous ran to the current position on the new `Sexp.t`
  defined
- `Eval` runs the entire compiler on the newly defined `Sexp.t`,
  updating the `Context` in the process before we proceed with the
  `current` form.


This is best to seen in action in the ###Examples Section

#### Environment Types and Effects

With the input and output types out of the way (`pass_argument ->
job`), we can now talk about the surrounding environment that was
alluded too earlier.

```ocaml
(* This represents the monad type that we require for a pass *)
module type MonadEff = sig
  type 'a t
  type 'a output_eff
  (* Trace State effect *)
  val get_trace : unit    -> Trace.t t
  val set_trace : Trace.t -> unit    t
  (* feed_back State effect *)
  val get_feedback : unit    -> Feedback.t t
  val set_feedback : Feedback.t  -> unit   t
  (* This is in all likelihood what is needed to setup the environment
   * usurping the above effects for actually setting up the pass
   * we leave the above effects for demonstrative purposes
   *)
  val has_env   : unit -> Pipeline.surrounding_environment t
  val set_env   : Pipeline.surrounding_environment -> unit t
  (* Comonad effect *)
  val run      : 'a t -> 'a output_eff
end
```

This OCaml module refers to the following Haskell signatures

```haskell
type class Functor m => Comonad m where
  run :: m a -> a

type HasTrace     m = HasState @"trace" Trace.t
type HasFeedback  m = HasState @"Feedback"  Feedback.t
type HasMeta      m = (HasFeedback m, HasTrace m)

type HasEnv m =
  (HasMeta m, HasReader @"CurrentStepName" (Maybe NameSymbol.t) m)
```

What is important here is that the `has_nev`/`HasEnv` effect entails
all the data that is passed through `surrounding_environment` plus any
pass specific data that it may want.

This allows the automation tooling to automatically hook into the
environment of the pass to convert the `job` into a
`ComputationResult.t job` for any given environment that a pass may
care about.

We can see this signature in action with the automation functions of
this module.

#### Automation Abstractions

```ocaml
(* Type class in Haskell that dictates being able to extract to a step *)
module Runable :
  functor (M : MonadEff) -> sig
    type 'a t = 'a M.t

    (** [run] runs the given environment, extracting out a result with meta data
     * over the resulting value.
     *)
    val run : 'a t -> 'a ComputationResult.t M.output_eff

    (** [apply_simplified_pass] serves as a HOF that allows for passes to be
     * a more simplified type, namely a function that takes a [pass_arugment]
     * to an effectual result over [job] that determines how the pass should
     * be brought together.
     * Note that [Pipeline.computational_input -> Pipeline.working_environment t]
     * is an approximation of the [Step.t] type without the Meta information attached
     *)
    val apply_simplified_pass
        : (pass_argument -> job t)
          -> Pipeline.computational_input
          -> Pipeline.working_environment t

    (** [run_simplified_pass] simply combines the [run] function with
     * [apply_simplified_pass], to get the output effect, which corresponds to a
     * [Pipeline.Step.t] with an effect attached to it
     *)
    val run_simplified_pass
        : (pass_argument -> job M.output_eff)
          -> Pipeline.computational_input
          -> Pipeline.working_environment ComputationResult.t M.output_eff
    (** [simplify] allows a pass to ignore the fact that expression coming in may
     * be added to the [Context.t] already, and we can act as if it were just a
     * normal [Sexp.t] being passed in.
     *)
    val simplify : (simplified_pass_argument -> job M.output_eff)
                   -> (pass_argument -> job M.output_eff)

end
```

This roughly corresponds to the Haskell Signature

```haskell
type class HasExtract a m | a -> m where extract :: a x -> m (ComputationResult.t x)

data Automation.PassArgument
  = PassArg { context :: Context.t Sexp.t, current :: Sexp.t }

transformOutputType :: Automation.t -> Pipeline.workingEnvironment

-- we share the one m monad here for the function
-- because we will extract it after we run the m effect
applySimplifiedPass
  :: (HasTrace m, HasMeta m)
  => (Automation.PassArgument -> m Automation.t)
  -- These form Pipeline.Step.t for some m over the
  -- output modulo the ComputationResult.t over it
  -> Pipeline.ComputationalInput
  -> m Pipeline.WorkingEnvironment

runSimplifiedPass
  :: (HasExtract _a m)
  => (Automation.PassArgument -> m Automation.t)
  -> Pipeline.ComputationalInput
  -> m (ComputationResult.t Pipeline.WorkingEnvironment)
runSimplifiedPass f = do
  extract . applySimplifiedPass f
```

We can see the `extract` or `run` as the OCaml version calls it, takes
the environment needed to run the pass, runs it, and gives us back out
a `ComputationResult.t` over the output type in the effect.

The `HasMeta` and `HasTrace` effects are needed in practice to support
this. Note that we also get an output Monad type. This is needed as
the pass may invoke IO actions, and thus we really get back an
`output_eff`ect from the pass itself.

The `applySimplifiedPass` reflects the underlying purpose of the
`Automation` modules. Namely a pass has become as simple as
`(Automation.PassArgument -> m Automation.t)`, where we take only what
we care about, and return back an effect over the `job` type. the goal
of this function then becomes the following:

1. Split the surrounding_environment to the env of the pass
2. Split the list of expressions into one and invoke the pass one or
   many times
3. Take a list of `job`/`Automation.t` types and give back a
   `Pipeline.WorkingEnvironment`

Since the module has enough information via the constraints, this is
fairly easy to achieve.

This leads into `runSimplifiedPass`, which simply just composes
`applySimplifiedPass` and `extract`/`run`. We can see here how these
two functions combine and fulfill the signature of `Pipeline.Step.t`
which we will repast here

```ocaml
module Step : sig
  type t = computational_input -> working_environment ComputationResult.t
```

The `simplify` function is a simplification that allows as pass to not
worry if the argument has come from the context or not. This is useful
for passes which do simple transformations but are ran after the point
in which functions show up in the environment, or any passes before
that we know the terms don't show up in the environment.

#### Full API
```ocaml
(** the job of [Automation] is to make the step function more amenable to writing passes
 * It is not ergonomic to take extra information one may not care
about *)
module Automation : sig
  type stage = Current | FromTopToCurrent | Eval

  type process_job = { current : Pipeline.env_or_sexp; new_forms :
    (stage * Pipeline.env_or_sexp) list }

  type t = | ProcessJob of process_job_no_env
           | UpdateJob of {
               new_context : Sexp.t Context.t; process : process_job
             }

  type job = t

  (** [pass_arguments] is a pipeline processing function, namely we
  wrap
   * the current expression and the context into a single function *)
  type pass_argument = { current : Pipeline.env_or_sexp; context :
  Sexp.t Context.t }


  (* This represents the monad type that we require for a pass *)
  module type MonadEff = sig
    type 'a t
    type 'a output_eff
    (* Trace State effect *)
    val get_trace : unit    -> Trace.t t
    val set_trace : Trace.t -> unit    t
    (* feed_back State effect *)
    val get_feedback : unit       -> Feedback.t t
    val set_feedback : Feedback.t -> unit   t
    (* combining the two effects above  *)
    val get_meta : unit   -> Meta.t t
    val set_meta : Meta.t -> unit t

    (* This is in all likelihood what is needed to setup the environment
     * usurping the above effects for actually setting up the pass
     * we leave the above effects for demonstrative purposes
     *)
    val has_env   : unit -> Pipeline.surrounding_environment t
    val set_env   : Pipeline.surrounding_environment -> unit t
    (* Comonad effect *)
    val run      : 'a t -> 'a output_eff
  end

  val transform_output_type
      : job list -> Pipeline.working_environment


  (* Type class in Haskell that dictates being able to extract to a step *)
  module Runable :
    functor (M : MonadEff) -> sig
      type 'a t = 'a M.t

      (** [run] runs the given environment, extracting out a result with meta data
       * over the resulting value.
       *)
      val run : 'a t -> 'a ComputationResult.t M.output_eff

      (** [apply_simplified_pass] serves as a HOF that allows for passes to be
       * a more simplified type, namely a function that takes a [pass_arugment]
       * to an effectual result over [job] that determines how the pass should
       * be brought together.
       * Note that [Pipeline.computational_input -> Pipeline.working_environment t]
       * is an approximation of the [Step.t] type without the Meta information attached
       *)
      val apply_simplified_pass
          : (pass_argument -> job t)
            -> Pipeline.computational_input
            -> Pipeline.working_environment t

      (** [run_simplified_pass] simply combines the [run] function with
       * [apply_simplified_pass], to get the output effect, which corresponds to a
       * [Pipeline.Step.t] with an effect attached to it
       *)
      val run_simplified_pass
          : (pass_argument -> job M.output_eff)
            -> Pipeline.computational_input
            -> Pipeline.working_environment ComputationResult.t M.output_eff
      (** [simplify] allows a pass to ignore the fact that expression coming in may
       * be added to the [Context.t] already, and we can act as if it were just a
       * normal [Sexp.t] being passed in.
       *)
      val simplify : (simplified_pass_argument -> job M.output_eff)
                   -> (pass_argument -> job M.output_eff)
  end
end
```
## Examples

It would be remiss of us to not include any examples in this
demonstration, so we provide a few examples of using the library. The
code below were taken from a now deprecated compiler, so the code was
used for a period.

In the following code snippets Ι make heavy use of my [Sexp library](https://github.com/mariari/Misc-ML-Scripts/tree/master/Haskell/Sexp)

which has many tools that make working with s-expressions quite
simple. The library offers automatic deserialization of data types,
along with predicate searches. The default way to use the library for
passes is to make a Haskell ADT and have the library run on all data
that can be deserailzied. This is great in that you can enforce
exhaustion throughout the passes even though one is operating on
s-expressions. Some of the examples given below were before this was
implemented, and so use the library in a more tedious way of matching
symbols. Since both ways of working with the API's can be seen, one
can compare and contrast them.

### Deusgaring Cond

Let our first example be on desugaring `cond`. `Cond` is a staple in
lisp code

```lisp
(defun dispatch (x)
  (cond ((evenp x) 42)
        ((= x 5)   0)
        (t         24)))
DISPATCH
CL-USER> (dispatch 6)
42 (6 bits, #x2A, #o52, #b101010)
CL-USER> (dispatch 8)
42 (6 bits, #x2A, #o52, #b101010)
CL-USER> (dispatch 7)
24 (5 bits, #x18, #o30, #b11000)
CL-USER> (dispatch 5)
0 (0 bits, #x0, #o0, #b0)
CL-USER>
```

here `cond` is really just a multiway if

```lisp
(macroexpand-1 '(cond ((evenp x) 42)
                     ((= x 5)   0)
                     (t         24)))
(IF (EVENP X)
    42
    (IF (= X 5)
        0
        (THE T 24)))
T
```

So the desugaring strategy is rather straightforward, given a `sexp`
with `cond` in the front, then desugar the `cond` into a series of If's.

This can be written somewhat simply using the s-expression library.

```haskell
-- | @condTransform@ - CondTransform turns the cond form of the fronted
-- language into a series of ifs
-- - BNF input form:
--   + (:cond (pred-1 result-1) … (pred-n result-n))
-- - BNF output form:
--   + (if pred-1 result-1 (if pred-2 result-2 (… (if pred-n result-n))))
condTransform :: Sexp.T -> Sexp.T
condTransform xs = Sexp.mapPredStar xs (== Structure.nameCond) condToIf
  where
    condToIf sexp@(Sexp.Atom atom Sexp.:> _)
      | Just cond <- Structure.toCond sexp,
        Just last <- lastMay (cond ^. entailments) =
        let acc =
              Structure.IfNoElse (last ^. predicate) (last ^. answer)
                |> Structure.fromIfNoElse
         in foldr generation acc (initSafe (cond ^. entailments))
              |> Sexp.addMetaToCar atom
    condToIf _ = error "malformed cond"
    --
    generation predAns acc =
      Structure.If (predAns ^. predicate) (predAns ^. answer) acc
        |> Structure.fromIf
```

This code simply folds if on the `cond`. More complicated than a
native lisp version but sufficient to demonstrate the point. The
`mapPredStar` automatically searches any of the s-expressions given at
a given point. However this code does not account for the context of
the language that is down the pipeline. This is where the Berlin
pipeline comes in.

```haskell
condPass :: Step.Named
condPass =
  (Trace.withScope "Desugar.cond-runner" [] . Automation.simplify condTrans)
    |> Automation.runSimplifiedPass
    |> Step.T
    |> Step.namePass "Desugar.cond-to-if"

condTrans :: Automation.SimplifiedPassArgument -> Env.MinimalMIO Automation.Job
condTrans simplify = do
  Trace.withScope "Desugar.condTrans" [show (simplify ^. current)] $ do
    condTransform (simplify ^. current)
      >>| (`Automation.ProcessNoEnv` [])
      >>| Automation.ProcessJob

-- | @condTransform@ - CondTransform turns the cond form of the fronted
-- language into a series of ifs
-- - BNF input form:
--   + (:cond (pred-1 result-1) … (pred-n result-n))
-- - BNF output form:
--   + (if pred-1 result-1 (if pred-2 result-2 (… (if pred-n result-n))))
condTransform :: (MonadIO m, Meta.HasMeta m) => Sexp.T -> m Sexp.T
condTransform xs =
  Trace.withScope "Desugar.condTransform" [show xs] $ do
    Sexp.traversePredStar xs (== Structure.nameCond) condToIf
  where
    condToIf sexp@(Sexp.Atom atom Sexp.:> _) recur
      | Just cond <- Structure.toCond sexp,
        Just last <- lastMay (cond ^. entailments) =
        let acc =
              Structure.IfNoElse (last ^. predicate) (last ^. answer)
                |> Structure.fromIfNoElse
         in foldr generation acc (initSafe (cond ^. entailments))
              |> Sexp.addMetaToCar atom
              |> recur
    condToIf _ _ = Env.throw $ Env.MalformedData "cond is in an invalid format"
    --
    generation predAns acc =
      Structure.If (predAns ^. predicate) (predAns ^. answer) acc
        |> Structure.fromIf
```

Here we have refactored the code to be in the Berlin Pipeline
format. `condTransform` hardly changed, we only improve upon it with

```haskell
 Trace.withScope "Desugar.condTransform" [show xs] $
```

which adds tracing information to the given point, so if we wanted to
debug this code we can see precisely the input and outputs of the
`condTransform`.

Further there are two new functions `condTrans` and
`condPass`. `condTrans` is the main boilerplate for telling the
pipeline that we don't care about the environment, so please simplify
those details for us, and also that we don't affect the context, we
simply only process some S-expressions into a new form. The `condPass`
now simply tells the `Automation` module to apply the stated
simplifications and give the pass a name. Thus we've introduced 13
lines of boilerplate, however they have given us

1. The ability to apply our simple pass on the entire state of the language
2. Tracing at every level, so if we so wished we can see the effect of
   the pass even without stopping at desired location
3. A clear and clean stopping point so that we can debug our compiler.

### Infix Resolution

It is all well and good to show a pass which does not rely upon the
context to feed it information. However, what about a more complex
pass that relies upon information in expressions above the current
one?

Well for this, we will display code related to `infix
resolution`. This requires closures, as the language that used this
had the ability to make new infix symbols and declare their infixivity
on the fly.

```haskell
data Infix
  = Infix
      { infixOp :: NameSymbol.T,
        infixLt :: (Sexp.B (Bind.BinderPlus Infix)),
        infixInf :: Infix
      }
  | InfixNoMore
      { infixOp :: NameSymbol.T,
        infixLt :: (Sexp.B (Bind.BinderPlus Infix)),
        infixRt :: (Sexp.B (Bind.BinderPlus Infix))
      }
  deriving (Show, Generic, Eq)

infixConversionPass :: Step.Named
infixConversionPass = mkPass name trans
  where
    name = "infixConversion"
    trans = mkTrans name infixConversionTransform

infixConversionTransform ::
  ( HasThrow "error" Sexp.T m,
    Feedback.Eff m,
    Env.HasClosure m,
    MonadIO m
  ) =>
  Context.T ->
  Sexp.T ->
  m Sexp.T
infixConversionTransform ctx =
  infixConversion ctx |> traverseOnDeserialized

infixConversion ::
  (Env.ErrS m, Env.HasClosure m) => Env.Pass m Infix
infixConversion context atom rec' =
  case atom of
    Sexp.P (Bind.Other inf) _ -> do
      grouped <- groupInfix context inf
      case Shunt.shunt grouped of
        Right shunted ->
          rec' (convertShunt shunted)
        Left (Shunt.Clash pred1 pred2) ->
          Env.throwSexp (Env.Clash pred1 pred2)
        Left Shunt.MoreEles ->
          Env.throwSexp Env.ImpossibleMoreEles
    _ -> Env.handleAtom context atom rec' >>| Sexp.Atom

```

The trick of the more context aware passes is the notion of the `Bind`
module. Since passes that rely on the semantics of expressions that
come before it, it is not possible to generically process syntax
without know the language's `special forms`. These are the base forms
that fundamentally change semantics in the way that code walkers ought
to respect. An [Example list can be found for CL](https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node59.html).

For our language ours looks like

```haskell
data BinderPlus a
  = Other a
  | Lambda
      { binderPlusArgs :: Sexp.B (BinderPlus a),
        binderPlusBody :: Sexp.B (BinderPlus a)
      }
  | Declaim
      { binderPlusClaim :: Sexp.B (BinderPlus a),
        binderPlusBody :: Sexp.B (BinderPlus a)
      }
  | LetMatch
...
```

The list goes on. The `Other` sum type is the data that you the
programmer cares about. Thus using the `Sexp` library, one can
deseralize all the special forms of the language, handle them
generically with `handleAtom`, and then continue processing the form
they actually care about in the `Other`.

There are a few other parts to note, namely the `rec'` parameter, the
`Infix` data type, and the `mkTrans/mkPass` abstractions. the `rec'`
is there, so that when the pass returns, it will be run on the
returned form. This is exactly like how macros are applied in a LISP
like language. The `Infix` data structure is interesting, as we don't
automatically derive it's serializer and deserializer instead we write

```haskell

infixRename :: Sexp.Options
infixRename =
  Sexp.changeName
    (Sexp.defaultOptions @Infix)
    (Map.fromList [("InfixNoMore", ":infix")])

instance Sexp.DefaultOptions Infix

instance Sexp.Serialize Infix where
  serialize = Sexp.serializeOpt infixRename
  deserialize = Sexp.deserializeOpt infixRename
```

Which is to say, we make `InfixNoMore` share the deserializer name of
`Infix`, so that the serializer will try `Infix` first, when that
fails it will fall over to constructing an `InfixiNoMore`. This
automatically puts our code into an easy way of grouping Infix's into
a list.

```haskell
data InfFlat a = Inf NameSymbol.T | Ele a

infixToInfFlat ::
  Infix -> NonEmpty (InfFlat (Sexp.B (Bind.BinderPlus Infix)))
infixToInfFlat (InfixNoMore op lt rt) =
  NonEmpty.fromList [Ele lt, Inf op, Ele rt]
infixToInfFlat (Infix op lt rt) =
  NonEmpty.fromList [Ele lt, Inf op] <> infixToInfFlat rt

groupInfix ::
  (Env.ErrS f, Env.HasClosure f) =>
  Context.T ->
  Infix ->
  f (NonEmpty (Shunt.PredOrEle NameSymbol.T (Sexp.B (Bind.BinderPlus Infix))))
groupInfix context inf =
  traverse f (infixToInfFlat inf)
  where
    f (Ele a) = pure (Shunt.Ele a)
    f (Inf op) = do
      prec <- Env.lookupPrecedence op context
      pure (Shunt.Precedence (precedenceConversion op prec))
```


The exact details don't matter, we essentially just get it into a
format that [the shunt yard module](https://github.com/mariari/Misc-ML-Scripts/blob/master/Haskell/shunt.hs) would like.

Lastly, we have the `mkTrans/mkPass`. These are just abstractions
which help moves the Berlin Pipeline into being more specific for the
task. This is similar to how the [Meta Object Protocol](https://en.wikipedia.org/wiki/Metaobject) allowed various
companies to mold the behavior of the object system to their tastes.

```haskell
mkTrans ::
  NameSymbol.T ->
  -- | The name of the transform to be traced
  ( Context.T ->
    Sexp.T ->
    Env.MinimalMIO Sexp.T
  ) ->
  Automation.SimplifiedPassArgument ->
  Env.MinimalMIO Automation.Job
mkTrans name trans simplify =
  Trace.withScope scopeName [show (simplify ^. current)] $
    trans (simplify ^. context) (simplify ^. current)
      >>| (`Automation.ProcessNoEnv` [])
      >>| Automation.ProcessJob
  where
    scopeName = "Context" <> name <> "trans"

mkPass ::
  NameSymbol.T ->
  -- | The name of the transform to be traced
  ( Automation.SimplifiedPassArgument ->
    Env.MinimalMIO Automation.Job
  ) ->
  Step.Named
mkPass name trans =
  ( Trace.withScope
      runnerName
      []
      . simplifiedTrans
  )
    |> Automation.runSimplifiedPass
    |> Step.T
    |> Step.namePass passName
  where
    simplifiedTrans = Automation.simplify trans
    runnerName = passName <> "runner"
    passName = "Context" <> name
```

Overall we can see a few things.

1. The insistence on S-expressions has been a boon in that generic
   special forms can be handled specially and done uniformly
   throughout the codebase without having to repeat the logic for
   different ADT's.
   - Further safety is not lost, as we still have exhaustion on the
     special forms if we decide to add a new one!
2. After abstracting out common pass infrastructure, the boilerplate
   even for a context aware pass is less than 12 lines of code.
3. The boilerplate around the API is fixed for any processing job.

### A non processing example.

This pass was not included with the compiler unlike the other two
passes, however it was included with the original Berlin Pipeline
proposal, and shows off the more conceptually complex `Update` job.

What this means, is that the processing is generally much more
laborious than it ought to be, however the example is still quite
illustrative.

Let our running example be
```lisp
;; type foo = Bar int int | Baz int int
(:type foo ()
  (Bar int int)
  (Baz int int))
```

Let us first try to compile this down like
```lisp
(:declare-type foo ())

(:constructor Bar foo int int)

(:constructor Baz foo int int)
```

We can use the new-pipeline like so

```haskell
deconstructPass :: Pipeline.Step.Named
deconstructPass =
  Step.name_pass
    (Automation.runSimplifiedPass deconstructType)
    "Desugar.deocnstruct-type"

deocnstructType Automation.{current} =
  Trace.with "Desugar.deconstruct-type" [show current] $
    Sexp.foldSearchPredWithExtra
      (Automation.simplify f) (== Structure.typeName) current
    |> \Sexp.Extra{data, extra} ->
       Automation.PrcoessJob {current = data, newForms = extra}
  where
    f car cdr =
      Trace.with "Desugar.deconstruct-type-pass" [show car, show cdr] $
        case Structure.toType (car Sexp.:> cdr) of
          Just typ ->
            let extraData =
              (typ ^. body)
               >>= (\case Structure.Sum {name, arguments} ->
                            Structure.Constructor name (typ ^. name) arguments
                            |> pure
                          _ -> []
                   )
               >>| \x -> (Automation.Current, Structure.fromConstructor x)

            let currentForm =
               Structure.DeclareType (typ ^. name) (typ ^. arguments)
               |> Structure.fromDeclareType

            Structure.Extra {data = currentForm, extra = extraData}
          Nothing ->
           -- we get back a Failure on the ComputationResult
           throw @"failure" (ComputationResult.InvalidForm (car Sexp.:> cdr))
```


`deconstructorPass` is the pass itself, that we can directly
register. the implementation is held in the function
`deconstructType`. We show in gory details how precisely this pass
works. we search for a certain structure, using the normal `Sexp`
functions, once it's found we run our pass which we call `f`.

`f` now constructs two pieces of data, the current form to return,
along with any new forms which are defined from this. We construct the
extra forms by checking if they are sum types, if they are, then great
they are a constructor. We then take the constructors and say that we
will compile it `Current`ly along with the `declaration-type`. Note
that if we wanted to compile these ahead of time we could simply
replace `Current` with `Eval`. In this case, it makes more sense to
compile them `Current`ly instead of ahead of time. As for the current
form, this gets transformed into a generic declaration with the
arguments in tact, and given back as the current expression. Finally,
the `FromEvalTopToCurrent` uses the `current_step_name` discussed in
the ##Pipeline Section, to run the compiler up until the current point
and then work with them `Current`ly together.

in 25 lines we have an entire new compiler pass! That may seem like a
lot of lines, however most of them are simply type conversion to type
safe versions in which to do processing on, or shifting one data type
to another.

## Berlin Pipeline: Extensions

### Injection Passes

In short the Injection passes are a way to register pipeline passes
that run on every step. For both passes that take singular
`env_or_sexp` through `Automation`, and through passes that ought to
be run befroe and after the `Step.t` type.

To view the issue on inspiration and the state of this extension
please read

The signature laid out to do this work is as follows

The siganture of these functions are

```ocaml
open Pipeline

val Env.registerBeforePass : Automation.pass_argument -> m Automation.job
val Env.registerAfterPass  : Automation.pass_argument -> m Automation.job

val Env.register_before_pass_entireList : computational_input -> m computational_output
val Env.register_after_pass_entireList  : comptuational_input -> m computational_output
```

As for us the `Env.register*Pass` variants will run on individual
s-expressions, and will run before or after the pass itself. When one
runs `Automation.aplySimplifiedPass` that function will automatically
apply the registered functions.

the `Env.rgister*PassEntireList` variants will run before or after
each step at the eval level. Thus eval will handle running these
registered steps before and after each step is called.



The implementation of this requires tweaking where functions are
stored, since we want to tweak `surroudning_environemnt` to be the
following

```ocaml
  type around = Before | After

  type surrounding_environment = {
    (* This will be taken as a read value *)
    (** The [current_step_name] represents the current running step name *)
    current_step_name : NameSymbol.t option;
    meta_information  : Meta.t;
    on_single_pass    : (around * Automation.pass_argument -> Automation.pass_argument aroundIO) list
  }

  type around_env = {
    trace    : Trace.t;
    feedback : Feedback.t;
  }

  type aroundEnvIO a = MinIO {run : Sexp.t (IO.t around_env State.t) Except.t}
```

Since this requires values in `Pipeline` to have information about
`Pipeline.Automation`, we must remove all data declarations in
`Pipeline.Automation`, and put them in `Pipeline`, rexporting them in
`Pipeline.Automation`
