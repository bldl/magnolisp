#lang scribble/doc
@(require scribble/eval scribble/manual
	  (for-label magnolisp/runtime
                     (except-in racket/base do #%module-begin)
                     syntax/modresolve))

@(begin ;; trick from Racket docs
   (define-syntax-rule (bind id-1 id-2)
     (begin
       (require (for-label racket/base))
       (define id-1 (racket do))
       (define id-2 (racket #%module-begin))))
   (bind racket-do racket-module-begin))

@(define (warning . str)
   (list "(" (italic "Warning: ") str ")"))

@(define the-eval (make-base-eval))
@(the-eval '(require magnolisp/reader-ext magnolisp/runtime))
@;(the-eval '(current-readtable magnolisp-readtable))

@title{Magnolisp}

@author["Tero Hasu"]

This is a work-in-progress implementation of @deftech{Magnolisp}, a small, experimental language and implementation. It is experimental in its implementation technique, which is to replace the phase level 0 (runtime) language of @link["http://racket-lang.org/"]{Racket} with something non-Racket (here: Magnolisp), and translate it into another language (here: C++) for execution.

Magnolisp is an amalgamation of Racket and the likewise experimental programming language @link["http://magnolia-lang.org/"]{Magnolia}. Its algebraic language is inspired by Magnolia (or a subset thereof), but adapted for a more natural fit with Racket. Racket provides the module and macro systems. Magnolia is a good fit for C++ translation as it is designed for natural and efficient mapping to most mainstream languages.

@section{Magnolisp the Language}

@defmodulelang[magnolisp]

The Magnolisp language relies on Racket for its module and macro systems. All of Racket may be used for macro programming. The @racketmodname[racket/base] language is provided by default for phase level 1.

The @racketmodname[racket/base] definitions (with the exception of the @racket-do form) are also available at phase level 0 by default. They may also be used in runtime code, and evaluated as @racketmodname[magnolisp]. However, only a small subset of Racket can be handled by the Magnolisp compiler, and the compiler will report errors as appropriate for uncompilable language.

When a @racketmodname[magnolisp] module is evaluated as Racket, any module top-level runtime expressions will also get evaluated; this feature is intended to facilitate testing during development. The Magnolisp compiler, on the other hand, discards top-level expressions, and also any top-level definitions that are not actually part of the program being compiled. One motivation for making most of the @racketmodname[racket/base] bindings available for runtime code is their potential usefulness in code invoked by top-level expressions.

@subsection{Modules and Macros}

The Racket @racket[provide] and @racket[require] forms may be used as normal, also at phase level 0. However, as far as C++ compilation is concerned, these are only used to connect together Magnolisp definitions internally to the compiled program/library. C++ imports and exports are specified separately using the @racketid[foreign] and @racketid[export] annotations.

@warning{Locally scoped @racket[require] directives are not supported for phase level 0 code at present.}

@warning{When a macro expands to an un@racket[provide]d identifier at phase level 0, it is presently necessary to ensure that the location information of the identifier syntax object correctly identifies the source module of the binding.}

For defining macros and doing transformation time computation, the relevant Racket facilities (e.g., @racket[define-syntax], @racket[define-syntax-rule], @racket[begin-for-syntax], @etc) may be used as normal.

@subsection{Defining Forms}

@(defmodule magnolisp/runtime)

In Magnolisp, it is possible to declare @racket[function]s, types (with @racket[typedef]), and @racket[var]iables; of these, variable definitions are not allowed at the top level. The Magnolisp binding forms are in the @racketmodname[magnolisp/runtime] library. The @racketmodname[magnolisp] language provides @racketmodname[magnolisp/runtime] at phase level 0.

As Magnolisp has no standard library, it is ultimately necessary to define primitive types and functions (flagged as @racketid[foreign]) in order to be able to compile programs that do anything interesting.

@defform/subs[(function (id arg ...) maybe-annos maybe-body)
              ([maybe-body code:blank expr])]{
Declares a function. The (optional) body of a function is a single expression, which must produce a single value. @warning{Local functions are not supported by the C++ back end yet.}

Providing a body is optional in the case where the function is declared as @racket[foreign], in which case the compiler will ignore any body @racket[expr]. When a function without a body is invoked as Racket, the result is @|void-const|. When a @racket[foreign] function with a body is invoked as Racket, the body may be implemented in full Racket, typically to ``simulate'' the behavior of the C++ implementation.

A function with the @racket[export] flag in its annotations indicates that the function is part of the public API of a program that includes the containing module. When a function is used merely as a dependency, any @racket[export] flag is ignored.

When a function includes a @racketid[type] annotation, the type expression must be of the form @racket[_fn-type-expr].

For example:
@(racketblock+eval #:eval the-eval
  (function (identity x) 
    x)
  (function (five) (#:annos export (type (fn int)))
    5)
  (function (inc x) (#:annos foreign (type (fn int int)))
    (add1 x)))

Here, @racketid[identity] must have a single, concerete type, possible to determine from the context of use. It is not a generic function, and hence it may not be used in different type contexts.}

@defform[(typedef id maybe-annos)]{
Declares a type. Presently only foreign types may be declared, and @racket[id] gives the corresponding Magnolisp name. The @racketid[foreign] annotation should always be provided.

For example:
@(racketblock+eval #:eval the-eval
  (typedef int (#:annos foreign))
  (typedef long (#:annos (foreign cxx_long))))
}

@defform[(var id maybe-annos expr)]{
Declares a local variable with the name @racket[id], and the (initial) value given by @racket[expr]. A @racketid[type] annotation may be included to specify the Magnolisp type of the variable.

For example:
@(interaction #:eval the-eval
  (let ()
    (var x (#:annos (type int)) 5)
    (add1 x)))
}

@defform[(let-var id maybe-annos val-expr body)]{
A shorthand for declaring a single, annotated, locally scoped variable. The variable @racket[id] with the initial value given by @racket[val-expr] is only in the scope of the @racket[body] expression. Where no annotations are given, this form is equivalent to @racket[(let ((id val-expr)) body)]. With or without annotations, this form is semantically equivalent to the expression @racket[(do (var id maybe-annos val-expr) (return body))], provided that @racket[id] does not appear in @racket[val-expr].

For example:
@(interaction #:eval the-eval
  (let-var x (#:annos (type int)) 5
    (add1 x)))
}

@subsection{Annotations}

@(declare-exporting magnolisp/runtime)

@racketgrammar*[
#:literals (export foreign type)
[maybe-annos code:blank (#:annos anno-expr ...)]
[anno-expr export-anno-expr foreign-anno-expr type-anno-expr ...]
[export-anno-expr export (export C++-id)]
[foreign-anno-expr foreign (foreign C++-id)]
[type-anno-expr (type type-expr)]]

where:

@specsubform[C++-id]{
A valid C++ identifier. When not provided, a default C++ name is automatically derived from the Magnolisp name. For @racket[foreign] declarations, the C++ identifier must naturally match that of an existing C++ definition.}

The set of annotations that may be used in Magnolisp is open ended, to allow for additional tools support. Only the most central Magnolisp-compiler-recognized annotations are included in the above grammar.

It is not always necessary to explicitly specify a @racket[type] for a typed Magnolisp definition, as the Magnolisp compiler does whole-program type inference (in Hindley-Milner style). When evaluating as Racket, @racket[type] annotations are not used at all.

For convenience, the @racketmodname[magnolisp] language installs a reader extension that supports annotation related shorthands: @litchar{#an}@racket[(_anno-expr ...)] is short for @racket[(#:annos _anno-expr ...)]; and @litchar{^}@racket[_type-expr] is short for @racket[(type _type-expr)]. For example, @litchar{#an}(@litchar{^}@racketidfont{int}) reads as @racket[(#:annos (type int))].

@defform[(lit-of type-expr literal-expr)]{
Annotates a literal, which by themselves are untyped in Magnolisp. While the literal @racket["foo"] is treated as a @racket[string?] value by Racket, the Magnolisp compiler will expect to determine the literal expression's Magnolisp type based on annotations. The @racket[lit-of] form allows one to ``cast'' a literal to a specific type for the compiler.

For example:
@(interaction #:eval the-eval (lit-of int 5))
}

@defform[(anno! id anno-expr ...)]{
Explicitly annotates the identifier @racket[id] with the specified annotations. May be used to specify annotations for an identifier that is bound separately, probably by one of the Racket binding forms such as @racket[define], @racket[let], @etc

For example:
@(interaction #:eval the-eval
  (begin
    (define x 5)
    (anno! x (type int))))
}

While generally only declarations require annotations, @racket[lit-of] demonstrates a specific case where it is useful to associate annotations with expressions. Simple ``data'' can be stored in syntax properties, but that approach may not be suitable for identifiers that must respect lexical scope (as appearing in type expressions). The @racket[lit-of] form deals with this by binding the literal to a fresh identifier, and annotating it with a @racketid[type] using @racket[anno!].

@subsection{Type Expressions}

@racketgrammar*[
#:literals (fn)
[type-expr type-id fn-type-expr]
[fn-type-expr (fn type-expr ... type-expr)]]

Type expressions are parsed according to the above grammar, where @racket[_type-id] must be an identifier declared with @racket[typedef]. The @racket[(fn _type-expr ... _type-expr)] form contains type expressions for arguments and the return value, in that order. A Magnolisp function always returns a single value.

@subsection{Statements and Expressions}

@(declare-exporting magnolisp/runtime)

Unlike Racket, the Magnolisp language makes a distinction between statements and expressions. Although Magnolisp supports @emph{some} of the Racket language, a given Racket construct must typically appear only in a specific context (either statement or expression context).

In Magnolisp, an @racket[if] form is either a statement or expression, depending on context. That is, depending on context the form is either @racket[(if _test-expr _then-expr _else-expr)] or @racket[(if _test-expr _then-stat _else-stat)]. The @racket[when] and @racket[unless] forms are always statements, and contain statements in their body. The truthiness of an expression depends on a type-specific C++ predicate, assumed to be named @racketvarfont{C++-id}@racketidfont{_is_true}.

A @racket[(begin _stat ...)] form, in Magnolisp, signifies a sequence of statements, itself constituting a statement.

The @racket[(let ([id val-expr] ...) body ...+)], @racket[(let* ([id val-expr] ...) body ...+)], and @racket[(letrec ([id val-expr] ...) body ...+)] forms are statements in Magnolisp, and the @racket[_body]s must likewise be statements. The named variant of @racket[let] is not supported. A limited form of @racketidfont{let} is supported in expression context---see @racket[let-var].

@racket[(void _expr ...)] is a Magnolisp statement with no effect.

@racket[var], @racket[function], and @racket[typedef] declarations may appear in a statement position. The same is true of @racket[define] forms that conform to the restricted syntax supported by the Magnolisp compiler.

@defform[(do stat ...)]{
An @deftech{expression block} containing a sequence of statements. As the term implies, and expression block is an expression, despite containing statements. The block must produce a single value by @racket[return]ing it. Control must not reach the end of a block expression---the @racket[return] statement must be invoked somewhere before control ``falls out'' of the block. The returned value becomes the value of the containing @racket[do] expression.

For example:
@(interaction #:eval the-eval
  (do (void)
      (return 1)
      (return 2)))
}

@defform[(return expr)]{
A statement that causes any enclosing @racket[do] block (which must exist) to yield the value of the expression @racket[expr].}

@section{Evaluation}

Programs written in Magnolisp can be evaluated in the usual Racket way, provided that the @(hash-lang) signature specifies the language as @racketmodname[magnolisp]. Any module top-level phase level 0 expressions are evaluated, and the results are printed (as for Racket's @racket-module-begin).

@section{Compiler API}

@defmodule[magnolisp/compiler-api]

The Magnolisp implementation includes a compiler targeting C++. The @racketmodname[magnolisp/compiler-api] library provides an API for invoking the compiler.

@deftogether[(
@defproc[(compile-modules [module-path-v module-path?] ...) compilation-state?]
@defproc[(compile-files [path-s path-string?] ...) compilation-state?]
)]{
Invoke the compiler front end for analysing a Magnolisp program, whose ``entry modules'' are specified either as module paths or files. Any specified modules that are not in the @racketmodname[magnolisp] language are effectively ignored, as they do not contain any @racket[export]ed Magnolisp definitions.
Both functions return an opaque compilation state object, which may be passed to @racket[generate-files] for code generation.

Any @racket[path-s] is mapped to a @racket[`(file ,path-s)] module path, coercing @racket[path-s] to a string if necessary.
}

@defparam[mp-root-path rel-to-path-v any/c]{
A parameter that determines the path for resolving relative module paths during compilation. Its value must be passable to @racket[resolve-module-path] as the second argument. @warning{Resolving relative module paths in file hierarchies is mostly untested, and this API is subject to change.}}

@defproc[(compilation-state? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a compilation state object (as returned by @racket[compile-modules] or @racket[compile-files]), @racket[#f] otherwise.
}

@defproc[(generate-files 
          [st compilation-state?]
          [backends (hash/c symbol? (set/c symbol? #:cmp 'eq))]
          [#:outdir outdir path-string? (current-directory)]
          [#:basename basename string? "output"]
          [#:stdout stdout? boolean? #t]
          [#:banner banner? boolean? #t])
         void?]{
Performs code generation for the program whose intermediate representation (IR) is stored in the compilation state @racket[st]. Code generation is only performed with the specified compiler back ends, and for the specified back end specific file types. For instance, to generate both a C++ header and implementation, you may pass @racket[backends] as @racket[(hasheq 'cxx (seteq 'cc 'hh))]. Passing @racket[stdout?] as @racket[#t] causes code generation to standard output. When @racket[stdout?] is @racket[#t], the @racket[banner?] argument indicates whether banners (with filenames) should be printed to precede individual output files. When @racket[stdout?] is @racket[#f], the @racket[outdir] argument specifies the output directory for generated files. The @racket[basename] string is used as the ``stem'' for output file names.
}

@; The C++ backend depends on @exec{uncrustify} as an external tool, and the tool requires a configuration file; one is installable using the provided @filepath{Makefile}.

@section{@exec{mglc}}

The compiler can also be invoked via the @exec{mglc} command-line tool, specifying the program to compile. The tool gets installed by invoking @exec{raco setup}. (Alternatively you may just run it as @exec{./mglc} on Unix platforms.)

To compile a program with @exec{mglc}, list the source files of the program as arguments; the program will consist of all the functions in the listed files that are annotated with the @racket[export] flag, as well as any code on which they rely. A number of compiler options affecting compilation behavior may be passed, see @exec{mglc --help} for a list.

An example invocation would be:

@commandline{mglc --stdout --banner --cxx my-program.rkt}

which instructs the compiler to print out C++ code into standard output, with banners, for the program @filepath{my-program.rkt}.

@section{License}

Except where otherwise noted, all code is authored by Tero Hasu, copyright Tero Hasu and University of Bergen, and not licensed for distribution at this time.

@(close-eval the-eval)
