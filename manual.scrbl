#lang scribble/doc
@(require scribble/eval scribble/manual "manual-util.rkt"
	  (for-label syntax/modresolve
	             magnolisp/prelude magnolisp/runtime
                     (except-in racket/base do #%module-begin)))

@(define the-eval (make-base-eval))
@(the-eval '(require magnolisp/prelude magnolisp/runtime))

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

As Magnolisp has almost no standard library, it is ultimately necessary to define primitive types and functions (flagged as @racketid[foreign]) in order to be able to compile programs that do anything useful.

@defform/subs[(function (id arg ...) maybe-annos maybe-body)
              ([maybe-body code:blank expr])]{
Declares a function. The (optional) body of a function is a single expression, which must produce a single value.

Unlike in Racket, no @emph{tail-call optimization} may be assumed even when a recursive function application appears in @emph{tail position}.

Providing a body is optional in the case where the function is declared as @racket[foreign], in which case the compiler will ignore any body @racket[expr]. When a function without a body is invoked as Racket, the result is @|void-const|. When a @racket[foreign] function with a body is invoked as Racket, the body may be implemented in full Racket, typically to ``simulate'' the behavior of the C++ implementation. To implement a function body in Racket instead of Magnolisp, enclose the body expression within a @racket[begin-racket] form.

A function with the @racket[export] flag in its annotations indicates that the function is part of the public API of a program that includes the containing module. When a function is used merely as a dependency, any @racket[export] flag is ignored.

When a function includes a @racketid[type] annotation, the type expression must be of the form @racket[_fn-type-expr].

For example:
@(racketblock+eval #:eval the-eval
  (function (identity x) 
    x)
  (function (five) (#:annos export (type (fn int)))
    5)
  (function (inc x) (#:annos foreign (type (fn int int)))
    (add1 x))
  (function (seven) (#:annos foreign (type (fn int)))
    (begin-racket 1 2 3 4 5 6 7)))

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

Type expressions are parsed according to the above grammar, where @racket[_type-id] must be an identifier that names a type. The only built-in type is @racket[predicate], and any others must be declared using @racket[typedef]. The @racket[(fn _type-expr ... _type-expr)] form contains type expressions for arguments and the return value, in that order. A Magnolisp function always returns a single value.

@subsection{Statements and Expressions}

@(declare-exporting magnolisp/runtime)

Unlike Racket, the Magnolisp language makes a distinction between statements and expressions. Although Magnolisp supports @emph{some} of the Racket language, a given Racket construct must typically appear only in a specific context (either statement or expression context).

In Magnolisp, an @racket[if] form is either a statement or expression, depending on context. That is, depending on context the form is either @racket[(if _test-expr _then-expr _else-expr)] or @racket[(if _test-expr _then-stat _else-stat)]. The @racket[when] and @racket[unless] forms are always statements, and contain statements in their body. The @racket[_test-expr] conditional expression must always be of type @racket[predicate], and whether it holds depends on the ``truthiness'' of its value, as interpreted in C++ or Racket (as applicable).

A @racket[(begin _stat ...)] form, in Magnolisp, signifies a sequence of statements, itself constituting a statement.

The @racket[(let ([_id _expr] ...) _body ...+)], @racket[(let* ([_id _expr] ...) _body ...+)], and @racket[(letrec ([_id _expr] ...) _body ...+)] forms are statements in Magnolisp, and the @racket[_body]s must likewise be statements. The named variant of @racket[let] is not supported. A limited form of @racketidfont{let} is supported in expression context---see @racket[let-var].

The @racket[(set! _id _expr)] form is an assignment statement in Magnolisp.
The left-hand side expression @racket[_id] must be a reference to a bound variable.
(The @racket[_id] may naturally instead be a transformer binding to an assignment transformer, in which case the form is macro transformed as normal.)

In Magnolisp, @racket[(void)] signifies a statement with no effect. Unlike in Racket, arguments are not allowed. The @racket[(values)] form likewise signifies a statement with no effect, when it appears in a statement position. The two differ only when evaluating as Racket, as the former may only appear in a 1-value context, and the latter in a 0-value context.

The @racket[var], @racket[function], and @racket[typedef] declaration forms may appear in a statement position. The same is true of @racket[define] forms that conform to the restricted syntax supported by the Magnolisp compiler.

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

@subsection{Predicate Expressions}

@(defmodule magnolisp/prelude)

A @deftech{predicate expression} is simply an expression of type @racket[predicate], which is the only built-in type in Magnolisp.

@defthing[#:kind "type" predicate any/c]{
A built-in type. The ``literals'' of this type are @racket[true] and @racket[false]. All conditional expressions in Magnolisp are of this type.
}

@deftogether[(
@defproc[(TRUE) #t]
@defproc[(FALSE) #f]
)]{
The only built-in (primitive) functions in Magnolisp are @racket[TRUE] and @racket[FALSE], which are both of type @racket[(fn predicate)]. While @racket[TRUE] and @racket[FALSE] are built-in, only Racket implementations are provided; suitable implementations must be provided for C++ as necessary, named @racketidfont{mgl_predicate}, @racketidfont{mgl_TRUE}, and @racketidfont{mgl_FALSE}, respectively. The expression @racket[(TRUE)] is expected to always evaluate to a true value, and @racket[(FALSE)] is expected to always evaluate to a false value; in Racket these evaluate to @racket[#t] and @racket[#f], respectively.}

@deftogether[(
@defidform[true]
@defidform[false]
)]{
There is also shorthand syntax @racket[true] and @racket[false]; said syntaxes expand to @racket[(TRUE)] and  @racket[(FALSE)], respectively.}

@subsection{Racket Forms}

@(declare-exporting magnolisp/runtime)

To use Racket code in a runtime context, you may wrap the code in a form that indicates that the code is only intended for evaluation as Racket. 
Code so wrapped must be grammatically correct Racket, but not necessarily Magnolisp. The wrapping forms merely switch syntaxes, and have no effect on the namespace used for evaluating the enclosed sub-forms; the surrounding namespace is still in effect. Nesting of the wrapping forms is allowed.

@defform[(begin-racket Racket-form ...)]{
A Racket expression that is equivalent to writing @racket[(let () Racket-form ...)]. The Magnolisp semantics is to: ignore such forms when at module top-level; treat such forms as no-ops in statement context; and treat them as uncompilable expressions when appearing in an expression position. Uncompilable expressions are acceptable for as long as they are not part of a compiled program, or can be optimized away.

For example:
@(interaction #:eval the-eval
   (function (three) (#:annos foreign (type fn int))
     (begin-racket 
       (define x 1) 
       (set! x (begin 2 3)) 
       x))
   (three))
}

@defform[(begin-for-racket Racket-form ...)]{
Like @racket[begin-racket], but equivalent to writing @racket[(begin Racket-form ...)], and hence not necessarily a Racket expression. Intended particularly for allowing the splicing of Racket definitions into the enclosing context, which is not possible with @racket[begin-racket].

For example:
@(interaction #:eval the-eval
   (begin-for-racket
     (define six 6)
     (define (one-more x) (let dummy () (+ x 1))))
   (function (eight) (#:annos foreign (type fn int))
     (one-more (one-more six)))
   (eight))
}

@defform[(define-for-racket rest ...)]{
Shorthand for writing @racket[(begin-for-racket (define rest ...))]. Intended for introducing a single binding into the enclosing context, with a definition given in the Racket language.

For example:
@(interaction #:eval the-eval
   (define-for-racket two (begin 1 2))
   (function (four) (#:annos foreign (type fn int))
     (begin-racket (* (begin 1 2) two)))
   (four))
}

@subsection{Fully Expanded Programs}

@(declare-exporting magnolisp/runtime)

As far as the Magnolisp compiler is concerned, a Magnolisp program is fully expanded if it conforms to the following grammar.

Any non-terminal marked with the subscript ``rkt'' is as documented in the ``Fully Expanded Programs'' section of The Racket Reference. Any non-terminal marked with the subscript ``ign'' is for language that is ignored by the Magnolisp compiler, but which may be useful when evaluating as Racket. Anything of the form @(indirect-id id) is actually a non-terminal like @racketvarfont{id-expr}, but for the specific identifier @racketvarfont{id}. Form @racket[(#,(stxprop-flag local-ec) _sub-form ...)] means the form @racket[(_sub-form ...)] whose syntax object has the property @racket['local-ec] set to a true value.

@racketgrammar*[
#:literals (begin begin-for-syntax call/ec define-values define-syntaxes if let-values letrec-values letrec-syntaxes+values quote set! values void #%expression #%magnolisp #%plain-app #%plain-lambda #%provide #%require #%top)
[module-begin-form (#,racket-module-begin mgl-modlv-form ...)]
[mgl-modlv-form (#%provide #,(rkt-nt raw-provide-spec) ...)
		(#%require #,(rkt-nt raw-require-spec) ...)
		#,(rkt-ign-nt submodule-form)
                (begin mgl-modlv-form ...)
		#,(ign-nt begin-for-syntax-form)
                module-level-def
                #,(ign-nt define-syntaxes-form)
                #,(rkt-ign-nt expr)
                #,(ign-nt in-racket-form)]
[begin-for-syntax-form (begin-for-syntax #,(rkt-nt module-level-form) ...)]
[define-syntaxes-form (define-syntaxes (trans-id ...) #,(rkt-nt expr))]
[module-level-def (define-values (id) mgl-expr)
		  (define-values (id ...) 
                    (#%plain-app #,(indirect-id values) mgl-expr ...))]
[mgl-expr id
          (#%plain-lambda (id ...) mgl-expr)
	  (if mgl-expr mgl-expr mgl-expr)
	  (let-values ([(id) mgl-expr]) mgl-expr)
	  (letrec-syntaxes+values
              ([(trans-id ...) #,(rkt-ign-nt expr)] ...)
              ([(id) mgl-expr])
            mgl-expr)
	  #,(racket (#,(racket quote) _datum))
          local-ec-expr
	  (#%plain-app #,(indirect-id #%magnolisp) (quote foreign-type))
	  (#%plain-app id-expr mgl-expr ...)
          (#%top . id)
          (#%expression mgl-expr)
          in-racket-form]
[stat (if mgl-expr stat stat)
      (begin stat ...)
      (let-values ([(id ...) mgl-expr] ...)
        stat ...+)
      (letrec-values ([(id ...) mgl-expr] ...)
        stat ...+)
      (letrec-syntaxes+values
          ([(trans-id ...) #,(rkt-ign-nt expr)] ...)
          ([(id ...) mgl-expr] ...)
        stat ...+)
      (set! id mgl-expr)
      (#%plain-app #,(indirect-id values))
      (#%plain-app #,(indirect-id void))
      local-ec-jump
      (#%expression stat)
      in-racket-form]
[local-ec-expr (#,(stxprop-flag local-ec) #%plain-app #,(indirect-id call/ec)
                 (#%plain-lambda (id) stat ...))]
[local-ec-jump (#,(stxprop-flag local-ec) #%plain-app id-expr mgl-expr)]
[id-expr id (#%top . id) (#%expression id-expr)]
]

where:

@specsubform[id]{
An identifier. Not @emph{the} reserved @racket[#%magnolisp] identifier.}

@specsubform[trans-id]{
An identifier with a @emph{transformer binding}.}

@specsubform[datum]{
A piece of literal data. A @(racket (#,(racket quote) _datum)) form is a literal in Magnolisp, and its type must be possible to infer from context.}

@specsubform[in-racket-form]{
Any Racket form that has the syntax property @racket['in-racket] set to a true value. These are ignored by the Magnolisp compiler where possible, and it is an error if they persist in contexts where they ultimately cannot be ignored. The @racket[begin-racket] and @racket[begin-for-racket] forms are implemented through this mechanism.}

@specsubform[local-ec-expr]{
A restricted form of @racket[call/ec] invocation, which is flagged with the syntax property @racket['local-ec]. The semantic restriction is that non-local escapes (beyond the enclosing function's body) are not allowed.}

@specsubform[local-ec-jump]{
A restricted form of escape continuation invocation, flagged with the syntax property @racket['local-ec]. The escape must be local.}

@warning{The parsing of type declarations is not presently as permissive as the above grammar indicates. It more or less assumes syntax as produced by @racket[typedef].}

@warning{For some of the @(indirect-id id) non-terminals, the current parser actually assumes a direct @racket[_id].}

@defthing[#%magnolisp any/c]{
A value binding whose identifier is used to uniquely identify some Magnolisp core syntactic forms. The value of the variable does not matter when compiling an Magnolisp, as it is never used. For purposes of evaluating as Racket, it holds some function that may be applied to any number of arguments, and which produces a single, undefined value.}

@section{Evaluation}

Programs written in Magnolisp can be evaluated in the usual Racket way, provided that the @(hash-lang) signature specifies the language as @racketmodname[magnolisp]. Any module top-level phase level 0 expressions are evaluated, and the results are printed (as for Racket's @racket-module-begin).

@section{Compiler API}

@defmodule[magnolisp/compiler-api]

The Magnolisp implementation includes a compiler targeting C++. The @racketmodname[magnolisp/compiler-api] library provides an API for invoking the compiler.

@deftogether[(
@defproc[(compile-modules 
          [module-path-v module-path?] ...
          [#:relative-to rel-to-path-v 
                         (or/c path-string? (-> any) false/c) #f])
         compilation-state?]
@defproc[(compile-files [path-s path-string?] ...) compilation-state?]
)]{
Invoke the compiler front end for analysing a Magnolisp program, whose ``entry modules'' are specified either as module paths or files. Any specified modules that are not in the @racketmodname[magnolisp] language are effectively ignored, as they do not contain any @racket[export]ed Magnolisp definitions.
Both functions return an opaque compilation state object, which may be passed to @racket[generate-files] for code generation.

The optional argument @racket[rel-to-path-v] is as for @racket[resolve-module-path]. It is only relevant for relative module paths, and indicates to which path such paths should be considered relative.

Any @racket[path-s] is mapped to a @racket[`(file ,path-s)] module path, coercing @racket[path-s] to a string if necessary.
}

@defproc[(compilation-state? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a compilation state object (as returned by @racket[compile-modules] or @racket[compile-files]), @racket[#f] otherwise.
}

@defproc[(generate-files 
          [st compilation-state?]
          [backends (listof (cons/c symbol? any/c))]
          [#:outdir outdir path-string? (current-directory)]
          [#:basename basename string? "output"]
          [#:out out output-port? (current-output-port)]
          [#:banner banner? boolean? #t])
         void?]{
Performs code generation for the program whose intermediate representation (IR) is stored in the compilation state @racket[st]. Code generation is only performed with the specified compiler back ends, and for the specified back end specific file types. For instance, to generate both a C++ header and implementation, you may pass @racket[backends] as @racket['((cxx (cc hh)))]. The @racket[backends] argument is an association list with one entry per backend. Passing @racket[out] as @racket[#f] causes code generation into (separate) files; otherwise the specified output port is used. When @racket[out] is a true value, the @racket[banner?] argument indicates whether banners (with filenames) should be printed to precede individual output files. When @racket[out] is @racket[#f], the @racket[outdir] argument specifies the output directory for generated files. The @racket[basename] string is used as the ``stem'' for output file names.
}

@; The C++ backend depends on @exec{uncrustify} as an external tool, and the tool requires a configuration file; one is installable using the provided @filepath{Makefile}.

@section{@exec{mglc}}

The compiler can also be invoked via the @exec{mglc} command-line tool, specifying the program to compile. The tool gets installed by invoking @exec{raco setup}. (Alternatively you may just run it as @exec{./mglc} on Unix platforms.)

To compile a program with @exec{mglc}, list the source files of the program as arguments; the program will consist of all the functions in the listed files that are annotated with the @racket[export] flag, as well as any code on which they rely. A number of compiler options affecting compilation behavior may be passed, see @exec{mglc --help} for a list.

An example invocation would be:

@commandline{mglc --stdout --banner --cxx my-program.rkt}

which instructs the compiler to print out C++ code into standard output, with banners, for the program @filepath{my-program.rkt}.

@section{Example Code}

For sample Magnolisp programs, see the @filepath{test-*.rkt} files in the @filepath{tests} directory of the Magnolisp implementation codebase.

@section{License}

Except where otherwise noted, all code is authored by Tero Hasu, copyright Tero Hasu and University of Bergen, and not licensed for distribution at this time.

@(close-eval the-eval)
