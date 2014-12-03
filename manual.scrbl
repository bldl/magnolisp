#lang scribble/manual
@(require scribble/eval "manual-util.rkt"
	  (for-label syntax/modresolve
	             magnolisp/prelude magnolisp/surface
                     (except-in racket/base do #%module-begin)))

@(define the-eval (make-base-eval))
@(the-eval '(require magnolisp/prelude magnolisp/surface))

@title{Magnolisp}

@author["Tero Hasu"]

This is @deftech{Magnolisp}, a small, experimental language and implementation. It is experimental in its implementation technique, which is to replace the phase level 0 (runtime) language of @link["http://racket-lang.org/"]{Racket} with something non-Racket (here: Magnolisp), and translate it into another language (here: C++) for execution.

Magnolisp is inspired by Racket and the likewise experimental programming language @link["http://magnolia-lang.org/"]{Magnolia}. Its algebraic language resembles Racket's, and Racket also provides the module and macro systems. The language is restricted in ways similar to Magnolia, with the restrictions designed to support static reasoning about code, and to allow for fairly direct mapping to most mainstream languages.

Magnolisp is intended to explore and demonstrate techniques for source-to-source compilation on top of Racket, not to support writing of useful applications.

@section{Magnolisp the Language}

@defmodulelang[magnolisp]

The Magnolisp language relies on Racket for its module and macro systems. All of Racket may be used for macro programming. The @racketmodname[racket/base] language is provided by default for phase level 1 (compile time).

The @racketmodname[racket/base] definitions (with the exception of the @racket-do form) are also available at phase level 0 by default. They may also be used in runtime code, and evaluated as @racketmodname[magnolisp]. However, only a small subset of Racket can be handled by the Magnolisp compiler, and either the Magnolisp Racket language or the Magnolisp compiler will report errors as appropriate for uncompilable language.

When a @racketmodname[magnolisp] module is evaluated as Racket, any module top-level runtime expressions will also get evaluated; this feature is intended to facilitate testing during development. The Magnolisp compiler, on the other hand, discards top-level expressions, and also any top-level definitions that are not actually part of the program being compiled. One motivation for making most of the @racketmodname[racket/base] bindings available for runtime code is their potential usefulness in code invoked by top-level expressions.

@subsection{Modules and Macros}

The Racket @racket[provide] and @racket[require] forms may be used as normal, also at phase level 0. However, as far as C++ compilation is concerned, these are only used to connect together Magnolisp definitions internally to the compiled program/library. C++ imports and exports are specified separately using the @racket[foreign] and @racket[export] annotations.

For defining macros and macro-expansion time computation, the relevant Racket facilities (e.g., @racket[define-syntax], @racket[define-syntax-rule], @racket[begin-for-syntax], @etc) may be used as normal.

@subsection{Defining Forms}

@(defmodule magnolisp/surface)

In Magnolisp, it is possible to declare @racket[function]s, types (with @racket[typedef]), and @racket[var]iables; of these, variable definitions are not allowed at the top level. The Magnolisp binding forms are in the @racketmodname[magnolisp/surface] library. The @racketmodname[magnolisp] language provides @racketmodname[magnolisp/surface] at phase level 0.

As Magnolisp has almost no standard library, it is ultimately necessary to define primitive types and functions (flagged as @racket[foreign]) in order to be able to compile programs that do anything useful.

@defform/subs[(function (id arg ...) maybe-annos maybe-body)
              ([maybe-body code:blank expr])]{
Declares a function. The (optional) body of a function is a single expression, which must produce a single value.

Unlike in Racket, no tail-call elimination may be assumed even when a recursive function application appears in @emph{tail position}.

Providing a body is optional in the case where the function is declared as @racket[foreign], in which case the compiler will ignore any body @racket[expr]. When a function without a body is invoked as Racket, the result is @|void-const|. When a @racket[foreign] function with a body is invoked as Racket, the body may be implemented in full Racket, typically to ``simulate'' the behavior of the C++ implementation. To implement a function body in Racket syntax instead of Magnolisp syntax, enclose the body expression within a @racket[begin-racket] form.

A function with the @racket[export] flag in its annotations indicates that the function is part of the public API of a program that includes the containing module. When a function is used merely as a dependency (i.e., its containing module was not specified as being a part of the program), any @racket[export] flag is ignored.

When a function includes a @racket[type] annotation, the type expression must be of the form @racket[_fn-type-expr] (see @secref{type-expressions}).

For example:
@(racketblock+eval #:eval the-eval
  (function (identity x) 
    x)
  (function (five) (#:annos export [type (fn int)])
    5)
  (function (inc x) (#:annos foreign [type (fn int int)])
    (add1 x))
  (function (seven) (#:annos foreign [type (fn int)])
    (begin-racket 1 2 3 4 5 6 7)))

Here, @racketid[identity] must have a single, concerete type, possible to determine from the context of use. It is not a generic function, and hence it may not be used in multiple different type contexts within a single program.}

@defform[(typedef id maybe-annos)]{
Declares a type. Presently only foreign types may be declared, and @racket[id] gives the corresponding Magnolisp name. The @racket[foreign] annotation should always be provided.

For example:
@(racketblock+eval #:eval the-eval
  (typedef int (#:annos foreign))
  (typedef long (#:annos [foreign my_cxx_long])))
}

@defform[(var id maybe-annos expr)]{
Declares a local variable with the name @racket[id], and the (initial) value given by @racket[expr]. A @racket[type] annotation may be included to specify the Magnolisp type of the variable.

For example:
@(interaction #:eval the-eval
  (let ()
    (var x (#:annos [type int]) 5)
    (add1 x)))
}

@defform[(let-var id maybe-annos val-expr body)]{
A shorthand for declaring a single, annotated, locally scoped variable. The variable @racket[id] with the initial value given by @racket[val-expr] is only in the scope of the @racket[body] expression. This form is equivalent to @racket[(let ((id (let-annotate (_anno-expr ...) val-expr))) body)].

For example:
@(interaction #:eval the-eval
  (let-var x (#:annos [type int]) 5
    (add1 x)))

Where one uses other variants of @racketidfont{let}, it is still possible to specify annotations for the bindings with @racket[let-annotate].}

@subsection{Annotations}

@(declare-exporting magnolisp/surface)

@racketgrammar*[
#:literals (export foreign type)
[maybe-annos code:blank (#:annos anno-expr ...)]
[anno-expr export-anno-expr foreign-anno-expr type-anno-expr ...]
[export-anno-expr export (export C++-id)]
[foreign-anno-expr foreign (foreign C++-id)]
[type-anno-expr (type type-expr)]]

where:

@specsubform[C++-id]{
A valid C++ identifier. When not provided, a default C++ name is automatically derived from the Magnolisp name.}

The set of annotations that may be used in Magnolisp is open ended, to allow for additional tools support. Only the most central Magnolisp-compiler-recognized annotations are included in the above grammar.

It is not always necessary to explicitly specify a @racket[type] for a typed Magnolisp definition, as the Magnolisp compiler does whole-program type inference (in Hindley-Milner style). When evaluating as Racket, @racket[type] annotations are not used at all.

For convenience, the @racketmodname[magnolisp] language installs a reader extension that supports annotation related shorthands: @litchar{#an}@racket[(_anno-expr ...)] is short for @racket[(#:annos _anno-expr ...)]; @litchar{#ap}@racket[(_anno-expr ...) _expr] is short for @racket[(let-annotate (_anno-expr ...) _expr)]; and @litchar{^}@racket[_type-expr] is short for @racket[(type _type-expr)]. For example, @litchar{#an}(@litchar{^}@racketidfont{int}) reads as @racket[(#:annos (type int))].

@deftogether[(
@defform[(foreign C++-id)]
@defidform[#:link-target? #f foreign]
)]{
An annotation that marks a type or function definition as foreign. That is, it is a primitive implemented in C++. Whether explicitly specified or derived from the Magnolisp name, any @racket[C++-id] must naturally match that of an existing C++ definition.}

@deftogether[(
@defform[(export C++-id)]
@defidform[#:link-target? #f export]
)]{
An annotation that marks a function definition as ``public''. That is, the function is to be part of the Magnolisp API that is produced for the library being implemented. Its declaration will thus appear in any generated header file.}

@defform[(type type-expr)]{
An annotation that specifies the Magnolisp type of a function, variable, or expression.}

@defform[(cast type-expr expr)]{
Annotates expression @racket[expr] with the type given by @racket[type-expr]. A @racket[cast] is commonly used to specify the type of a literal, which by themselves are generally untyped in Magnolisp. While the literal @racket["foo"] is treated as a @racket[string?] value by Racket, the Magnolisp compiler will expect to determine the literal expression's Magnolisp type based on annotations. The @racket[cast] form allows one to ``cast'' an expression to a specific type for the compiler.

For example:
@(interaction #:eval the-eval
  (cast int 5))

While generally only declarations require annotations, @racket[cast] demonstrates a specific case where it is useful to associate annotations with expressions.}

@defform[(let-annotate (anno-expr ...) expr)]{
Explicitly annotates the expression @racket[expr] with the specified annotations. May be used to specify annotations for an identifier that is bound using the regular Racket binding forms such as @racket[define], @racket[let], @etc

For example:
@(interaction #:eval the-eval
  (define x (let-annotate ([type int]) 5))
  x
  (let ([x (let-annotate ([type int]) 6)])
    x))
}

@subsection[#:tag "type-expressions"]{Type Expressions}

@(declare-exporting magnolisp/surface)

@racketgrammar*[
#:literals (fn)
[type-expr type-id fn-type-expr]
[fn-type-expr (fn type-expr ... type-expr)]]

Type expressions are parsed according to the above grammar, where @racket[_type-id] must be an identifier that names a type. The only predefined types in Magnolisp are @racket[Void] and @racket[Bool], and any others must be declared using @racket[typedef].

@defform[(fn type-expr ... type-expr)]{
A function type expression, containing type expressions the function's arguments and its return value, in that order. A Magnolisp function always returns a single value.}

@subsection{Statements and Expressions}

@(declare-exporting magnolisp/surface)

Like Racket (and unlike C++), the Magnolisp language makes no distinction between statements and expressions. Some expressions yield no useful value, however; such expressions conceptually produce a result of type @racket[Void] (such result values do exist at Racket run time, but not at C++ run time). Some expressions yield @emph{no} values (or @emph{multiple} values), but are merely used as a syntactic device, and only allowed to appear in certain contexts.

Magnolisp borrows a number of constructs from Racket (or Scheme). For example, there is a conditional form @racket[(if _test-expr _then-expr _else-expr)], as well as the derived forms @racket[(when _test-expr _then-expr ...+)] and @racket[(unless _test-expr _then-expr ...+)]. The @racket[_test-expr] conditional expression must always be of type @racket[Bool], and whether it holds depends on the ``truthiness'' of its value, as interpreted in C++ or Racket (as applicable). The branches of an @racket[if] must generally be of the same type, except where the result of the @racket[if] form is discarded. The @racket[when] and @racket[unless] can generally only appear in such result-discarding contexts, as they have an implicit ``else'' branch of type @racket[Void].

A @racket[(begin _body _...)] form, in Magnolisp, signifies a sequence of expressions, itself constituting an expression. Similarly to Racket, to allow declarations to appear within an expression sequence, @racket[(let () _body _...)] should be used instead.

The @racket[(let ([_id _expr] _...) _body ...+)], @racket[(let* ([_id _expr] _...) _body ...+)], and @racket[(letrec ([_id _expr] _...) _body ...+)] forms are also available in Magnolisp, but the named variant of @racket[let] is not supported.

The @racket[(set! _id _expr)] form is likewise available in Magnolisp, supporting assignment to variables.
The left-hand side expression @racket[_id] must be a reference to a bound variable.
(The @racket[_id] may naturally instead be a transformer binding to an assignment transformer, in which case the form is macro transformed as normal.)

In Magnolisp, @racket[(void _expr _...)] is an expression with no useful result (the result is of the unit type @racket[Void]). Any arguments to @racket[void] are evaluated as usual, but they are not used. The @racket[(values)] form signifies ``nothing,'' and has no result; hence it is an error for @racket[(values)] to appear in a position where the context expects a result. In result expecting contexts, the former may only appear in a 1-value context, and the latter in a 0-value context (there are few in Magnolisp).

The @racket[var], @racket[function], and @racket[typedef] declaration forms may appear in a Racket @emph{internal-definition context} (and not Racket @emph{expression context}). The same is true of @racketidfont{define} forms that conform to the restricted syntax supported by the Magnolisp compiler.

@defform[(do expr ...)]{
An @deftech{expression block} containing a sequence of expressions. As the term implies, an expression block is itself an expression. The block must produce a single value by @racket[return]ing it, or by letting it ``fall out'' of the block. The returned or fallen-out value becomes the value of the containing @racket[do] expression. All value giving expressions in a block must be of the same type.

For example:
@(interaction #:eval the-eval
  (do (when #t
        (return (five)))
      (seven)))
}

@defform[(return expr)]{
An expression that causes any enclosing @racket[do] block (which must exist) to yield the value of the expression @racket[expr].}

@subsection{Standard Library}

@(defmodule magnolisp/prelude)

A boolean expression is simply an expression of type @racket[Bool], which is one of the two predefined types in Magnolisp. The other one is @racket[Void], which is Magnolisp's unit type (whose values carry no information).

The @racket[Bool] and @racket[Void] types are defined by the @racketmodname[magnolisp/prelude] module, which serves as the runtime library of Magnolisp. The @racketmodname[magnolisp/prelude] names are bound for phase level 0 in the @racketmodname[magnolisp] language.

@defthing[#:kind "type" Bool any/c]{
A predefined type. The literals of this type are @racket[#t] and @racket[#f]. All conditional expressions in Magnolisp are of this type. The corresponding C++ type is @racketidfont{bool}, and the corresponding C++ constant values are @racketidfont{true} and @racketidfont{false}, respectively.}

@defthing[#:kind "type" Void any/c]{
A predefined type. There are no literals for @racket[Void] values, but the Magnolisp core form @racket[(void _expr _...)] evaluates such a value, at least conceptually. Such values may not actually exist at C++ run time. The corresponding C++ type is @racketidfont{void}.}

@subsection{Racket Forms}

@(declare-exporting magnolisp/surface)

To use Racket code in a runtime context, you may wrap the code in a form that indicates that the code is only intended for evaluation as Racket. 
Code so wrapped must be grammatically correct Racket, but not necessarily Magnolisp. The wrapping forms merely switch syntaxes, and have no effect on the namespace used for evaluating the enclosed sub-forms; the surrounding namespace is still in effect. Nesting of the wrapping forms is allowed.

@defform[(begin-racket Racket-form ...)]{
A Racket expression that is equivalent to writing @racket[(let () Racket-form _...)]. The Magnolisp semantics is to: ignore such forms when at module top-level; and treat them as uncompilable expressions when appearing in an expression position. Uncompilable expressions are acceptable for as long as they are not part of a compiled program, or can be optimized away.

For example:
@(interaction #:eval the-eval
   (function (three) (#:annos foreign [type (fn int)])
     (begin-racket 
       (define x 1) 
       (set! x (begin 2 3)) 
       x))
   (three))

One use case is to @racket[local-require] a Racket definition into a context where a Magnolisp definition by the same name is being implemented. For example:

@(interaction #:eval the-eval
  (function (equal? x y) 
    (#:annos [type (fn int int Bool)] foreign)
    (begin-racket
      (local-require (only-in racket/base equal?))
      (equal? x y)))
  (equal? "foo" "foo"))
}

@defform[(begin-for-racket Racket-form ...)]{
Like @racket[begin-racket], but equivalent to writing @racket[(begin Racket-form _...)], and hence not necessarily a Racket expression. Intended particularly for allowing the splicing of Racket definitions into the enclosing context, which is not possible with @racket[begin-racket].

For example:
@(interaction #:eval the-eval
   (begin-for-racket
     (define six 6)
     (define (one-more x) (let dummy () (+ x 1))))
   (function (eight) (#:annos foreign [type (fn int)])
     (one-more (one-more six)))
   (eight))
}

@defform[(define-for-racket rest ...)]{
Shorthand for writing @racket[(begin-for-racket (define rest ...))]. Intended for introducing a single binding into the enclosing context, with a definition given in the Racket language.

For example:
@(interaction #:eval the-eval
   (define-for-racket two (begin 1 2))
   (function (four) (#:annos foreign [type (fn int)])
     (begin-racket (* (begin 1 2) two)))
   (four))
}

@subsection{Fully Expanded Programs}

@(declare-exporting magnolisp/surface)

As far as the Magnolisp compiler is concerned, a Magnolisp program is fully expanded if it conforms to the following grammar. Any syntactic ambiguities are resolved in favor of the first matching grammar rule.

A non-terminal @(elem (racket _nt) (subscript "rkt")) is as documented for non-terminal @racket[_nt] in the ``Fully Expanded Programs'' section of the Racket Reference. A form @(elem (racket _form) (subscript "ign")) denotes language that is ignored by the Magnolisp compiler, but which may be useful when evaluating as Racket. A form @(elem (racket _form) (subscript (racket _property ≠ #f))) means the form @racket[_form] whose syntax object has the property named @racket[_property] set to a true value. Form @(elem (racket (_sub-form ...)) (subscript (racket _property ≠ #f))) is alternatively written as @(racket (#,(subscript (racket _property ≠ #f)) _sub-form ...)). Anything of the form @(indirect-id _id) is actually a non-terminal like @racketvarfont{id-expr}, but for the specific identifier @racketvarfont{id}.

@racketgrammar*[
#:literals (begin begin-for-syntax call/ec define-values define-syntaxes if let-values letrec-values letrec-syntaxes+values quote set! values void #%expression #%magnolisp #%plain-app #%plain-lambda #%provide #%require #%top)
[module-begin-form (#,racket-module-begin mgl-modlv-form ...)]
[mgl-modlv-form #,(ign (racket (#%provide #,(rkt-nt raw-provide-spec) ...)))
		#,(ign (racket (#%require #,(rkt-nt raw-require-spec) ...)))
		#,(rkt-ign-nt submodule-form)
                (begin mgl-modlv-form ...)
		#,(ign (racket (begin-for-syntax #,(rkt-nt module-level-form) ...)))
                module-level-def
                #,(ign (racket (define-syntaxes (trans-id ...) Racket-expr)))
                #,(ign-nt Racket-expr)
                #,(ign-nt in-racket-form)]
[module-level-def (define-values (id) mgl-expr)
		  (define-values (id ...) 
                    (#%plain-app #,(indirect-id values) mgl-expr ...))]
[Racket-expr #,(rkt-nt expr)]
[in-racket-form #,(flagged in-racket (racket _Racket-form))]
[mgl-expr #,(ign-nt in-racket-form)
	  (begin mgl-expr ...+)
          (#%expression mgl-expr)
          (#%plain-lambda (id ...) mgl-expr)
	  (if #,(ign-nt Racket-expr) 
              (#%plain-app #%magnolisp (quote foreign-type))
              #,(ign-nt Racket-expr))
	  (if mgl-expr mgl-expr mgl-expr)
      	  (#%plain-app #,(indirect-id void) mgl-expr ...)
	  (#%plain-app #,(indirect-id values) mgl-expr)
	  (#%plain-app #,(indirect-id values))
          local-ec-block
	  local-ec-jump
	  (#%plain-app id-expr mgl-expr ...)
	  (#,(sub-flag annotate) 
           let-values ([() (begin mgl-anno-expr 
                                  (#%plain-app values))] 
                       ...) 
             mgl-expr)
      	  (let-values (bind-in-let ...) mgl-expr ...+)
          (letrec-values (bind-in-let ...) mgl-expr ...+)
          (letrec-syntaxes+values
              ([(trans-id ...) #,(ign-nt Racket-expr)] ...)
              (bind-in-let ...)
            mgl-expr ...+)
	  (set! id mgl-expr)
	  #,(racket (#,(racket quote) _datum))
          (#%top . id)
	  id]
[bind-in-let
      [(id ...) 
       (#%plain-app #,(indirect-id values) mgl-expr ...)]
      [() mgl-expr]
      [(id) mgl-expr]]
[local-ec-block (#,(sub-flag local-ec) #%plain-app #,(indirect-id call/ec) 
                (#%plain-lambda (id) mgl-expr ...+))]
[local-ec-jump #,(flagged local-ec (racket (#%plain-app id-expr mgl-expr)))]
[id-expr id (#%top . id) (#%expression id-expr)]
[mgl-anno-expr #,(harnessed anno-expr)]
[anno-expr (#%plain-app #%magnolisp (quote anno) 
            (quote type) type-expr)
           (#%plain-app #%magnolisp (quote anno) 
            (#,(racket quote) id) anno-val-expr)]
[anno-val-expr #,(racket (#,(racket quote) _datum))
               #,(racket (#,(racket quote-syntax) _datum))]
[type-expr id fn-type-expr]
[fn-type-expr (if #,(ign-nt Racket-expr)
                  (#%plain-app #%magnolisp 'fn type-expr ...+)
                  #,(ign-nt Racket-expr))]
]

where:

@specsubform[id]{
An identifier.}

@specsubform[trans-id]{
An identifier with a @emph{transformer binding}.}

@specsubform[datum]{
A piece of literal data. A @(racket (#,(racket quote) _datum)) form is a literal in Magnolisp, and its type must be possible to infer from context. Boolean literals are an exception, as their Magnolisp type is recognized as @racket[Bool].}

@specsubform[Racket-form]{
Any Racket core form.}

@specsubform[in-racket-form]{
Any Racket form that has the syntax property @racket['in-racket] set to a true value. These are ignored by the Magnolisp compiler where possible, and it is an error if they persist in contexts where they ultimately cannot be ignored. (The @racket[begin-racket] and @racket[begin-for-racket] forms are implemented through this mechanism.)}

@specsubform[submodule-form]{
A Racket submodule definition. Submodules are not actually supported by the @racketmodname[magnolisp] language, but the Magnolisp compiler does allow them to appear, and merely ignores them.}

@specsubform[anno-expr]{
An annotation expression, containing an identifier @racket[_id] naming the kind of annotation, and an expression specifying the ``value'' of the annotation. In the generic case, any symbol can be used to name an annotation kind, and any @racket[quote]d or @racket[quote-syntax]ed datum can give the value. Only annotations of kind @racket['type] are parsed in a specific way.}

@specsubform[local-ec-block]{
A restricted form of @racket[call/ec] invocation, which is flagged with the syntax property @racket['local-ec]. The semantic restriction is that non-local escapes (beyond the enclosing function's body) are not allowed.}

@specsubform[local-ec-jump]{
A restricted form of escape continuation invocation, flagged with the syntax property @racket['local-ec]. The escape must be local, and it must be within the lexical scope of a matching @racket[_local-ec-block].}

@warning{For some of the @(indirect-id id) non-terminals, the current parser actually assumes a direct @racket[_id].}

@defthing[#%magnolisp any/c]{
A value binding whose identifier is used to uniquely identify some Magnolisp core syntactic forms. It always appears in the application position of a Racket @racket[#%plain-app] core form. The value of the variable does not matter when compiling an Magnolisp, as it is never used. To prevent evaluation as Racket, all the syntactic constructs exported by @racketmodname[magnolisp] surround @racket[#%magnolisp] applications with a ``short-circuiting'' Racket expression.}

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
          [#:out out (or/c #f output-port?) (current-output-port)]
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

@section{Software Download}

A Git repository of the Magnolisp source code can be found at:
@nested[#:style 'inset]{@url{https://github.com/bldl/magnolisp}}

Racket version 6 is required to run the software. It may well be installable directly off GitHub with the command:

@commandline{raco pkg install git://github.com/bldl/magnolisp}

@section{License}

Except where otherwise noted, the following license applies:

Copyright © 2012-2014 University of Bergen and the authors.

Authors: Tero Hasu

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation files
(the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

@(close-eval the-eval)
