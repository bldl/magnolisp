#lang scribble/manual
@(require scribble/eval "util.rkt"
	  (for-label syntax/modresolve
	  	     (only-in racket/base add1 sub1 + - * /)
	             (prefix-in r. (only-in racket/base define))
                     @; a trick to get only the original exports
		     (combine-in
		       (only-meta-in 0 magnolisp/main)
		       magnolisp/core
		       magnolisp/modbeg
		       magnolisp/surface
		       magnolisp/prelude)
	             magnolisp/compiler-api
		     ))

@(define the-eval (make-base-eval))
@(the-eval '(require magnolisp/main))

@title{Magnolisp}

@author["Tero Hasu"]

This is @deftech{Magnolisp}, a small, experimental language and implementation. It is experimental in its implementation technique, which is to replace the phase level 0 (runtime) language of @link["http://racket-lang.org/"]{Racket} with something non-Racket (here: Magnolisp), and translate it into another language (here: C++) for execution.

Magnolisp is inspired by Racket and the likewise experimental programming language @link["http://magnolia-lang.org/"]{Magnolia}. Its algebraic language resembles Racket's, and Racket also provides the module and macro systems. The language is restricted in ways similar to Magnolia, with the restrictions designed to support static reasoning about code, and to allow for fairly direct mapping to most mainstream languages.

Magnolisp is intended to explore and demonstrate techniques for source-to-source compilation on top of Racket, not to support writing of useful applications.

@section{Magnolisp the Language}

@defmodulelang[magnolisp]

The Magnolisp language relies on Racket for its module and macro systems. All of Racket may be used for macro programming. The @racketmodname[racket/base] language is provided by default for phase level 1 (compile time).

A small subset of @racketmodname[racket/base] definitions is also available at phase level 0 by default, as these may be used in runtime code, and evaluated as @racketmodname[magnolisp] in the Racket VM. However, only a small subset of Racket can be handled by the Magnolisp compiler, and either the Magnolisp Racket language or the Magnolisp compiler will report errors as appropriate for uncompilable language.

When a @racketmodname[magnolisp] module is evaluated as Racket, any module top-level runtime expressions will also get evaluated; this feature is intended to facilitate testing during development. The Magnolisp compiler, on the other hand, discards top-level expressions, and also any top-level definitions that are not actually part of the program being compiled.

@section{Modules and Macros}

The Racket @racket[provide] and @racket[require] forms may be used in Magnolisp as normal, also at phase level 0. However, as far as C++ compilation is concerned, these are only used to connect together Magnolisp definitions internally to the compiled program/library. C++ imports and exports are specified separately using the @racket[foreign] and @racket[export] annotations.

For defining macros and macro-expansion time computation, the relevant Racket facilities (e.g., @racket[define-syntax], @racket[define-syntax-rule], @racket[begin-for-syntax], @etc) may be used as normal.

@section{Syntactic Forms}

@defmodule[magnolisp/surface]

The surface syntax of Magnolisp consists of a collection of Magnolisp-specific forms, as well as a selection of supported Racket forms. The @racketmodname[magnolisp/surface] module defines the Magnolisp-specific ones, and the @racketmodname[magnolisp] language exports these at phase level 0.

@subsection{Defining Forms}

In Magnolisp, it is possible to @racket[define] functions, types, and variables; of these, variable definitions are not allowed at the top level.

As Magnolisp has almost no standard library, it is ultimately necessary to define primitive types and functions (flagged as @racket[foreign]) in order to be able to compile programs that do anything useful. For this reason there is also a @racket[primitives] convenience form for defining multiple @racket[foreign] types and/or functions at once.

@defform*[((define #:type id maybe-annos)
           (define id maybe-annos expr)
	   (define (id arg ...) maybe-annos expr ...)
	   (define (id arg ...) maybe-annos #:function Racket-expr))]{
The first form defines a type. Presently only foreign types may be defined using this form, and @racket[id] gives the corresponding Magnolisp name. The @racket[foreign] annotation should always be provided.

For example:
@(racketblock+eval #:eval the-eval
  (define #:type int #:: (foreign))
  (define #:type long #:: ([foreign my_cxx_long])))

It is acceptable to define a type for a local (lexical) scope, if the type is not referenced elsewhere.

The second form defines a variable with the name @racket[id], and the (initial) value given by @racket[expr]. A @racket[type] annotation may be included to specify the Magnolisp type of the variable.

For example:
@(interaction #:eval the-eval
  (let ()
    (define x #:: ([type int]) 5)
    (add1 x)))

Any (module) top-level variable definition will not get translated into C++.

The third form defines a function. The (optional) body of a function is a sequence of expressions, which (if present) must produce a single value.

Unlike in Racket, no tail-call elimination may be assumed even when a recursive function application appears in @emph{tail position}.

Functions may be defined locally within another function. A local function may reference free variables from a surrounding lexical scope, as long as they are not used as L-values (i.e., targets of assignment), and as long as the variables are not top-level.

Where the function is declared as @racket[foreign], the Magnolisp compiler will ignore any body @racket[expr ...] sequence. When a function without a body is invoked as Racket, the result is @|void-const|. When a @racket[foreign] function has a body, it is typically there to ``simulate'' the behavior of the C++ implementation in the Racket VM. For purposes of simulation it can be useful to make use of the full Racket runtime language; to implement a function body in Racket syntax instead of Magnolisp syntax, enclose the body expression within a @racket[begin-racket] form.

A function with the @racket[export] flag in its annotations indicates that the function is part of the public API of a program that includes the containing module. When a function is used merely as a dependency (i.e., its containing module was not specified as being a part of the program), any @racket[export] flag is ignored.

When a function includes a @racket[type] annotation, the type expression must specify a function type (see @secref{type-expressions}).

For example:
@(racketblock+eval #:eval the-eval
  (define (identity x) 
    x)
  (define (five) #:: (export [type (-> int)])
    5)
  (define (inc x) #:: (foreign [type (-> int int)])
    (add1 x))
  (define (seven) #:: (foreign [type (-> int)])
    1 2 3 4 5 6 7)
  (define (nine) #:: (foreign [type (-> int)])
    (define x (seven))
    (define (compute)
      (inc (inc x)))
    (compute)))

Here, @racketid[identity] must have a single, concrete type, possible to determine from the context of use. It is not a generic function, and hence it may not be used in multiple different type contexts within a single program.

The second @racket[define] form may also be used to define a function (even a top-level one), provided that the ``variable initializer'' @racket[_expr] is a lambda expression. Alternatively, if the annotations indicate that the definition is for a @racket[foreign] function (whose @racket[type] is explicitly given as a function type), then @racket[id] is also taken to bind a function. Any ``variables'' that are applied in a program are also taken to refer to functions. For @racket[foreign] function definitions of this form, the @racket[_expr] would typically be a function (value) for ``simulating'' the foreign behavior.

For example:
@(interaction #:eval the-eval
  (let ()
    (define two1 #:: ([type (-> int)]) (#%plain-lambda () 2))
    (define two2 (#%plain-lambda () 2))
    (define mul #:: (foreign [type (-> int int int)]) *)
    (define (four) (mul (two1) (two2)))
    (mul (four) (four))))

The fourth @racket[define] form (with the @racket[#:function] keyword) may likewise be used to define a function, one whose Racket implementation is given as an @racket[Racket-expr]ession (for which Racket syntax is assumed). The arity of the function value that the expression yields must match the number of declared @racket[arg]uments. When this form is used, it is not necessary to give the type of the function, provided that the type can be inferred from context.}

@defform*[((declare #:type id maybe-annos)
           (declare (id arg ...) maybe-annos))]{
Forms used to declare C++ translation information for types or functions, not to implement them, or to bind the identifier @racket[id]. The binding must already exist.

The first form states that @racket[id] is a type, probably providing annotations for it (@racket[declare]'ing types as @racket[foreign] is presently compulsory).

The second form states that @racket[id] is a function, possibly providing annotations for it. The arguments @racket[arg ...] only serve to specify the arity of the function.

The key difference between @racket[define] and @racket[declare] is that the former binds the identifier, and thus at the same time necessarily specifies any Racket implementation, while the latter does not. That is: @Racket-racket[define] can be used to give a Racket implementation for something; Magnolisp's @racket[define] can be used to give both a Racket implementation (possibly a ``non-implementation'') as well as C++ translation information for something; whereas @racket[declare] only gives the C++ translation information for something.}

@defform*[((typedef id maybe-annos)
           (typedef id type-expr))]{
Defines a type. If a @racket[type-expr] is given, then @racket[id] is bound as an alias for that type expression. Otherwise annotations must specify the semantics for the type.}

@defform/subs[(function (id arg ...) maybe-annos maybe-body)
              ([maybe-body code:blank expr])]{
@deprecated[#:what "form" @racket[define]]{}
Defines a function.}

@defform[(var id maybe-annos expr)]{
@deprecated[#:what "form" @racket[define]]{}
Defines a local variable.}

@defform/subs[#:literals (::)
(primitives definition ...)
  ([definition
    [#:type id]
    [#:function (id arg ...) :: type-expr]])]{
Defines the specified types and functions as @racket[foreign] primitives, whose C++ name is assumed to be the same as the Magnolisp name. Any defined functions will have no Racket implementation (or rather, they will have an implementation that does nothing).}

@subsection{Annotations}

@racketgrammar*[
#:literals (export foreign type)
[maybe-annos code:blank (code:line #:: (anno-expr ...))]
[anno-expr export-anno-expr foreign-anno-expr type-anno-expr ...]
[export-anno-expr export (export C++-id)]
[foreign-anno-expr foreign (foreign C++-id)]
[type-anno-expr (type type-expr)]]

where:

@specsubform[C++-id]{
A valid C++ identifier. When not provided, a default C++ name is automatically derived from the Magnolisp name.}

The set of annotations that may be used in Magnolisp is open ended, to allow for additional tools support. Only the most central Magnolisp-compiler-recognized annotations are included in the above grammar.

It is not always necessary to explicitly specify a @racket[type] for a typed Magnolisp definition, as the Magnolisp compiler does whole-program type inference (in Hindley-Milner style). When evaluating as Racket, @racket[type] annotations are not used at all.

For convenience, the @racketmodname[magnolisp] language installs a reader extension that supports annotation related shorthands: @litchar{#ap}@racket[(_anno-expr ...) _expr] is short for @racket[(annotate (_anno-expr ...) _expr)]; and @litchar{^}@racket[_type-expr] is short for @racket[(type _type-expr)]. For example, @litchar{#ap}(@litchar{^}@racketidfont{int}) @racket[_expr] reads as @racket[(annotate ((type int)) _expr)].

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

@defform[#:literals (display write print cxx-str datum type-id)
  (literal fmt-elem ...)
  #:grammar ([fmt-elem str fmt-expr]
             [fmt-expr (fmt-insn fmt-obj)]
	     [fmt-insn display write print cxx-str]
	     [fmt-obj datum type-id str])]{
An annotation for type definitions, specifying how to format literal datums of that type. The specification is given as a sequence of elements, which are either string literals or simple formatting expressions. Each @racket[fmt-expr] is translated into a string, and the resulting strings are then concatenated, in order, to get the C++ string encoding for a each literal @racket[datum] of the concerned type. Thus, exact same datums of different types can translate differently.

@specsubform[str]{
A string literal.}

@specsubform[#:literals (display) display]{
Use @Racket-racket[display] to format the object.}
@specsubform[#:literals (write) write]{
Use @Racket-racket[write] to format the object.}
@specsubform[#:literals (print) print]{
Use @Racket-racket[print] to format the object.}
@specsubform[#:literals (cxx-str) cxx-str]{
Format the object as a ``regular'' C++ string literal, after coercing it into a string. Only non-control ASCII characters are allowed in the string.}

@specsubform[#:literals (datum) datum]{
Substituted with the datum of the literal.}
@specsubform[#:literals (type-id) type-id]{
Substituted with the C++ name of the literal's type.}}

@subsection[#:tag "type-expressions"]{Type Expressions}

@racketgrammar*[
#:literals (-> <> exists ∃ for-all ∀ auto)
[type-expr type-id
           (-> type-expr ... type-expr)
           (<> type-expr type-expr ...)
	   (exists type-var-id ... type-expr)
	   (for-all type-var-id ... type-expr)
	   (auto)]]

Type expressions are parsed according to the above grammar, where @racket[_type-id] must be an identifier that names a type.
The only predefined types in Magnolisp are @racket[Void] and @racket[Bool], and any others must be defined using @racket[define #:type] or some other type-binding form.

A @racket[_type-var-id] is an identifier that gets bound as a type variable for the context of its type expression.

@defform[(-> type-expr ... type-expr)]{
A function type expression, containing type expressions the function's arguments and its return value, in that order. A Magnolisp function always returns a single value.}

@defform[(<> type-expr type-expr ...)]{
A parametric type expression, containing type expressions for the type's base type and its type parameters, in that order. Type parameters translate as template parameters in C++.

For example:
@(racketblock+eval #:eval the-eval
  (define #:type stack #:: (foreign))
  (define (stack-id x) #:: ([type (-> (<> stack int) (auto))])
    x) (code:comment @#,elem{the C++ type of this @racket[x] use will be @racketid[stack<int>]}))}

@deftogether[(
  @defform[(exists type-var-id ... type-expr)]
  @defform[(∃ type-var-id ... type-expr)]
  )]{
An existential type. Specified by @racket[type-expr], which should make use of the @racket[type-var-id] type variables. Each type variable is taken to correspond to exactly one concrete type expression, which must be possible to infer.}

@deftogether[(
  @defform[(for-all type-var-id ... type-expr)]
  @defform[(∀ type-var-id ... type-expr)]
  )]{
A universal type. The @racket[type-var-id] type variables may take on any set of type assignments, which must be possible to infer based on their use context. (Each application context of a universally typed function may end up with a different assignment to the type variables.)

Only @racket[foreign] functions may be universally typed.

For example:
@(racketblock+eval #:eval the-eval
  (define #:type Box #:: (foreign))
  (define (box v) 
    #:: (foreign [type (∀ E (-> E (<> Box E)))]))
  (define (unbox box) 
    #:: (foreign [type (∀ E (-> (<> Box E) E))])))}

@defform[(auto)]{
A type expression that conveys no information about the thing being typed, indicating that it is expected for the type to be inferable based on the contexts of use of the thing. Equivalent to @racket[(exists T T)].}

@subsection{Value Expressions}

Like Racket (and unlike C++), the Magnolisp language makes no distinction between statements and expressions. Some expressions yield no useful value, however; such expressions conceptually produce a result of type @racket[Void] (such result values do exist at Racket run time, but not at C++ run time). Some expressions yield @emph{no} values (or @emph{multiple} values), but are merely used as a syntactic device, and only allowed to appear in certain contexts.

Magnolisp borrows a number of constructs from Racket (or Scheme). For example, there is a conditional form @racket[(if _test-expr _then-expr _else-expr)], as well as the derived forms @racket[(when _test-expr _then-expr ...+)] and @racket[(unless _test-expr _then-expr ...+)]. The @racket[_test-expr] conditional expression must always be of type @racket[Bool], and whether it holds depends on the ``truthiness'' of its value, as interpreted in C++ or Racket (as applicable). The branches of an @racket[if] must generally be of the same type, except where the result of the @racket[if] form is discarded. The @racket[when] and @racket[unless] can generally only appear in such result-discarding contexts, as they have an implicit ``else'' branch of type @racket[Void].

A @racket[(begin _body _...)] form, in Magnolisp, signifies a sequence of expressions, itself constituting an expression. Similarly to Racket, to allow definitions to appear within an expression sequence, @racket[(let () _body _...)] should be used instead.

The @racket[(let ([_id _expr] _...) _body ...+)], @racket[(let* ([_id _expr] _...) _body ...+)], and @racket[(letrec ([_id _expr] _...) _body ...+)] forms are also available in Magnolisp, but the named variant of @racket[let] is not supported.

The @racket[(set! _id _expr)] form is likewise available in Magnolisp, supporting assignment to variables.
The left-hand side expression @racket[_id] must be a reference to a bound variable.
(The @racket[_id] may naturally instead be a transformer binding to an assignment transformer, in which case the form is macro transformed as normal.)

In Magnolisp, @racket[(void _expr _...)] is an expression with no useful result (the result is of the unit type @racket[Void]). Any arguments to @racket[void] are evaluated as usual, but they are not used. The @racket[(values)] form signifies ``nothing,'' and has no result; hence it is an error for @racket[(values)] to appear in a position where the context expects a result. In result expecting contexts, the former may only appear in a 1-value context, and the latter in a 0-value context (there are few in Magnolisp).

The @racket[define] forms may appear in a Racket @emph{internal-definition context} (and not Racket @emph{expression context}). The same is true of @racket[define-values] forms that conform to the restricted syntax supported by the Magnolisp compiler.

@defform[(cast type-expr expr)]{
Annotates expression @racket[expr] with the type given by @racket[type-expr]. A @racket[cast] is commonly used to specify the type of a literal, which by themselves are generally untyped in Magnolisp. While the literal @racket["foo"] is treated as a @racket[string?] value by Racket, the Magnolisp compiler will expect to determine the literal expression's Magnolisp type based on annotations. The @racket[cast] form allows one to ``cast'' an expression to a specific type for the compiler.

For example:
@(interaction #:eval the-eval
  (cast int 5))

While generally only declarations require annotations, @racket[cast] demonstrates a specific case where it is useful to associate annotations with expressions.}

@defform[(annotate (anno-expr ...) expr)]{
Explicitly annotates the expression @racket[expr] with the specified annotations. May be used to specify annotations for an identifier that is bound using the regular Racket binding forms such as @racket[let], @racket[let*], @etc

For example:
@(interaction #:eval the-eval
  (let ([x (annotate ([type int]) 6)])
    x)
  (define-values (ten) (annotate ([type int]) 10))
  ten)
}

@defform[(if-target name then-expr else-expr)]{
A compile-time conditional expression that depends on the intended execution target. Currently the only meaningful target language @racket[name] is @racketidfont{cxx}, which stands for C++. When code is being compiled for a target matching @racket[name], only @racket[then-expr] will be included in generated executable code; otherwise it is @racket[else-expr] that will be subject to evaluation in the target environment.

Note that there is no specific support for execution-target-conditional macro expansion in Magnolisp (such conditionality is possible, but Magnolisp itself has no supporting mechanisms for it). Instead, to generate different code for different targets, one may use @racket[if-target] to macro generate code for @emph{all} targets at once (currently only C++ and Racket). The choice of which alternative code fragment to evaluate will be made after Magnolisp programs' macros have been expanded, but still at compile-time, either during source-to-source or bytecode compilation (depending on the execution target).

For example:
@(interaction #:eval the-eval
  (if-target cxx (seven) (five)))
}

@defform[(if-cxx then-expr else-expr)]{
A shorthand for @racket[(if-target cxx then-expr else-expr)].

See also: @racket[begin-racket], @racket[let-racket].}

@subsection{Racket Forms}

To include Racket code in a phase level 0 context that is significant to Magnolisp, you may wrap the code in a form that indicates that the code is only intended for parsing as Racket. Code so wrapped must be grammatically correct Racket, but not necessarily Magnolisp. The wrapping forms @racket[begin-racket] and @racket[let-racket] merely switch syntaxes, and have no effect on the namespace used for evaluating the enclosed sub-forms; the surrounding namespace is still in effect. Nesting of the wrapping forms is allowed.

@defform[(begin-racket Racket-form ...)]{
A Racket form that is equivalent to writing @racket[(begin Racket-form _...)], and hence not necessarily a Racket expression. Intended particularly for allowing the splicing of Racket definitions into the enclosing context, which is not possible with @racket[let-racket].

For example:
@(interaction #:eval the-eval
   (require (prefix-in r. (only-in racket/base define)))
   (begin-racket
     (r.define six 6)
     (r.define (one-more x) (let dummy () (+ x 1))))
   (define (eight) #:: (foreign [type (-> int)])
     (one-more (one-more six)))
   (eight))
}

@defform[(let-racket Racket-expr ...)]{
A Racket expression that is equivalent to writing @racket[(let () Racket-expr _...)]. The Magnolisp semantics is to: ignore such forms when at module top-level; and treat them as uncompilable expressions when appearing in an expression position. Uncompilable expressions are acceptable for as long as they are not part of a compiled program, or can be optimized away.

For example:
@(interaction #:eval the-eval
   (define (three) #:: (foreign [type (-> int)])
     (let-racket 
       (define-values (x y)
         (let ()
           (values 1 2))) 
       (set! x (let dummy () (one-more y)))
       x))
   (three))}

@section{Runtime Library}

@defmodule[magnolisp/prelude]

The Magnolisp language includes a small number of predefined names (that are not syntax). Most notably,
the compiler expects expressions of type @racket[Bool] and @racket[Void] in
certain contexts, and it also recognizes their identifiers. While the C++
translation semantics of these types are @emph{not} built into the compiler,
such semantics @emph{are} predefined by the language. More specifically, the
@racketmodname[magnolisp] language uses the @racketmodname[magnolisp/prelude]
module as its ``runtime library,'' one that specifies the C++ runtime names to
which these known types correspond. The @racketmodname[magnolisp/prelude]
module only contains such compile-time information, and no Racket bindings are
exported from it.

@defthing[#:kind "type" #:link-target? #f Bool any/c]{
A predefined type. The corresponding C++ type is @racketidfont{bool}, and the corresponding C++ constant values are @racketidfont{true} and @racketidfont{false}, respectively.}

@defthing[#:kind "type" #:link-target? #f Void any/c]{
A predefined type. Such values may not actually exist at C++ run time. The corresponding C++ type is @racketidfont{void}.}

@section{Core Magnolisp}

@defmodule[magnolisp/core]

There are a small number of Magnolisp-specific names that are treated specially by the Magnolisp compiler. These are bound in the @racketmodname[magnolisp/core] module, and exported for phase level 0 by the @racketmodname[magnolisp] language.

@subsection[#:tag "built-ins"]{Magnolisp Built-Ins}

A boolean expression is simply an expression of type @racket[Bool], which is one of the two predefined types in Magnolisp. The other one is @racket[Void], which is Magnolisp's unit type (whose values carry no information).

@defthing[#:kind "type" Bool any/c]{
A predefined type. The literals of this type are @racket[#t] and @racket[#f] (which are also the only typed literals in the language). All conditional expressions in Magnolisp are of type @racket[Bool].}

@defthing[#:kind "type" Void any/c]{
A predefined type. There are no literals for @racket[Void] values, but the Magnolisp core form @racket[(void _expr _...)] evaluates such a value, at least conceptually.}

@subsection[#:tag "core-syntax"]{Magnolisp Core Syntax}

Magnolisp core syntax is encoded primarily in terms of Racket's core forms. Magnolisp core forms that have no Racket counterpart, however, are encoded in terms of the @racket[#%magnolisp] variable, which is treated specially by the Magnolisp compiler. The @racket[#%magnolisp] binding is exported from the @racketmodname[magnolisp/core] module.

It is possible to define multiple different surface syntaxes for Magnolisp, and these can be defined as libraries similar to the @racketmodname[magnolisp/surface] syntax definition used by the @racketmodname[magnolisp] language. All Magnolisp language variants must, however, refer to the same core bindings (i.e., as exported from @racketmodname[racket/base] and @racketmodname[magnolisp/core]), as no other bindings are treated specially by the Magnolisp compiler.

@defthing[#:kind "binding" #%magnolisp any/c]{
A value binding whose identifier is used to uniquely identify some Magnolisp core syntactic forms. It always appears in the applied-procedure position of a Racket @racket[#%plain-app] core form. The value of the variable does not matter when compiling as Magnolisp, as it is never used. To prevent evaluation as Racket, all the syntactic constructs exported by @racketmodname[magnolisp] surround @racket[#%magnolisp] applications with a ``short-circuiting'' Racket expression.}

@subsection{Fully Expanded Programs}

As far as the Magnolisp compiler is concerned, a Magnolisp program is fully expanded if it conforms to the following grammar. Any syntactic ambiguities are resolved in favor of the first matching grammar rule.

A non-terminal @(elem (racket _nt) (subscript "rkt")) is as documented for non-terminal @racket[_nt] in @secref["fully-expanded" #:doc '(lib "scribblings/reference/reference.scrbl")] of the Racket Reference. 

A form @(elem (racket _form) (subscript "ign")) denotes language that is ignored by the Magnolisp compiler, but which may be useful when evaluating as Racket.

A form @(elem (racket _form) (subscript (racket _pname = _pval))) means the form @racket[_form] whose syntax object has the property named @racket[_pname] set to the value @racket[_pval]. 
A form @(elem (racket _form) (subscript (racket _pname ≠ _pval))) means the form @racket[_form] whose syntax object has the property named @racket[_pname] set to some value that is not @racket[_pval]. 
A form @(elem (racket (_sub-form ...)) (subscript (racket _pname _=/≠ _pval))) may alternatively be written as @(racket (#,(subscript (racket _pname _=/≠ _pval)) _sub-form ...)). 

Anything of the form @(indirect-id _id) is actually a non-terminal like @racketvarfont{id-expr}, but for the specific identifier @racketvarfont{id}.

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
[module-level-def (define-values ()
                    (begin
	              (if #,(ign-nt Racket-expr) 
                          (#%plain-app #%magnolisp (quote declare)
			    id mgl-expr)
                          #,(ign-nt Racket-expr))
		      (#%plain-app values)))
                  (define-values (id) mgl-expr)
		  (define-values (id ...) 
                    (#%plain-app values mgl-expr ...))]
[Racket-expr #,(rkt-nt expr)]
[in-racket-form #,(stxpropped (racket _Racket-form) for-target ≠ 'cxx)]
[mgl-expr #,(ign-nt in-racket-form)
	  (begin mgl-expr ...+)
	  (begin0 mgl-expr mgl-expr ...)
          (#%expression mgl-expr)
          (#%plain-lambda (id ...) mgl-expr ...+)
          if-target-expr
	  (if #,(ign-nt Racket-expr) 
              (#%plain-app #%magnolisp (quote foreign-type))
              #,(ign-nt Racket-expr))
	  (if mgl-expr mgl-expr mgl-expr)
      	  (#%plain-app #,(indirect-id void) mgl-expr ...)
	  (#%plain-app #,(indirect-id values) mgl-expr)
	  (#%plain-app #,(indirect-id values))
	  (#%plain-app id-expr mgl-expr ...)
	  (#,(stxprop-elem annotate) 
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
[if-target-expr
          (#,(stxprop-elem if-target = 'cxx)
           if #,(ign-nt Racket-expr) mgl-expr #,(ign-nt Racket-expr))
          (#,(stxprop-elem if-target ≠ 'cxx)
           if #,(ign-nt Racket-expr) #,(ign-nt Racket-expr) mgl-expr)]
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
Any Racket form that has the syntax property @racket['for-target] set to some value that is not @racket['cxx], meaning that the form is not intended for compilation to C++. These are ignored by the Magnolisp compiler where possible, and it is an error if they persist in contexts where they ultimately cannot be ignored. (The @racket[begin-racket] and @racket[begin-for-racket] forms are implemented through this mechanism.)}

@specsubform[if-target-expr]{
Indicates a choice between two expressions that is conditional on the compilation target language. Where the syntax property @racket['if-target] is set to the value @racket['cxx], the Magnolisp compiler will only compile the first expression. If it is set to some other value (indicating another target language), only the second expression will be compiled. (The @racket[if-target] form is implemented through this mechanism.)}

@specsubform[submodule-form]{
A Racket submodule definition. Submodules are not actually supported by the @racketmodname[magnolisp] language, but the Magnolisp compiler does allow them to appear, and merely ignores them.}

@specsubform[anno-expr]{
An annotation expression, containing an identifier @racket[_id] naming the kind of annotation, and an expression specifying the ``value'' of the annotation. In the generic case, any symbol can be used to name an annotation kind, and any @racket[quote]d or @racket[quote-syntax]ed datum can give the value. Only annotations of kind @racket['type] are parsed in a specific way.}

@warning{For some of the @(indirect-id id) non-terminals, the current parser actually assumes a direct @racket[_id].}

@section{Evaluation}

Programs written in Magnolisp can be evaluated in the usual Racket way, provided that the @(hash-lang) signature specifies the language as @racketmodname[magnolisp]. Any module top-level phase level 0 expressions are evaluated, and the results are printed (as for Racket's @racket-module-begin).

It is also possible to launch a Magnolisp REPL, by issuing the command:

@commandline{racket -I magnolisp}

@section{Magnolisp-Based Languages}

@defmodule[magnolisp/modbeg]

It is possible to implement languages other than Magnolisp that are translatable into C++ using Magnolisp's compiler. To enable this, the language's implementation must conform to the following requirements:
@itemlist[

@item{The macros of the language must target Magnolisp's @seclink["core-syntax"]{core syntax}.}

@item{It follows that the language must refer to Magnolisp's @seclink["built-ins"]{built-in types}, since certain core forms expect said types.}

@item{The language must export Magnolisp's @racketid[#%module-begin] macro, or its own variant thereof, one that prepares all the information that the Magnolisp compiler expects.}

@item{Where it is not the @racketmodname[magnolisp/prelude] module that specifies C++ mappings for the language's built-ins and primitives, the name of said module (or modules) must be communicated to the compiler via the language's own @a-module-begin macro.}

]

For an example of a language targeting Magnolisp core language, see
@link["http://bldl.github.io/erda/#%28part._.Erda.C__%29"]{@ErdaCxx}.

@defform[(module-begin form ...)]{
An implementation of Magnolisp's @racketid[#%module-begin].}

@defproc[(make-module-begin
          [stx syntax?]
          [#:prelude-path prelude-stx syntax? #''(magnolisp/prelude)])
	 syntax?]{
A helper function for implementing Magnolisp-compiler-compatible @a-module-begin macros. The @racket[stx] argument should be syntax for the @racketid[(#%module-begin form ...)] macro invocation. The @racket[prelude-stx] argument may be used to specify syntax for a list of module paths that should be loaded by the compiler, so that the compiler will know how to translate runtime support names into C++.}

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
Performs code generation for the program whose intermediate representation (IR) is stored in the compilation state @racket[st]. Code generation is only performed with the specified compiler back ends, and for the specified back end specific file types. For instance, to generate both a C++ header and implementation, you may pass @racket[backends] as @racket['((cxx (parts cc hh)))]. The @racket[backends] argument is an association list with one entry per backend. Passing @racket[out] as @racket[#f] causes code generation into (separate) files; otherwise the specified output port is used. When @racket[out] is a true value, the @racket[banner?] argument indicates whether banners (with filenames) should be printed to precede individual output files. When @racket[out] is @racket[#f], the @racket[outdir] argument specifies the output directory for generated files. The @racket[basename] string is used as the ``stem'' for output file names.
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

Note that some of the example programs are written in Magnolisp language variants other than @racketmodname[magnolisp] (which is the only one documented here), but the differences are typically minor and superficial.

@section{Source Code}

A Git repository of the Magnolisp source code can be found at:
@nested[#:style 'inset]{@url{https://github.com/bldl/magnolisp}}

@section[#:tag "install"]{Installation}

Racket version 6.3 or higher is required to run the software. The software has been tested with version 6.4 only. Version 6.2.1 will not work due to Racket 6.3 having a different @link["http://www.cs.utah.edu/~mflatt/scope-sets/"]{macro model}, and also due to differences in its @link["http://blog.racket-lang.org/2015/10/retiring-unstable.html"]{organization of @racketmodfont{unstable} libraries}.

The software is installable directly off GitHub with the command:
@commandline{raco pkg install git://github.com/bldl/magnolisp}

@section{License}

Except where otherwise noted, the following license applies:

Copyright © 2012-2016 University of Bergen and the authors.

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
