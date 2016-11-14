# cl-handlers

*A cross-browser DSL for defining HTTP handlers*

[![Build Status](https://travis-ci.org/inaimathi/cl-handlers.svg?branch=master)](https://travis-ci.org/inaimathi/cl-handlers)

## Usage

### Test Suite
You can run the test-suite from a Lisp REPL with

    (ql:quickload (list :cl-handlers :cl-handlers-test))
	(prove:run :cl-handlers-test)

### Handler Definition

#### Basics

In order to define handlers, use `define-handler`

    (define-handler (hello) ((message :string))
	  (format nil "You sent: ~s" message))

The name of the handler becomes its URI, and it accepts parameters according to the arglist. The above handler will listen for `GET` requests coming at the route `/hello`. It will expect the parameter `message`. So something like `/hello?message=foo` will result in a `200` response with the content `You sent: "foo"`.

#### Other Methods

You can use other methods by specifying them in the `define-handler` form.

    (define-handler (hello :method :post) ((message :string))
	  (format nil "You sent: ~s" message))

Will do the same as the original form, except it will only accept `POST` requests.

#### Different types

The built-in types are `:string`, `:integer` and `:keyword`. These automatically parse appropriate parameters for you.

    (define-handler (add) ((a :integer) (b :integer))
	  (write-to-string (+ a b)))

creates an addition handler. If a request comes in with parameters `a` and `b` that are parseable to integer (eg: `/add?a=12&b=10`), the return value will be the string of the sum of `a` and `b` (eg: `"22"`). If either parameter is not parseable as an integer (eg2: `/add?a=foo&b=5`), a `400` response is returned instead.

#### Path Variables

These parameters can be destructured from the path, rather than taken from GET/POST params.

    (define-handler (add/-a/-b) ((a :integer) (b :integer))
	  (write-to-string (+ a b)))

with this approach, the incoming request can look like `/add/12/10`. You can also inline the type annotations if you like.

    (define-handler (add/-a=integer/-b=integer) ()
	  (write-to-string (+ a b)))

### Running a Server

Once you've defined some handlers, the function `make-app` will return a function suitable for running them as a web application.

```
CL-HANDLERS> (define-handler (test) () "Hello world!")
#<HANDLER-TABLE {1002E084D3}>
CL-HANDLERS> (define-handler (add) ((a :integer) (b :integer))
    (write-to-string (+ a b)))
#<HANDLER-TABLE {1002E084D3}>
CL-HANDLERS> (define-handler (add :method :post) ((a :integer) (b :integer))
    (write-to-string (+ a b)))
#<HANDLER-TABLE {1002E084D3}>
CL-HANDLERS> (define-handler (add/-a=integer/-b=integer) ()
    (write-to-string (+ a b)))
#<HANDLER-TABLE {1002E084D3}>
CL-HANDLERS> (make-app)
#<CLOSURE (LAMBDA (ENV) :IN MAKE-APP) {100B67D79B}>
CL-HANDLERS>
```

It's a function of one argument, suitable running with [`:clack`](https://github.com/fukamachi/clack).

```
CL-HANDLERS> (ql:quickload :clack)
To load "clack":
  Load 1 ASDF system:
    clack
; Loading "clack"

(:CLACK)
CL-HANDLERS> (clack:clackup (make-app) :server :hunchentoot :port 5000 :use-thread nil)
```

You can then navigate to each of the specified local handlers. For instance, going to `http://localhost:5000/add/12/13` should display the plain-text response `25`.

The app returned this way is also incidentally runnable with the [`:woo` server](https://github.com/fukamachi/woo).

```
CL-HANDLERS> (ql:quickload :woo)
To load "woo":
  Load 1 ASDF system:
    woo
; Loading "woo"

(:WOO)
CL-HANDLERS> (woo:run (make-app) :port 5000)
```

## Exported Symbols

### Handler-related function

#### `:define-handler`

Inserts a handler into the `handler-table` in context.

#### `:with-handler-table`

Executes forms in the context of the given `handler-table`. By default all `define-handler` and `find-handler` calls use a global `handler-table` defined internally to `:cl-handlers`.

#### `:empty`

Returns the empty handler table.

#### `:find-handler`

Tries to find a handler in the `handler-table` in context.

#### `:make-app`

Optionally takes a `handler-table` (defaults to the current one in context), and makes an `app` (a function that takes an environment and generates the appropriate HTTP response). The result is suitable for serving via [`:woo`](https://github.com/fukamachi/woo) or [`:clack`](https://github.com/fukamachi/clack).

### Parsing functions

*(This part should probably be in a dedicated project. It deals with getting tihngs out of string representations.)*

#### `:from-string`

Takes a type designating symbol and a string. Tries to parse that kind of thing from the string. Might throw arbitrary errors (for instance `(from-string :integer "blah")` will throw a low-level INT-related error).

Define these for your own types, but actually use `string->` if you need to call a parse.

#### `:string->`

Wrapper around `from-string` that only throws `from-string-error`s rather than arbitrary ones.

#### `:from-string-error`

The base error class thrown by `from-string` and `string->`. Extend this if you want your own parsing errors.

#### `:from-string-unknown-type`

The error type thrown for undefined types. Don't extend this; it'll cause odd behaviors with `define-handler`s' static checking.

## TODO/Notes

- Fuckton of warnings to deal with after latest round of changes to `make-handler`
- Write some more tests as you go

### Open Questions
- How do we handle session?
  - Should be optional. As in, optionally turn it off to save the cycles needed to generate a session key.
- Is it possible to automate handler testing based on the type annotations?
  - My gut says "yes". We'd need `arbitrary :: symbol -> a` that took a type annotation and returned a random value of that type. Testing a handler would then mean
    1. Generating a list of arbitrary parameters according to its specification
	2. Serializing the values from step 1 to strings
	3. Calling the handler function with the result from step 2
	4. Recording the result, and reporting on non-`200` responses (or, possibly non-error responses depending on the input)
	5. Optionally, generate the query appropriate HTTP request and send it up using `drakma` or something (record results of this the same way)
  - Put some serious thought into this

### Decisions
- Variable param syntax is `-foo=bar`. The type is optional if you intend to declare it in an arg-style annotation
- No duplicate declarations (lots of little holes to fall down in that direction)
- Path variables may be declared inline or as arg-style declarations with the same name
- `define-handler` statically checks:
  - That all parameters are annotated
  - That there are no duplicate parameters
  - That the specified method is valid
  - That all annotations correspond to valid types (TODO)
- Completely minimal error-handler table/definition system, but handlers can specify their status codes. This way, we have a guaranteed-working error system underneath a potentially highly custom and flexible set of error pages.
- Method specializers are consed onto the path when we do a `trie-lookup`, and insertion from `define-handler`.
- Turns out [`:clack`](http://quickdocs.org/clack/) is already cross-platform in pretty much the way this is trying to be, so I'll target that instead of rolling my own. Keep an eye on it; if `fukamachi` stops maintining it, might need to actually target individual servers.


### Notes FOR THE FUTURE
- If it turns out that we want handlers that accept requests of any method, the easiest way to implement them seems to be
	1. Conditionally consing the method onto the URL in `define-handler`
	2. As part of `find-handler`, search for the naked path first then with the consed method.
- We're doing a lot of work in `make-handler` that might never get used. We could analyze `body` and try to eliminate unnecessary intermediate functions and values, inlining where we can.
