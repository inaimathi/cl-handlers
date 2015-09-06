# cl-handlers

*A cross-browser DSL for defining HTTP handlers*

**This library is still in development.** Don't use it yet.

## TODO/Notes

- In the case of a `POST` request with a form-encoded body, read the body and parse it along with uri parameters
  - Add a `read-body` callback to handler bodies to `POST` requests with other body data formats
- Add a `headers` function that a user can call to get a specific header inside of a handler context (may need this for some `POST` use-cases that aren't form-encoded, such as file uploads)
- Write some more tests as you go
- Think about generalizing errors a bit more. Maybe they should be basically the same as handlers, but stored elsewhere
  - Specific use case: I'd like a 400 error that tells the API caller what they fucked up and how. That can't happen with static responses, as nice as that is perf-wise

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
- Completely minimal error-handler table/definition system, but handlers should be able to specify their status codes (this would let you come up with more elaborate errors in some cases, but would still keep the safety net of a plain string handler)
- Method specializers are consed onto the path when we do a `trie-lookup`, and insertion from `define-handler`.
- Turns out [`:clack`](http://quickdocs.org/clack/) is already cross-platform in pretty much the way this is trying to be, so I'll target that instead of rolling my own. Keep an eye on it; if `fukamachi` stops maintining it, might need to actually target individual servers.

### Notes FOR THE FUTURE
- If it turns out that we want handlers that accept requests of any method, the easiest way to implement them seems to be
	1. Conditionally consing the method onto the URL in `define-handler`
	2. As part of `find-handler`, search for the naked path first then with the consed method.

## Usage
You can run the test-suite from a Lisp REPL with

    (ql:quickload (list :cl-handlers :cl-handlers-test))
	(prove:run :cl-handlers-test)

This project does nothing else until I write at least one server adapter.

The first one will probably be for [`fukamachi/woo`](https://github.com/fukamachi/woo), but don't hold me to that.

## Export Notes

### Handler-related function

#### `:define-handler`

Inserts a handler into the `handler-table` in context.

#### `:with-handler-table`

Executes forms in the context of the given `handler-table`. By default all `define-handler` and `find-handler` calls use a global `handler-table` defined internally to `:cl-handlers`.

#### `:empty`

Returns the empty handler table.

#### `:find-handler`

Tries to find a handler in the `handler-table` in context.

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
