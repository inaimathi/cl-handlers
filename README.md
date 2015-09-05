# cl-handlers

*A cross-browser DSL for defining HTTP handlers*

**This library is still in development.** Don't use it yet.

## TODO/Notes

### Open Questions
- How do we deal with POST parameters?
  - Deal with URL-encoded parameters in a standardized way.
  - Give users hooks to deal with the rest (a callback to read the body from stream)
- How do we deal with session? Same as in `house`?
- Do we expose `header` properties to handlers? If so, how?
- How do we organize this project so that it doesn't depend on every server it supports?

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
- Method specializers are consed onto the path when we do a `trie-lookup`

### Notes FOR THE FUTURE
- If it turns out that we want handlers that accept requests of any method, the easiest way to implement them seems to be
	1. Conditionally consing the method onto the URL in `define-handler`
	2. As part of `find-handler`, search for the naked path first then with the consed method.

## Usage
You can run the test-suite from a Lisp REPL with

    (ql:quickload (list :cl-handlers :cl-handlers-test))
	(prove:run :cl-handlers-test)

This project does nothing else until I write at least one server adapter.

The first one will probably be for [`fukamachi/woo`](), but don't hold me to that.

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
