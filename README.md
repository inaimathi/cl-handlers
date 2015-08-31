# cl-handlers

*A cross-browser DSL for defining HTTP handlers*

**This library is still in development.** Don't use it yet.

## TODO/Notes

- Decide on the variable param syntax
	- Currently `<foo>`, but seems weird to bracket the variable name
	- It can't be `:blah` for Lisp-`reader`-related reasons (also,
	  I'd want to be able to specify type inline)
	- Probably shouldn't be `?foo` because it might be confusing for
	  new readers (`?` is the param separator in a URI)
	- Maybe `>foo`? `>foo>>integer`
- Decide what `define-handler` should check statically
	- Currently checks that you've declared all your path variables, and
	  makes sure there are no duplicates.
	- Should probably check that all your types are valid (that you
	  don't do something like `(foo :mumble)` without having the appropriate
	  parsers defined for `:mumble`)
- Figure out what serving these looks like.
	- Probably need to write server-specific functions here. Or a method that
	  dispatches on servers somehow

## Usage

You can run the test-suite from a Lisp REPL with

    (ql:quickload (list :cl-handlers :cl-handlers-test))
	(prove:run :cl-handlers-test)

Otherwise, this project does nothing until I write at least one server adapter.

The first one will probably be for [`fukamachi/woo`](), but don't hold me to that.

## Export Notes

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



### `:with-handler-table`

Executes forms in the context of the given `handler-table`. By default all `define-handler` and `find-handler` calls use a global `handler-table` defined internally to `:cl-handlers`.

### `:empty`

Returns the empty handler table.

### `:define-handler`

Inserts a handler into the `handler-table` in context.

### `:find-handler`

Tries to find a handler in the `handler-table` in context.
