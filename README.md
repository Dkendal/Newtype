# Newtype

Newtype is a programming language for type level programming in typescript.

# Why

Typescript has a powerful typesystem, so powerful that it's turing
complete[^turing-completeness]. One is fully capable of writing a JSON parser
[^json-parser] using only the type system, ignoring the question of if this is
a good idea or not. In spite of this power, typescript's type system grammar is
packed full of impicit rules [^distributive-conditional-types][^jcalz], and
becomes quite unweildy with any significant length.

## Installing from source

```bash
git clone https://github.com/dkendal/newtype
cd newtype
stack build
stack install
```

## Grammar and syntax

Newtype borrows a good deal of syntax and ideas from Haskell and Idris.
Statements are only allowed at the top of the document, not unlike typescript.

### Primitive types

Primitive types are the same as in Typescript.

```haskell
any
unknown
object
string
number
bigint
boolean
null
undefined
never
```

### Statements

#### Imports

```haskell
import "./foo-bar" (Foo, Bar)
```

#### exports

Members are exported separately from their definition.

```haskell
provide (Foo, Bar)
```

#### Type definition

```haskell
type True = true
```

#### Interfaces

```haskell
interface ArrayBufferTypes where
  ArrayBuffer : ArrayBuffer
```

### Expressions

### Object literals, properties

#### Generic type application

Angle brackets and commas, and parens are omitted in newtype. Expressions may
be parenthesized to disambiguate.

```haskell
type Foo = Pick Bar "a" | "b"
```

```typescript
type Foo = Pick<Bar, "a" | "b">
```

#### Mapped types

Mapped types[^mapped-types] use set builder notation[^set-builder-notation].

```
{ Value : Key <- Type }
```

#### Infer prefix operator

`?` replaces the `infer` keyword.

```haskell
?Type
```

#### Conditional types

Newtypes makes it much easier to write conditional types[^conditional-types]

All expressions below compile to an equivalent typescript `A extends B ? Then :
Else` expression.

#### Operator

Newtype introduces a number of binary operators for type comparison.

- `A <: B` is equivalent to `A extends B`, read as extends-left.
- `A :> B` is equivalent to `B extends A`, read as extends-right.
- `A == B`
- `A != B`

It also introduces a single prefix operator to negate a condition

- `not A <: B`

#### If-Then-Else

```haskell
if A <: B
then C
else D
```

If the `else` branch is omitted, it is assumed to be `never`.

#### Boolean infix operators

Expressions can be combined using boolean infix operators.

```haskell
if A <: B then if C <: D then E
```

Can be rewritten as:

```haskell
if A <: B and C <: D then E
```

#### Case statements

```haskell
case A of
  string -> 1
  object -> 2
  _ -> never
```

## Related projects

- https://github.com/g-plane/TypeCake

## Footnotes

[^turing-completeness]: https://github.com/Microsoft/TypeScript/issues/14833
[^json-parser]: https://github.com/jamiebuilds/json-parser-in-typescript-very-bad-idea-please-dont-use
[^distributive-conditional-types]: https://www.typescriptlang.org/docs/handbook/2/conditional-types.html#distributive-conditional-types
[^jcalz]: Basically any question answered by the infamous and prolific jcalz has answered on Stackoverlow: https://stackoverflow.com/users/2887218/jcalz
[^mapped-types]: https://www.typescriptlang.org/docs/handbook/2/mapped-types.html
[^set-builder-notation]: https://en.wikipedia.org/wiki/Set-builder_notation#In_programming_languages
[^conditional-types]: https://www.typescriptlang.org/docs/handbook/2/conditional-types.html
