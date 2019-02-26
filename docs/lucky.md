# Lucky
A toy functional programming language implemented in Rust. It will be similar to Haskell; however, will support mutable data and state.

# Syntax

## Atoms
Atoms are the smallest structural unit of the language. This includes:
* Identifiers: x, y
* Literals: 1, 1.0, "string"
* Operators: +, -, (, ), [, ]
* Keywords: let, val, def, use

## Data Types

There are 6 build in data types: 

* int
* float
* char
* string
* list
* dictionary

Indexing of lists and dictionaries will be done with the '?' operator. I am doing this because there are no explicit generic instantiation.

```
let x = [1..10]
let y = x ? 0
```

## Functions

```code

# with type annotations
let foo x: int y: int -> int =  x + y // int int -> int

let foo: int int -> int = \x y => x + y

# without type annotations
let foo x y = x + y

```

## Abstract Data Types

This is how a sum type is defined.

```
type Expr = IntLiteral int | FloatLiteral float | Binary Op Expr Expr | ...

type ColorFormats = RGB | RGBA | R | G | B | A
```

This is how a product type is defined.

```
type Package = Package string Function Structure
```

This is a record type.

```
data Book = {
    title: string,
    author: string,
    year: int
}

let book = Book {title: "Potter", author: "Knowles", year: 1999}

let book = Book {"potter", "Knowles", 1999}
```

To access a field use a colon. As an example here is the access of the title field from Book.

```
let book = Book {"potter", "Knowles", 1999}

let title = book:title
```