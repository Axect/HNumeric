# HNumeric

## Packages

* Vector : Contain vector, matrix, linear algebra
* Stats : Contain statistical functions

## Usage

### Basic Vector Use

```haskell
let a = Vector [1,2,3] -- Vector declaration
let b = Vector [4,5,6]

-- Print Vector
print a

-- You can (+1) by fmap (Vector is functor)
(+1) <$> a 

-- You can make list from vector
toList a -- [1, 2, 3]

-- You can make vector from list
fromList [1,2,3] -- Vector [1,2,3]

-- You can add (subtract, multiply, divide) vectors
a + b -- Vector [5,7,9]

-- Declare Matrix
let c = Vector [[1,2],[3,4]]

-- Determinant
det c
```
