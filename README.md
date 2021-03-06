# HNumeric

![Travis](https://travis-ci.org/Axect/HNumeric.svg?branch=master)

## Packages

* HNum.Vector  : Contain vector, matrix, linear algebra
* HNum.Stats   : Contain statistical functions
* HNum.CSV     : CSV Tools for HNum (Contain DataFrame)
* HNum.Special : Special Function wrapper for HNum
* HNum.F       : Functional Programming Tools for HNum

## Installation

### 1. Cabal Install

```sh
cabal update
cabal install HNumeric
```

That's all!

### 2. Import to Stack project

If you use this package to your own project, then you should change `stack.yaml` and `package.yaml`

#### 1) Change `stack.yaml`

```yaml
# In stack.yaml
extra-deps:
  - git: https://github.com/Axect/HNumeric.git
    commit: [Latest Commit]
  - normaldistribution-1.1.0.3
  - random-1.1
```

* Replace [Latest Commit] with latest commit in [HNumeric Commit](https://github.com/Axect/HNumeric/commits/master)

#### 2) Change `package.yaml`

```yaml
# In package.yaml
dependecies:
- base
- HNumeric
- normaldistribution
- random
```

Then enjoy!

## Documentation

Documentation is prepared on authorea

[HNumeric Documentation](https://www.authorea.com/users/223838/articles/307659-hnumeric-documentation)

## Usage

### Import Module

* HNum.Vector
* HNum.Stats
* HNum.CSV
* HNum.F

### Basic Vector Usage

```haskell
-- HNumeric-0.3.0.0 Documentation

let a = vector [1,2,3] -- Vector declaration
let b = Vector [4,5,6] -- small v and large V are same (for convenient)

-- Print Vector
print a

-- You can (+1) by fmap (Vector is functor)
(+1) <$> a 

-- Or MATLAB-like operator (.+, .-, .*, ./, .^)
a .+ 1 -- do not 1 .+ a (. means position of vector)

-- You can make list from vector
toList a -- [1, 2, 3]

-- You can make vector from list
fromList [1,2,3] -- Vector [1,2,3]

-- You can add (subtract, multiply, divide) vectors
a + b -- Vector [5,7,9]

-- Also dot product is here.
a .*. b -- 1*4 + 2*5 + 3*6 = 32

-- Declare Matrix (Syntactic Sugar)
let c = matrix [[1,2],[3,4]]

-- or Declare using R Syntax
let d = Matrix {val = Vector [5,6,7,8], row = 2, col = 2, byRow = True}

-- Determinant
det c

-- Inverse
inv c

-- Transpose
transpose c

-- Matrix ops with Constant (+, -, *, /, ^)
c .+ 1 -- Matrix [[2,3],[4,5]]

-- Matrix ops with Matrix (+, -)
c + c -- Matrix [[2,4],[6,8]]

-- Matrix Multiplication
c %*% d

-- Matrix - Inverse Multiplication
c %/% d

-- Vector Concatenate
hcat a b -- Vector [1,2,3,4,5,6]
vcat a b -- Matrix [[1,2,3],[4,5,6]]

-- Matrix Concatenate
hcat c d -- Matrix [[1,2,5,6],[3,4,7,8]]
vcat c d -- Matrix [[1,2],[3,4],[5,6],[7,8]]

-- Insert Vector to Matrix
vector [1, 2] .: c -- Matrix [[1,2],[1,2],[3,4]]
```

### Basic Stats Usage

```haskell
-- Sample Vector (import Vector)
v = vector [1..10]
w = vector [10, 9 .. 1]

-- Mean
mean v

-- Var
var v

-- Std
std v

-- Cov Matrix
cov v w

-- Only Cov
cov' v w

-- Linear Fit
(intercept, slope) = lm v w -- (11.0, -1.0) -- (Intercept, Slope)

-- Linear Fit function
lineFit (intercept, slope) (Vector [1 .. 20])

-- RSS
rss v w

-- RSE
rse v w
```

### DONE

* Effective Matrix Structure (R-like Structure)
* Divide and Conquer Matrix Multiplication, Determinant, Inverse
* Module CSV with DataFrame (read / write)
* FuncTools
* Parallelize Matrix Arithmetics

### TODO (2018.06.18)

* DSL Documentation by LaTeX (Developing)
* More Statistical Tools (Like Normal Distribution)
