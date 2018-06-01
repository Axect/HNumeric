# HNumeric

![Travis](https://travis-ci.org/Axect/HNumeric.svg?branch=master)

## Packages

* Vector : Contain vector, matrix, linear algebra
* Stats : Contain statistical functions

## Installation

### 1. Native Use

You can use this package just change `app/Main.hs`
Then, just type next command

```bash
git clone https://github.com/Axect/HNumeric
```

### 2. Import to Stack project

If you use this package to your own project, then you should change `stack.yaml` and `[own_package].cabal`

#### 1) Change `stack.yaml`

```yaml
# In stack.yaml
extra-deps:
  - git: https://github.com/Axect/HNumeric.git
    commit: [Latest Commit]
```

* Replace [Latest Commit] with latest commit in [HNumeric Commit](https://github.com/Axect/HNumeric/commits/master)

#### 2) Change `[Your Package].cabal`

* Just add HNumeric to `build-depends`
* For example, see below

```cabal
# In your cabal file
# This example - test.cabal
library
  exposed-modules:
      Lib
  other-modules:
      Paths_test
  hs-source-dirs:
      src
  build-depends:
      base,
      HNumeric
  default-language: Haskell2010

executable test-exe
  main-is: Main.hs
  other-modules:
      Paths_test
  hs-source-dirs:
      app
  ghc-options: -threaded -rtsopts -with-rtsopts=-N
  build-depends:
      base,
      test,
      HNumeric
  default-language: Haskell2010
```

#### 3) ETC

* Remove your `package.yaml`
* `stack build`
* Then Enjoy!

## Usage

### Import Module

* Vector
* Stats

### Basic Vector Use

```haskell
let a = Vector [1,2,3] -- Vector declaration
let b = Vector [4,5,6]

-- Print Vector
print a

-- You can (+1) by fmap (Vector is functor)
(+1) <$> a 

-- Or MATLAB-like operator (.+, .-, .*, ./, .^)
a .+ 1

-- You can make list from vector
toList a -- [1, 2, 3]

-- You can make vector from list
fromList [1,2,3] -- Vector [1,2,3]

-- You can add (subtract, multiply, divide) vectors
a + b -- Vector [5,7,9]

-- Also dot product is here.
a .*. b -- 1*4 + 2*5 + 3*6 = 32

-- Horizontal concatenate
a .++. b -- Vector [1,2,3,4,5,6]
-- or
hcat a b

-- Vertical Concatenate
a .**. b -- Vector [[1,2,3],[4,5,6]] -- Matrix Integer
-- or
vcat a b

-- Declare Matrix
let c = Vector [[1,2],[3,4]]

-- Determinant
det c

-- Inverse
inv c

-- Matrix ops with Constant (+, -, *, /, ^)
c %+ 1 -- Vector [[2,3],[4,5]]

-- Matrix ops with Matrix (+, -)
c %+% c -- Vector [[2,4],[6,8]]

-- Vector concat with Matrix
Vector [0,1] .: c -- Vector [[0,1],[1,2],[3,4]]

-- Matrix concat with Matrix
c %++% c -- Vector [[1,2],[3,4],[1,2],[3,4]]
```

### Basic Stats Usage

```haskell
-- Sample Vector (import Vector)
v = Vector [1..10]
w = Vector [10, 9 .. 1]

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
