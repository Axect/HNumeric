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

### 2. Cabal Install

First, install prerequisite.

```sh
cabal install normaldistribution
```
  
Second, install tarball & unpack.

```sh
export VERSION=0.2.0.0

wget -O HNumeric-${VERSION}.tar.gz https://github.com/Axect/HNumeric/blob/master/dist/HNumeric-0.1.0.0.tar.gz\?raw\=true

tar -xvf HNumeric-${VERSION}.tar.gz
```

Finally, build & install

```sh
cabal build
cabal install
```

Finish!

### 3. Import to Stack project

If you use this package to your own project, then you should change `stack.yaml` and `package.yaml`

#### 1) Change `stack.yaml`

```yaml
# In stack.yaml
extra-deps:
  - git: https://github.com/Axect/HNumeric.git
    commit: [Latest Commit]
  - normaldistribution-1.1.0,3
```

* Replace [Latest Commit] with latest commit in [HNumeric Commit](https://github.com/Axect/HNumeric/commits/master)

#### 2) Change `package.yaml`

```yaml
# In package.yaml
dependecies:
- base
- HNumeric
- normaldistribution
```

Then enjoy!

## Usage

### Import Module

* HNum.Vector
* HNum.Stats
* ~~CSV~~

### Basic Vector Usage

```haskell
-- HNumeric-0.2.0.0 Documentation

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

-- Declare Matrix (Syntactic Sugar)
let c = matrix [[1,2],[3,4]]

-- or Declare using R Syntax
let d = Matrix {val = Vector [1,2,3,4], row = 2, col = 2, byRow = True}

-- Determinant
det c

-- Inverse
inv c

-- Matrix ops with Constant (+, -, *, /, ^)
c .+ 1 -- Matrix [[2,3],[4,5]]

-- Matrix ops with Matrix (+, -)
c + c -- Matrix [[2,4],[6,8]]
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

### TODO

* ~~Effective Matrix Multiplication~~
* Write Vector to CSV
* ~~Haddock~~
* DataFrame using Map
* ~~Fix Matrix Implementation~~
* ~~Numeric Class Define~~
