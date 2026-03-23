# Standard Library: Math and Numeric Functions

*Examples on this page may reference functions from other stdlib modules
without showing explicit `(import ...)` statements. In the REPL or your
own code, you will need to import non-prelude modules before using their
exports — see [Importing
modules](https://willbrannon.com/arl/articles/lang-reference.html#importing-modules).*

## Arithmetic Helpers

### %

Modulo helper using R %%.

**Signature:** `(% x y)`

**Parameters:** - **`x`** — Dividend - **`y`** — Divisor

**Examples:**

    arl> (% 10 3)   ; => 1
    #> 1
    arl> (% 17 5)   ; => 2
    #> 2

------------------------------------------------------------------------

### inc

Increment numeric value by n (default 1).

**Signature:** `(inc x [n 1])`

**Parameters:** - **`x`** — Number to increment - **`n`** — Amount to
add (default 1)

**Examples:**

    arl> (inc 5)       ; => 6
    #> 6
    arl> (inc 5 3)     ; => 8
    #> 8

**See also:** [dec](#dec)

------------------------------------------------------------------------

### dec

Decrement numeric value by n (default 1).

**Signature:** `(dec x [n 1])`

**Parameters:** - **`x`** — Number to decrement - **`n`** — Amount to
subtract (default 1)

**Examples:**

    arl> (dec 5)       ; => 4
    #> 4
    arl> (dec 5 3)     ; => 2
    #> 2

**See also:** [inc](#inc)

------------------------------------------------------------------------

### clamp

Clamp numeric value x to the inclusive range \[lo, hi\].

**Signature:** `(clamp x lo hi)`

**Parameters:** - **`x`** — Value to clamp - **`lo`** — Lower bound
(inclusive) - **`hi`** — Upper bound (inclusive)

**Examples:**

    arl> (clamp 5 0 10)   ; => 5
    #> 5
    arl> (clamp -3 0 10)  ; => 0
    #> 0
    arl> (clamp 15 0 10)  ; => 10
    #> 10

**See also:** [within?](#within-p)

------------------------------------------------------------------------

### within?

Return \#t if x is within the inclusive range \[lo, hi\].

**Signature:** `(within? x lo hi)`

**Parameters:** - **`x`** — Value to test - **`lo`** — Lower bound
(inclusive) - **`hi`** — Upper bound (inclusive)

**Examples:**

    arl> (within? 5 0 10)   ; => #t
    #> TRUE
    arl> (within? -3 0 10)  ; => #f
    #> FALSE
    arl> (within? 10 0 10)  ; => #t
    #> TRUE

**See also:** [clamp](#clamp)

------------------------------------------------------------------------

### signum

Return sign of x: -1, 0, or 1.

**Signature:** `(signum x)`

**Parameters:** - **`x`** — Numeric value

**Examples:**

    arl> (signum 42)    ; => 1
    #> 1
    arl> (signum -5)    ; => -1
    #> -1
    arl> (signum 0)     ; => 0
    #> 0

------------------------------------------------------------------------

## Power and Roots

### expt

Return base raised to power.

**Signature:** `(expt base power)`

**Parameters:** - **`base`** — Base number - **`power`** — Exponent

**Examples:**

    arl> (expt 2 10)   ; => 1024
    #> 1024
    arl> (expt 3 3)    ; => 27
    #> 27

------------------------------------------------------------------------

## Integer Division

### quotient

Return integer quotient of x divided by y.

**Signature:** `(quotient x y)`

**Parameters:** - **`x`** — Dividend - **`y`** — Divisor

**Examples:**

    arl> (quotient 10 3)   ; => 3
    #> 3
    arl> (quotient -10 3)  ; => -3
    #> -3

**See also:** [remainder](#remainder), [modulo](#modulo)

------------------------------------------------------------------------

### remainder

Return remainder of x divided by y (same sign as x).

**Signature:** `(remainder x y)`

**Parameters:** - **`x`** — Dividend - **`y`** — Divisor

**Examples:**

    arl> (remainder 10 3)   ; => 1
    #> 1
    arl> (remainder -10 3)  ; => -1
    #> -1

**See also:** [quotient](#quotient), [modulo](#modulo)

------------------------------------------------------------------------

### modulo

Return modulo of x and y (same sign as y).

**Signature:** `(modulo x y)`

**Parameters:** - **`x`** — Dividend - **`y`** — Divisor

**Examples:**

    arl> (modulo 10 3)    ; => 1
    #> 1
    arl> (modulo -10 3)   ; => 2
    #> 2

**See also:** [quotient](#quotient), [remainder](#remainder),
[%](#percent)

------------------------------------------------------------------------

## Number Theory

### gcd

Return greatest common divisor of arguments.

**Signature:** `(gcd a b ...)`

**Parameters:** - **`args`** — Two or more integers

**Examples:**

    arl> (gcd 12 8)      ; => 4
    #> 4
    arl> (gcd 12 8 6)    ; => 2
    #> 2

**See also:** [lcm](#lcm)

------------------------------------------------------------------------

### lcm

Return least common multiple of arguments.

**Signature:** `(lcm a b ...)`

**Parameters:** - **`args`** — Two or more integers

**Examples:**

    arl> (lcm 4 6)       ; => 12
    #> 12
    arl> (lcm 3 4 5)     ; => 60
    #> 60

**See also:** [gcd](#gcd)

------------------------------------------------------------------------

## Complex Number Utilities

### make-rectangular

Construct a complex number from real and imaginary parts.

**Signature:** `(make-rectangular real imag)`

**Parameters:** - **`real`** — Real component - **`imag`** — Imaginary
component

**Examples:**

    arl> (make-rectangular 3 4)  ; => 3+4i
    #> 3+4i

**See also:** [make-polar](#make-polar), [real-part](#real-part),
[imag-part](#imag-part)

------------------------------------------------------------------------

### make-polar

Construct a complex number from polar coordinates (magnitude and angle).

**Signature:** `(make-polar magnitude angle)`

**Parameters:** - **`magnitude`** — Distance from origin - **`angle`** —
Angle in radians

**Examples:**

    arl> (make-polar 5 0)      ; => 5+0i
    #> 5+0i

**See also:** [make-rectangular](#make-rectangular),
[magnitude](#magnitude), [angle](#angle)

------------------------------------------------------------------------

### real-part

Extract the real part of a complex number.

**Signature:** `(real-part z)`

**Parameters:** - **`z`** — Complex number

**Examples:**

    arl> (real-part (make-rectangular 3 4))  ; => 3.0
    #> 3

**See also:** [imag-part](#imag-part),
[make-rectangular](#make-rectangular)

------------------------------------------------------------------------

### imag-part

Extract the imaginary part of a complex number.

**Signature:** `(imag-part z)`

**Parameters:** - **`z`** — Complex number

**Examples:**

    arl> (imag-part (make-rectangular 3 4))  ; => 4.0
    #> 4

**See also:** [real-part](#real-part),
[make-rectangular](#make-rectangular)

------------------------------------------------------------------------

### magnitude

Compute the magnitude (modulus) of a complex number.

**Signature:** `(magnitude z)`

**Parameters:** - **`z`** — Complex number

**Examples:**

    arl> (magnitude (make-rectangular 3 4))  ; => 5.0
    #> 5

**See also:** [angle](#angle), [make-polar](#make-polar)

------------------------------------------------------------------------

### angle

Compute the angle (argument) of a complex number.

**Signature:** `(angle z)`

**Parameters:** - **`z`** — Complex number

**Examples:**

    arl> (angle (make-rectangular 1 1))  ; => 0.7853981633974483 (pi/4)
    #> 0.785398163397448

**See also:** [magnitude](#magnitude), [make-polar](#make-polar)

------------------------------------------------------------------------
