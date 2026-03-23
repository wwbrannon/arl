# Standard Library: Strings, Display, and I/O

*Examples on this page may reference functions from other stdlib modules
without showing explicit `(import ...)` statements. In the REPL or your
own code, you will need to import non-prelude modules before using their
exports — see [Importing
modules](https://willbrannon.com/arl/articles/lang-reference.html#importing-modules).*

## String Operations

Core string manipulation functions: joining, splitting, searching,
replacing, and accessing characters. Most search/replace functions
default to fixed (literal) matching; pass `:fixed #f` for regex.

### string-join

Join strings with separator.

**Signature:** `(string-join x [sep ""])`

**Parameters:** - **`x`** — A list of strings to join. - **`sep`** —
Separator inserted between elements (default ““).

**Examples:**

    arl> (string-join (list "a" "b" "c") "-")  ; => "a-b-c"
    #> "a-b-c"
    arl> (string-join (list "x" "y" "z"))      ; => "xyz"
    #> "xyz"
    arl> (string-join (list "hello" "world") " ")  ; => "hello world"
    #> "hello world"

**See also:** [string-split](#string-split),
[string-format](#string-format)

------------------------------------------------------------------------

### string-split

Split string on separator.

**Signature:** `(string-split x [sep ""])`

**Parameters:** - **`x`** — The string to split. - **`sep`** — Separator
to split on (default ““), splitting into characters.

**Examples:**

    arl> (string-split "a-b-c" "-")  ; => ("a" "b" "c")
    #> ("a" "b" "c")
    arl> (string-split "hello" "")   ; => ("h" "e" "l" "l" "o")
    #> ("h" "e" "l" "l" "o")

**See also:** [string-join](#string-join)

------------------------------------------------------------------------

### trim

Trim leading and trailing whitespace.

**Signature:** `(trim str)`

**Parameters:** - **`str`** — The string to trim.

**Examples:**

    arl> (trim "  hello  ")  ; => "hello"
    #> "hello"
    arl> (trim "no spaces")  ; => "no spaces"
    #> "no spaces"

------------------------------------------------------------------------

### string-format

Format string with sprintf.

**Signature:** `(string-format fmt args...)`

**Parameters:** - **`fmt`** — A sprintf-style format string. -
**`args`** — Values to substitute into the format placeholders.

**Examples:**

    arl> (string-format "Hello, %s!" "world")   ; => "Hello, world!"
    #> "Hello, world!"
    arl> (string-format "%d + %d = %d" 1 2 3)   ; => "1 + 2 = 3"
    #> "1 + 2 = 3"
    arl> (string-format "%.2f" 3.14159)          ; => "3.14"
    #> "3.14"

**See also:** [string-concat](#string-concat)

------------------------------------------------------------------------

### string-contains?

Check pattern in string (fixed).

**Signature:** `(string-contains? str pattern [fixed #t])`

**Parameters:** - **`str`** — The string to search in. - **`pattern`** —
The pattern to search for. - **`fixed`** — If \#t (default), match
literally; if \#f, treat as regex.

**Examples:**

    arl> (string-contains? "hello world" "world")       ; => #t
    #> TRUE
    arl> (string-contains? "hello world" "xyz")          ; => #f
    #> FALSE
    arl> (string-contains? "hello world" "o.*d" :fixed #f)  ; => #t
    #> TRUE

**See also:** [string-match?](#string-match-p),
[string-find](#string-find)

------------------------------------------------------------------------

### string-match?

Check pattern in string (regex).

**Signature:** `(string-match? str pattern [fixed #f])`

**Parameters:** - **`str`** — The string to search in. - **`pattern`** —
The pattern to search for. - **`fixed`** — If \#f (default), treat as
regex; if \#t, match literally.

**Examples:**

    arl> (string-match? "hello" "^h.*o$" :fixed #f)  ; => #t
    #> TRUE
    arl> (string-match? "hello" "xyz" :fixed #f)      ; => #f
    #> FALSE
    arl> (string-match? "hello" "hello" :fixed #t)     ; => #t
    #> TRUE

**See also:** [string-contains?](#string-contains-p),
[string-find](#string-find)

------------------------------------------------------------------------

### string-find

Find pattern index or \#nil.

**Signature:** `(string-find str pattern [fixed #t])`

**Parameters:** - **`str`** — The string to search in. - **`pattern`** —
The pattern to find. - **`fixed`** — If \#t (default), match literally;
if \#f, treat as regex.

**Examples:**

    arl> (string-find "hello world" "world")  ; => 6
    #> 6
    arl> (string-find "hello world" "xyz")    ; => #nil
    arl> (string-find "hello" "l")            ; => 2
    #> 2

**See also:** [string-contains?](#string-contains-p),
[string-match?](#string-match-p)

------------------------------------------------------------------------

### string-replace

Replace first match. Default is literal (fixed); pass :fixed \#f for
regex.

**Signature:** `(string-replace str pattern replacement [fixed #t])`

**Parameters:** - **`str`** — The string to modify. - **`pattern`** —
The pattern to match. - **`replacement`** — The replacement string. -
**`fixed`** — If \#t (default), match literally; if \#f, treat as regex.

**Examples:**

    arl> (string-replace "hello world" "world" "there")    ; => "hello there"
    #> "hello there"
    arl> (string-replace "aaa" "a" "b")                     ; => "baa"
    #> "baa"

**See also:** [string-replace-all](#string-replace-all)

------------------------------------------------------------------------

### string-replace-all

Replace all matches. Default is literal (fixed); pass :fixed \#f for
regex.

**Signature:** `(string-replace-all str pattern replacement [fixed #t])`

**Parameters:** - **`str`** — The string to modify. - **`pattern`** —
The pattern to match. - **`replacement`** — The replacement string. -
**`fixed`** — If \#t (default), match literally; if \#f, treat as regex.

**Examples:**

    arl> (string-replace-all "aaa" "a" "b")   ; => "bbb"
    #> "bbb"
    arl> (string-replace-all "hello world world" "world" "there")  ; => "hello there there"
    #> "hello there there"

**See also:** [string-replace](#string-replace)

------------------------------------------------------------------------

### string-append

Concatenate strings together.

**Signature:** `(string-append args...)`

**Parameters:** - **`args`** — Zero or more strings to concatenate.

**Examples:**

    arl> (string-append "hello" " " "world")  ; => "hello world"
    #> "hello world"
    arl> (string-append "a" "b" "c")          ; => "abc"
    #> "abc"
    arl> (string-append)                       ; => ""
    #> ""

**See also:** [string-concat](#string-concat),
[string-join](#string-join)

------------------------------------------------------------------------

### -\>string

Convert value to string representation.

**Signature:** `(->string x)`

**Parameters:** - **`x`** — The value to convert to a string.

**Examples:**

    arl> (->string 42)     ; => "42"
    #> "42"
    arl> (->string #t)     ; => "TRUE"
    #> "TRUE"
    arl> (->string 3.14)   ; => "3.14"
    #> "3.14"

------------------------------------------------------------------------

### char-at

Get character at index (0-based).

**Signature:** `(char-at str index)`

**Parameters:** - **`str`** — The string to index into. - **`index`** —
Zero-based character position.

**Examples:**

    arl> (char-at "hello" 0)  ; => "h"
    #> "h"
    arl> (char-at "hello" 4)  ; => "o"
    #> "o"

**See also:** [string-ref](#string-ref), [string-slice](#string-slice)

------------------------------------------------------------------------

### string-ref

Scheme-style character accessor (alias for char-at).

**Signature:** `(string-ref str index)`

**Parameters:** - **`str`** — The string to index into. - **`index`** —
Zero-based character position.

**Examples:**

    arl> (string-ref "hello" 0)    ; => "h"
    #> "h"
    arl> (string-ref "hello" 4)    ; => "o"
    #> "o"

**See also:** [char-at](#char-at)

------------------------------------------------------------------------

### string-slice

Extract substring from start (inclusive) to end (exclusive), 0-based.

**Signature:** `(string-slice str start end)`

**Parameters:** - **`str`** — The string to slice. - **`start`** —
Zero-based start index (inclusive). - **`end`** — Zero-based end index
(exclusive).

**Examples:**

    arl> (string-slice "hello" 1 4)  ; => "ell"
    #> "ell"
    arl> (string-slice "hello" 0 5)  ; => "hello"
    #> "hello"
    arl> (string-slice "hello" 2 2)  ; => ""
    #> ""

**See also:** [char-at](#char-at)

------------------------------------------------------------------------

### string-length

Return length of string.

**Signature:** `(string-length str)`

**Parameters:** - **`str`** — The string to measure.

**Examples:**

    arl> (string-length "hello")  ; => 5
    #> 5
    arl> (string-length "")       ; => 0
    #> 0

------------------------------------------------------------------------

## String Case

Functions for converting string case.

### string-upcase

Convert string to uppercase.

**Signature:** `(string-upcase str)`

**Parameters:** - **`str`** — The string to convert.

**Examples:**

    arl> (string-upcase "hello")  ; => "HELLO"
    #> "HELLO"
    arl> (string-upcase "Hello World")  ; => "HELLO WORLD"
    #> "HELLO WORLD"

**See also:** [string-downcase](#string-downcase),
[string-titlecase](#string-titlecase)

------------------------------------------------------------------------

### string-downcase

Convert string to lowercase.

**Signature:** `(string-downcase str)`

**Parameters:** - **`str`** — The string to convert.

**Examples:**

    arl> (string-downcase "HELLO")  ; => "hello"
    #> "hello"
    arl> (string-downcase "Hello World")  ; => "hello world"
    #> "hello world"

**See also:** [string-upcase](#string-upcase),
[string-titlecase](#string-titlecase)

------------------------------------------------------------------------

### string-titlecase

Convert string to title case (capitalize first letter of each word).

**Signature:** `(string-titlecase str)`

**Parameters:** - **`str`** — The string to convert.

**Examples:**

    arl> (string-titlecase "hello world")  ; => "Hello World"
    #> "Hello World"
    arl> (string-titlecase "HELLO WORLD")  ; => "Hello World"
    #> "Hello World"

**See also:** [string-upcase](#string-upcase),
[string-downcase](#string-downcase)

------------------------------------------------------------------------

## String Comparison

Lexicographic comparison functions for strings.

### string\<?

Lexicographic less-than comparison.

**Signature:** `(string<? a b)`

**Parameters:** - **`a`** — First string. - **`b`** — Second string.

**Examples:**

    arl> (string<? "abc" "def")  ; => #t
    #> TRUE
    arl> (string<? "def" "abc")  ; => #f
    #> FALSE
    arl> (string<? "abc" "abc")  ; => #f
    #> FALSE

------------------------------------------------------------------------

### string\>?

Lexicographic greater-than comparison.

**Signature:** `(string>? a b)`

**Parameters:** - **`a`** — First string. - **`b`** — Second string.

**Examples:**

    arl> (string>? "def" "abc")  ; => #t
    #> TRUE
    arl> (string>? "abc" "def")  ; => #f
    #> FALSE
    arl> (string>? "abc" "abc")  ; => #f
    #> FALSE

------------------------------------------------------------------------

### string=?

String equality comparison.

**Signature:** `(string=? a b)`

**Parameters:** - **`a`** — First string. - **`b`** — Second string.

**Examples:**

    arl> (string=? "hello" "hello")  ; => #t
    #> TRUE
    arl> (string=? "hello" "world")  ; => #f
    #> FALSE

------------------------------------------------------------------------

### string\<=?

Lexicographic less-than-or-equal comparison.

**Signature:** `(string<=? a b)`

**Parameters:** - **`a`** — First string. - **`b`** — Second string.

**Examples:**

    arl> (string<=? "abc" "abc")  ; => #t
    #> TRUE
    arl> (string<=? "abc" "def")  ; => #t
    #> TRUE
    arl> (string<=? "def" "abc")  ; => #f
    #> FALSE

------------------------------------------------------------------------

### string\>=?

Lexicographic greater-than-or-equal comparison.

**Signature:** `(string>=? a b)`

**Parameters:** - **`a`** — First string. - **`b`** — Second string.

**Examples:**

    arl> (string>=? "abc" "abc")  ; => #t
    #> TRUE
    arl> (string>=? "def" "abc")  ; => #t
    #> TRUE
    arl> (string>=? "abc" "def")  ; => #f
    #> FALSE

------------------------------------------------------------------------

## String/List Conversion

Functions for converting between strings, lists, and numbers.

### string-\>list

Convert string to list of characters.

**Signature:** `(string->list str)`

**Parameters:** - **`str`** — The string to convert.

**Examples:**

    arl> (string->list "hello")  ; => ("h" "e" "l" "l" "o")
    #> ("h" "e" "l" "l" "o")
    arl> (string->list "")       ; => ()
    #> ()

**See also:** [list-\>string](#list-to-string)

------------------------------------------------------------------------

### list-\>string

Convert list of characters to string.

**Signature:** `(list->string lst)`

**Parameters:** - **`lst`** — A list of single-character strings.

**Examples:**

    arl> (list->string (list "h" "e" "l" "l" "o"))  ; => "hello"
    #> "hello"
    arl> (list->string (list))                        ; => ""
    #> ""

**See also:** [string-\>list](#string-to-list)

------------------------------------------------------------------------

### number-\>string

Convert number to string with optional base (2-36).

**Signature:** `(number->string num base-args...)`

**Parameters:** - **`num`** — The number to convert. - **`base-args`** —
Optional base for conversion (2, 8, 10, or 16; default 10).

**Examples:**

    arl> (number->string 42)       ; => "42"
    #> "42"
    arl> (number->string 255 16)   ; => "ff"
    #> "ff"
    arl> (number->string 10 2)     ; => "1010"
    #> "1010"
    arl> (number->string 8 8)      ; => "10"
    #> "10"

**See also:** [string-\>number](#string-to-number)

------------------------------------------------------------------------

### string-\>number

Parse string to number with optional base (2-36).

**Signature:** `(string->number str base-args...)`

**Parameters:** - **`str`** — The string to parse. - **`base-args`** —
Optional base for parsing (2, 8, 10, or 16; default 10).

**Examples:**

    arl> (string->number "42")       ; => 42
    #> 42
    arl> (string->number "3.14")     ; => 3.14
    #> 3.14
    arl> (string->number "ff" 16)    ; => 255
    #> 255
    arl> (string->number "1010" 2)   ; => 10
    #> 10
    arl> (string->number "bad")      ; => #f
    #> FALSE

**See also:** [number-\>string](#number-to-string)

------------------------------------------------------------------------

## String Predicates

Common string predicates for prefix, suffix, and emptiness checks.

### string-prefix?

Return \#t if str starts with prefix.

**Signature:** `(string-prefix? prefix str)`

**Parameters:** - **`prefix`** — String to check for at the start -
**`str`** — String to check

**Examples:**

    arl> (string-prefix? "he" "hello")  ; => #t
    #> TRUE
    arl> (string-prefix? "wo" "hello")  ; => #f
    #> FALSE

**See also:** [string-suffix?](#string-suffix-p),
[string-contains?](#string-contains-p)

------------------------------------------------------------------------

### string-suffix?

Return \#t if str ends with suffix.

**Signature:** `(string-suffix? suffix str)`

**Parameters:** - **`suffix`** — String to check for at the end -
**`str`** — String to check

**Examples:**

    arl> (string-suffix? "lo" "hello")  ; => #t
    #> TRUE
    arl> (string-suffix? "he" "hello")  ; => #f
    #> FALSE

**See also:** [string-prefix?](#string-prefix-p),
[string-contains?](#string-contains-p)

------------------------------------------------------------------------

### string-empty?

Return \#t if str is the empty string.

**Signature:** `(string-empty? str)`

**Parameters:** - **`str`** — String to check

**Examples:**

    arl> (string-empty? "")   ; => #t
    #> TRUE
    arl> (string-empty? "a")  ; => #f
    #> FALSE

**See also:** [string-length](#string-length)

------------------------------------------------------------------------

### string-repeat

Repeat string n times.

**Signature:** `(string-repeat str n)`

**Parameters:** - **`str`** — String to repeat - **`n`** — Number of
repetitions

**Examples:**

    arl> (string-repeat "ab" 3)  ; => "ababab"
    #> "ababab"
    arl> (string-repeat "x" 0)   ; => ""
    #> ""

**See also:** [string-append](#string-append)

------------------------------------------------------------------------

## Console I/O

Functions for reading from standard input and writing to standard
output.

### read-line

Read single line from stdin.

**Signature:** `(read-line [prompt ""])`

**Parameters:** - **`prompt`** — Prompt string to display before reading
(default ““)

**Examples:**

    arl> (read-line)              ; waits for user input, returns string
    arl> (read-line "Name? ")     ; prints prompt, then waits for input
    #> Name? 

------------------------------------------------------------------------

### read-from-string

**Signature:** `(read-from-string x)`

**See also:**
[read](https://willbrannon.com/arl/articles/lang-core.html#read)

------------------------------------------------------------------------

### write-string

Write string to output (alias for cat).

**Signature:** `(write-string x)`

**Parameters:** - **`x`** — Value to write to standard output

**Examples:**

    arl> (write-string "hello")  ; outputs "hello" with no newline
    #> hello

**See also:** [display](#display), [newline](#newline)

------------------------------------------------------------------------

### newline

Output a newline.

**Signature:** `(newline)`

**See also:** [display](#display), [write-string](#write-string)

------------------------------------------------------------------------

## File I/O

Functions for reading from and writing to files.

### read-file

Read entire file as string.

**Signature:** `(read-file path [encoding "UTF-8"])`

**Parameters:** - **`path`** — File path to read - **`encoding`** —
Character encoding to use (default “UTF-8”)

**Examples:**

``` arl
(read-file "data.txt")                  ; => file contents as string
(read-file "data.txt" "latin1")         ; read with specific encoding
```

------------------------------------------------------------------------

### read-lines

Read file into list of lines.

**Signature:** `(read-lines path [encoding "UTF-8"])`

**Parameters:** - **`path`** — File path to read - **`encoding`** —
Character encoding to use (default “UTF-8”)

**Examples:**

``` arl
(read-lines "data.txt")  ; => ("line1" "line2" "line3")
```

------------------------------------------------------------------------

### write-file

Write string or lines to file.

**Signature:** `(write-file path content [sep "\n"])`

**Parameters:** - **`path`** — File path to write to - **`content`** —
String or list of lines to write - **`sep`** — Separator used to join
lines (default “”) - **`encoding`** — Character encoding to use (default
“UTF-8”)

**Examples:**

``` arl
(write-file "out.txt" "hello world")
(write-file "out.txt" (list "line1" "line2"))
```

------------------------------------------------------------------------

### write-lines

Write list of lines to file.

**Signature:** `(write-lines path lines [encoding "UTF-8"])`

**Parameters:** - **`path`** — File path to write to - **`lines`** —
List of lines to write - **`encoding`** — Character encoding to use
(default “UTF-8”)

**Examples:**

``` arl
(write-lines "out.txt" (list "line1" "line2" "line3"))
```

------------------------------------------------------------------------

### append-file

Append content to file.

**Signature:** `(append-file path content [sep "\n"])`

**Parameters:** - **`path`** — File path to append to - **`content`** —
String or list of lines to append - **`sep`** — Separator used to join
lines (default “”) - **`encoding`** — Character encoding to use (default
“UTF-8”)

**Examples:**

``` arl
(append-file "log.txt" "new entry\n")
```

------------------------------------------------------------------------

### file-exists?

Return \#t if file exists.

**Signature:** `(file-exists? path)`

**Parameters:** - **`path`** — File path to check

**Examples:**

``` arl
(file-exists? "data.txt")    ; => #t or #f
(file-exists? "nope.txt")    ; => #f
```

------------------------------------------------------------------------

### file-size

Return size of file in bytes.

**Signature:** `(file-size path)`

**Parameters:** - **`path`** — File path to query

**Examples:**

``` arl
(file-size "data.txt")       ; => 1024 (bytes)
```

------------------------------------------------------------------------

### file-modified-time

Return file modification time as numeric timestamp.

**Signature:** `(file-modified-time path)`

**Parameters:** - **`path`** — File path to query

**Examples:**

``` arl
(file-modified-time "data.txt")  ; => 1700000000 (numeric timestamp)
```

------------------------------------------------------------------------

### file-delete

Delete file. Return \#t on success.

**Signature:** `(file-delete path)`

**Parameters:** - **`path`** — File path to delete

**Examples:**

``` arl
(file-delete "temp.txt")     ; => #t
```

------------------------------------------------------------------------

## Directory Operations

Functions for querying and manipulating directories.

### directory-exists?

Return \#t if directory exists.

**Signature:** `(directory-exists? path)`

**Parameters:** - **`path`** — Directory path to check

**Examples:**

    arl> (directory-exists? "/tmp")         ; => #t
    #> TRUE
    arl> (directory-exists? "/nonexistent") ; => #f
    #> FALSE

------------------------------------------------------------------------

### directory-list

List directory contents as list of filenames.

**Signature:** `(directory-list path [full.names #f])`

**Parameters:** - **`path`** — Directory path to list - **`full.names`**
— When \#t, return full file paths instead of just names (default \#f)

**Examples:**

``` arl
(directory-list ".")               ; => ("file1.txt" "file2.txt")
(directory-list "." #t)            ; => ("./file1.txt" "./file2.txt")
```

------------------------------------------------------------------------

### directory-delete

Delete directory. Return \#t on success.

**Signature:** `(directory-delete path [recursive #t])`

**Parameters:** - **`path`** — Directory path to delete -
**`recursive`** — When \#t, delete contents recursively (default \#t)

**Examples:**

``` arl
(directory-delete "/tmp/mydir")    ; => #t
```

------------------------------------------------------------------------

## Environment and System

Functions for environment variables, shell commands, and process
control.

### getenv

Get environment variable value. Return \#nil if not set.

**Signature:** `(getenv name)`

**Parameters:** - **`name`** — Environment variable name

**Examples:**

    arl> (getenv "HOME")           ; => "/home/user"
    #> "/home/runner"
    arl> (getenv "UNDEFINED_VAR")  ; => #nil

**See also:** [setenv](#setenv)

------------------------------------------------------------------------

### setenv

Set environment variable.

**Signature:** `(setenv name value)`

**Parameters:** - **`name`** — Environment variable name - **`value`** —
Value to assign to the variable

**Examples:**

    arl> (setenv "MY_VAR" "hello")  ; => #t
    #> TRUE

**See also:** [getenv](#getenv)

------------------------------------------------------------------------

### system-output

Execute shell command and capture output as string.

**Signature:** `(system-output command)`

**Parameters:** - **`command`** — Shell command string to execute

**Examples:**

    arl> (system-output "whoami")   ; => "username"
    #> "runner"
    arl> (system-output "echo hi")  ; => "hi"
    #> "hi"

------------------------------------------------------------------------

### exit

Exit program with status code.

**Signature:** `(exit status)`

**Parameters:** - **`status`** — Exit status code (default 0)

------------------------------------------------------------------------

### format-value

Format value for display.

**Signature:** `(format-value x)`

**Parameters:** - **`x`** — Value to format as a string

------------------------------------------------------------------------

## Display and Output

Functions for formatting and printing values.

### display

Print value with newline.

**Signature:** `(display x)`

**Parameters:** - **`x`** — Value to print

**Examples:**

    arl> (display 42)             ; prints "42" with newline
    #> 42 
    arl> (display "hello")        ; prints "hello" with newline
    #> "hello" 
    arl> (display (list 1 2 3))   ; prints "(1 2 3)" with newline
    #> (1 2 3) 

**See also:** [println](#println), [trace](#trace)

------------------------------------------------------------------------

### println

Alias for display.

**Signature:** `(println x)`

**Parameters:** - **`x`** — Value to print

**Examples:**

    arl> (println 42)               ; prints "42\n"
    #> 42 
    arl> (println (list 1 2 3))     ; prints "(1 2 3)\n"
    #> (1 2 3) 

**See also:** [display](#display)

------------------------------------------------------------------------

### string-concat

Concatenate values into a string. Strings are included as-is (unquoted);
other values are formatted.

**Signature:** `(string-concat args...)`

**Parameters:** - **`args`** — Values to concatenate together

**Examples:**

    arl> (string-concat "hello" " " "world")  ; => "hello world"
    #> "hello world"
    arl> (string-concat 1 "+" 2)              ; => "1+2"
    #> "1+2"

**See also:** [string-join](#string-join),
[string-format](#string-format)

------------------------------------------------------------------------

### trace

Print value and return it.

**Signature:** `(trace x rest...)`

**Parameters:** - **`x`** — Value to print and return - **`rest`** —
Optional label string to prefix the output

**Examples:**

    arl> (trace 42)               ; prints "42", returns 42
    #> 42 
    #> 42
    arl> (trace 42 "result")      ; prints "result: 42", returns 42
    #> "result" : 42 
    #> 42

**See also:**
[warn](https://willbrannon.com/arl/articles/lang-core.html#warn),
[display](#display)

------------------------------------------------------------------------
