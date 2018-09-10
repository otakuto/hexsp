# Hexsp
Hexsp is an esolang.

## Feature
* Based on lisp.
* Program is represented by list of hexadecimal numbers.

## Run
```
$ runhaskell hexsp.hs
```
```
$ runhaskell hexsp.hs examples/add1to100.hxp
```

## Examples

Nil
```
hexsp> EE 33
lisp>  ()
```

Space
```
hexsp> EE 00 33
lisp>  ( )
```

Spaces
```
hexsp> 00 EE 00 00 33 00
lisp>   (  )
```

Number
```
hexsp> 55 01
lisp>  1

hexsp> 55 01 01
lisp>  257
```

Negative number
```
hexsp> 22 01
lisp>  -1

hexsp> 22 01 01
lisp>  -257
```

Quote
```
hexsp> 99 EE 55 01 55 02 33
lisp>  '(1 2)

hexsp> EE 92 07 E0 EE 55 01 33 33
lisp>  (quote (1))
```

Symbol
```
hexsp> DE AD BE EF
lisp>  DEADBEEF

hexsp> 01 01
lisp>  0101

hexsp> 0A 0A
lisp>  0A0A
```

True
```
hexsp> 77
lisp>  t
```

Dotted Pair
```
hexsp> EE 55 01 11 55 02 33
lisp>  (1 . 2)

hexsp> EE 55 01 11 EE 55 02 11 EE 33 33 33
lisp>  (1 . (2 . ()))
```

Escape
```
hexsp> 55 FF 55
lisp>  85

hexsp> 55 FF FF
lisp>  255

hexsp> 55 FF 00
lisp>  0

hexsp> 22 FF 00
lisp>  0

hexsp> AA FF 11 FF 22 FF 33 FF 55 FF 99 FF EE FF FF FF 00 BB
lisp>  AA1122335599EEFF00BB
```

## Function

car  
CA 70
```
hexsp> EE CA 70 EE 55 01 33 33
01
```

cdr  
CD 70

cons  
C0 25

atom  
A7 03

quote  
92 07 E0

write  
37 17 E0
```
hexsp> EE 37 17 E0 55 01 33
55 01
```

begin  
BE 91 20
```
hexsp> EE BE 91 20 EE 00 EE 37 17 E0 EE 33 33 00 00 EE 37 17 E0 EE 33 33 00 33 33
EE 33
EE 33
```

eq  
E9

add  
AD D0
```
hexsp> EE AD D0 55 01 55 02 33
55 03
```

sub  
52 B0

setq  
5E 79

if  
1F
```
hexsp> EE 1F 00 77 00 55 01 55 02 33
55 01
hexsp> EE 1F 00 EE 33 00 55 01 55 02 33
55 02
```

defunc  
DE F2 2C
```
hexsp> EE DE F2 2C 00 0F 00 EE 01 00 02 33 EE AD D0 00 01 00 02 33 33 EE 37 17 E0 EE 0F 00 55 01 55 02 33 33
55 03
```

