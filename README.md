# Monads in Racket, A Dynamically-Typed Language

This library demonstrates an approach to monads in a dynamically-typed
language, including a way of achieving return-type polymorphism. It
started off as an experiment, documented in
[this blog post][eighty-twenty-blog-post], which see for motivation
and comparison to other approaches etc.

The code uses [Racket](http://racket-lang.org/)'s
[generic interfaces][generics] feature in place of Haskell's type
classes.

The approach taken here employs two techniques: (1) *coercions*
between monad representations, and (2) "undecided" quasi-monadic
placeholder values. Taken together, these give a Haskell-like feel for
monads in a dynamically-typed language.

The techniques are illustrated using Racket, but aren't specific to
Racket.

  [eighty-twenty-blog-post]: http://eighty-twenty.org/2015/01/25/monads-in-dynamically-typed-languages/
  [generics]: http://docs.racket-lang.org/reference/struct-generics.html
  [oleg-article]: http://okmij.org/ftp/Scheme/monad-in-Scheme.html

## Example

[Oleg's article on monads in Scheme][oleg-article] gave a kind of
challenge problem, near the end. It uses many different types of monad
together, using the monomorphic technique he developed:

```racket
(IO::run
 (IO::>> (put-line "Enter a number: ")
         (IO::>>= (read-int)
                  (λ (n)
                    (IO::>>= (IO::return (for/list [(i n)] i))
                             (λ (all-n)
                               (IO::>>=
                                (IO::return
                                 (List:>>= all-n
                                           (λ (i)
                                             (if (even? i)
                                                 (List::return i)
                                                 (List::fail "odd")))))
                                (λ (evens) (IO::return evens)))))))))
```

With the polymorphic monads provided by this library, we are able to
use our generic `do`-notation macro to write instead

```racket
(run-io (do (mdisplay "Enter a number: ")
            n <- mread
            all-n <- (return (for/list [(i n)] i))
            evens <- (return (do i <- all-n
                                 #:guard (even? i)
                                 (return i)))
            (return evens)))
```

This compares favourably to the Haskell

```haskell
main = do putStr "Enter a number: "
          n <- getInt
          allN <- return [0 .. n-1]
          evens <- return $ do i <- allN
                               guard $ i `mod` 2 == 0
                               return i
          return evens
```

## Copyright and License

Copyright &copy; 2015 Tony Garnock-Jones

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

See the files `lgpl.txt` and `gpl.txt` for the relevant license terms.
