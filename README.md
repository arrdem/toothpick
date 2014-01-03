# Toothpick

> I was taught assembler in my second year at school,<br>
> It's kind of like construction work<br>
> With a toothpick, for a tool<br>
> So when I made my senior year I threw my code away<br>
> And learned the way to program, that I still prefer today<br>
> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ~ The Eternal Flame

Toothpick is a toolkit for idiomaticly generating inspectable assembler bytecode
from Clojure. It is intended for use in compilers and translators, but the truly
adventurous could no doubt perform manual code generation and force the JVM to
execute the result.

Toothpick is joined at the hip to
[Batbridge](https://github.com/arrdem/batbridge), which is the development
target architecture for Toothpick.

## Status

Toothpick is nearing an `0.1.0` state, as the assembler itself is almost feature
complete and Toothpick has successfully build and run code on bytecode enabled
Batbridge simulators. The `0.1.0` release will become a dependency of the
batbridge project and will be used in the test suite thereof for program
compilation.

Soon to come is a JVM bytecode module, followed by some subset of Intel
x86_64. These will be paired with matching simulators and tools for running the
bytecode on "bare metal" so it may be some time.

**Working Components**

 - `toothpick.architecture` provides a sane and probably stable
   interface for defining the instructions which constitute an
   instruction set.
 - `toothpick.architecture` ISAs provide the information needed to
   build an `(instruction, params)` pair to raw bytes.
 - `toothpick.isa.batbridge` provides a full definition of the
   Batbridge instruction set used as the target of my blog post series
   on processor design and implementation.

## Usage

```Clojure
user> (require '[toothpick.assembler :refer [assemble]]])
nil
user> (require '[toothpick.isa.batbridge :as bb])
nil
user> (assemble bb/batbridge
       [[:add 0 29 30 15]
	    [:sub 1 30 29 15]])
(3223187471 3292456975)
user> (assemble bb/batbridge
       [:start
	    [:add 0 29 30 15]
		[:sub 1 30 29 15]
		:end
		[:add 31 30 29 :start]])
(3223187471 3292456975 3288262656)
```

## License

Copyright © 2013 Reid "arrdem" McKenzie

Distributed under the Eclipse Public License, the same as Clojure.
