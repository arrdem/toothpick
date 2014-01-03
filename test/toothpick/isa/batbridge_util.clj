(ns toothpick.isa.batbridge-util)

;; This code is imported from the Batbridge simulator, and provides binary
;; encoding inspection and disassembly for Batbridge instructions.
;; ------------------------------------------------------------------------------
(defn word->opcode
  "Pulls the opcode bits out of a word"

  [word]
  (-> word
      (bit-shift-right 26)
      (bit-and 0x3f)))


(defn word->dst 
  "Pulls the destination bits out of a word"

  [word]
  (-> word
      (bit-shift-right 21)
      (bit-and 0x1f)))


(defn word->srca 
  "Pulls the source A bits out of a word"

  [word]
  (-> word
      (bit-shift-right 16)
      (bit-and 0x1f)))


(defn word->srcb 
  "Pulls the source B bits out of a word"

  [word]
  (-> word
      (bit-shift-right 11)
      (bit-and 0x1f)))


(defn word->lit 
  "Pulls the literal bits out of a word"

  [word]
  (let [frag (bit-and word 0x7ff)]
    (if (= 1024 (bit-and frag 1024)) ;; is the top bit set?
      (bit-or -1024 frag)            ;; mask in all the higher bits
      frag)))


(def bytecode->opcode
  "Maps bytecodes to their opcodes as per the spec."

  {0x00 :htl
   0x10 :ld
   0x11 :st
   0x20 :iflt
   0x21 :ifle
   0x22 :ifne
   0x30 :add
   0x31 :sub
   0x32 :div
   0x33 :mod
   0x34 :mul
   0x35 :and
   0x36 :or
   0x37 :nand
   0x38 :xor
   0x3A :sl
   0x3B :sr
   0x3C :sal
   0x3D :sar})


(defn word->symbol-map
  "Pulls appart a word, building the symbolic assembler map which the
  Clojure simulators are designed to work with and which a human can
  reasonably debug."

  [word]
  {:icode (#(get bytecode->opcode %1 %1)
           (word->opcode word))
   :d   (word->dst word)
   :a  (word->srca word)
   :b  (word->srcb word)
   :i (word->lit word)})
