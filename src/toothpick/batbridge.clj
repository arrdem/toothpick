(ns toothpick.batbridge
  (:require [clojure.string :refer [lower-case]]
            [toothpick.core :refer [bit-fmt]]))

; hex digit: literal value
;; t : target register
;; s : source register
;; a : register representing left operand
;; b : register representing right operand
;; x : register representing index (multiplied by operand size)
;; i : immediate signed quantity
;; _ : value is not read. Set to 0 when assembled.

(defn register? [x]
  (and (number? x)
       (< x 32)
       (>= x 0)))

; Registers
;-------------------------------------------------------------------------------
;; The PC 31 (0b11111)
;;   - reading it produces the PC of the next instruction
;;   - having it as a target causes a branch
(def r_PC 31)

;; The Zero register 30 (0b11110)
;;   - always read as 0
;;   - writing to it prints its value on the console (ASCII)
(def r_ZERO 30)

;; The Immediate value 29 (0b1101)
;;   - when read produces the 11 bit immediate field in the instruction
;;   - writing to it prints its value on the console (HEX)
(def r_IMM 29)

(def register-symbol-map
  (-> (reduce (fn [m v]
                (assoc m v v))
              {} (range 32))
      (assoc 31 :r_PC)
      (assoc 30 :r_ZERO)
      (assoc 29 :r_IMM)))

; Opcode decoding & encoding helpers
(def common-layout
  [7 5 5 5 11])

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
  (bit-and word 0x7ff))

(defn decode-args 
  "Disassembles a word into its' stock members"
  [word]
  {:dst   (register-symbol-map (word->dst word))
   :srca  (register-symbol-map (word->srca word))
   :srcb  (register-symbol-map (word->srcb word))
   :lit   (word->lit word)})

(defmacro defopcode 
  "A grand utility macro for quickly defining the reader and assembler
  for some opcode. This macro generates two function definitions, one
  of which encodes its arguments into an instruction word after
  verifying that the arguments match their given constraints." 
  [icode code fmt-args]
  (let [pred-args (filter symbol? fmt-args)
        fmt-args  (map #(if (symbol? %1) 
                          (gensym) %1) fmt-args)
        syms      (filter symbol? fmt-args)
        nice-name (lower-case (name icode))]
    `(do (defn ~(symbol (str "assemble-" nice-name))
           ~(apply vector syms)
           ~@(map (fn [x y] `(do (assert (~x ~y))))
                  pred-args
                  syms)
           (bit-fmt common-layout ~code ~@fmt-args))
         (defn ~(symbol (str "read-" nice-name))
           [word#]
           (when (= ~code (word->opcode word#))
             (-> word#
                 (decode-args)
                 (assoc :opcode ~(keyword nice-name))))))))

;-------------------------------------------------------------------------------
;; HLT 0x00   000000 _____ _____ _____ ___________
(defopcode htl 0x00 [0 0 0 0])

;; LD  0x10   010000 ttttt aaaaa xxxxx iiiiiiiiiii
(defopcode ld 0x10 [register? register? register? number?])

;; ST  0x11   010001 sssss aaaaa xxxxx iiiiiiiiiii
(defopcode st 0x11 [register? register? register? number?])

;; IFLT 0x20  100000 _____ aaaaa bbbbb iiiiiiiiiii
(defopcode iflt 0x20 [0 register? register? number?])

;; IFLE 0x21  100001 _____ aaaaa bbbbb iiiiiiiiiii
(defopcode ifle 0x21 [0 register? register? number?])

;; IFEQ 0x22  100010 _____ aaaaa bbbbb iiiiiiiiiii
(defopcode ifeq 0x22 [0 register? register? number?])

;; IFNE 0x23  100013 _____ aaaaa bbbbb iiiiiiiiiii
(defopcode ifne 0x21 [0 register? register? number?])

;; ADD  0x30  110000 ttttt aaaaa bbbbb iiiiiiiiiii
(defopcode add 0x30 [register? register? register? number?])

;; SUB  0x31  110001 ttttt aaaaa bbbbb iiiiiiiiiii
(defopcode sub 0x31 [register? register? register? number?])

;; DIV  0x32  110010 ttttt aaaaa bbbbb iiiiiiiiiii
(defopcode div 0x32 [register? register? register? number?])

;; MOD  0x33  110011 ttttt aaaaa bbbbb iiiiiiiiiii
(defopcode mod 0x33 [register? register? register? number?])

;; MUL  0x34  110100 ttttt aaaaa bbbbb iiiiiiiiiii
(defopcode mul 0x34 [register? register? register? number?])

;; AND  0x35  110101 ttttt aaaaa bbbbb iiiiiiiiiii
(defopcode and 0x35 [register? register? register? number?])

;; OR   0x36  110110 ttttt aaaaa bbbbb iiiiiiiiiii
(defopcode or 0x36 [register? register? register? number?])

;; NAND 0x37  110111 ttttt aaaaa bbbbb iiiiiiiiiii
(defopcode nand 0x37 [register? register? register? number?])

;; XOR  0x38  111000 ttttt aaaaa bbbbb iiiiiiiiiii
(defopcode xor 0x38 [register? register? register? number?])

;; SL   0x3a  111010 ttttt aaaaa bbbbb iiiiiiiiiii
(defopcode sl 0x3a [register? register? register? number?])

;; SAR  0x3b  111011 ttttt aaaaa bbbbb iiiiiiiiiii
(defopcode sar 0x3b [register? register? register? number?])

;; SLR  0x3c  111100 ttttt aaaaa bbbbb iiiiiiiiiii
(defopcode slr 0x3c [register? register? register? number?])

