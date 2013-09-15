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
       (< 32 x)
       (>= 0 x)))

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

; Opcodes
(def common-layout
  [7 5 5 5 11])

(defn word->opcode [word]
  (-> word
      (bit-shift-right 26)
      (bit-and 0x3f)))

(defn word->dst [word]
  (-> word
      (bit-shift-right 21)
      (bit-and 0x1f)))

(defn word->srca [word]
  (-> word
      (bit-shift-right 16)
      (bit-and 0x1f)))

(defn word->srcb [word]
  (-> word
      (bit-shift-right 11)
      (bit-and 0x1f)))

(defn word->lit [word]
  (bit-and word 0x7ff))

(defn decode-args [word]
  {:dst   (word->dst word)
   :srca  (word->srca word)
   :srcb  (word->srcb word)
   :lit   (word->lit word)})

(defmacro defopcode [icode code fmt-args]
  (let [syms (filter symbol? fmt-args)]
    `(do (defn ~(symbol (str "assemble-" (lower-case (name icode))))
           ~(apply vector syms)
           (bit-fmt common-layout ~code ~@fmt-args))
         (defn ~(symbol (str "read-" (lower-case (name icode))))
           [word#]
           (when (= ~code (word->opcode word#))
             (-> word#
                 (decode-args)
                 (assoc :opcode ~(keyword (lower-case (name icode))))))))))

;-------------------------------------------------------------------------------
;; HLT 0x00   000000 _____ _____ _____ ___________
(defopcode htl 0x00 [0 0 0 0])
;; LD  0x10   010000 ttttt aaaaa xxxxx iiiiiiiiiii
(defopcode ld 0x10 [t a x i])
;; ST  0x11   010001 sssss aaaaa xxxxx iiiiiiiiiii
(defopcode st 0x11 [s a x i])
;; IFLT 0x20  100000 _____ aaaaa bbbbb iiiiiiiiiii
(defopcode iflt 0x20 [0 a b i])
;; IFLE 0x21  100001 _____ aaaaa bbbbb iiiiiiiiiii
(defopcode ifle 0x21 [0 a b i])
;; IFEQ 0x22  100010 _____ aaaaa bbbbb iiiiiiiiiii
(defopcode ifeq 0x22 [0 a b i])
;; IFNE 0x23  100013 _____ aaaaa bbbbb iiiiiiiiiii
(defopcode ifne 0x21 [0 a b i])
;; ADD  0x30  110000 ttttt aaaaa bbbbb iiiiiiiiiii
(defopcode add 0x30 [t a b i])
;; SUB  0x31  110001 ttttt aaaaa bbbbb iiiiiiiiiii
(defopcode sub 0x31 [t a b i])
;; DIV  0x32  110010 ttttt aaaaa bbbbb iiiiiiiiiii
(defopcode div 0x32 [t a b i])
;; MOD  0x33  110011 ttttt aaaaa bbbbb iiiiiiiiiii
(defopcode mod 0x33 [t a b i])
;; MUL  0x34  110100 ttttt aaaaa bbbbb iiiiiiiiiii
(defopcode mul 0x34 [t a b i])
;; AND  0x35  110101 ttttt aaaaa bbbbb iiiiiiiiiii
(defopcode and 0x35 [t a b i])
;; OR   0x36  110110 ttttt aaaaa bbbbb iiiiiiiiiii
(defopcode or 0x36 [t a b i])
;; NAND 0x37  110111 ttttt aaaaa bbbbb iiiiiiiiiii
(defopcode nand 0x37 [t a b i])
;; XOR  0x38  111000 ttttt aaaaa bbbbb iiiiiiiiiii
(defopcode xor 0x38 [t a b i])
;; SL   0x3a  111010 ttttt aaaaa bbbbb iiiiiiiiiii
(defopcode sl 0x3a [t a b i])
;; SAR  0x3b  111011 ttttt aaaaa bbbbb iiiiiiiiiii
(defopcode sar 0x3b [t a b i])
;; SLR  0x3c  111100 ttttt aaaaa bbbbb iiiiiiiiiii
(defopcode slr 0x3c [t a b i])

