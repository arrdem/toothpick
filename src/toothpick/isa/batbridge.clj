(ns toothpick.isa.batbridge
  (:require [toothpick.architecture :refer :all]))


(defn register? [x]
  (and (number? x)
       (< x 32)
       (>= x 0)))


(defn literal? [i]
  (and (< i 1024)
       (>= i -1024)))


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


; Define the BatBridge instruction set
;-------------------------------------------------------------------------------

(defmacro bb-opcode [thread name code]
  `(opcode ~thread ~name
           (const-field :icode 6 ~code)
           (parameter-field :d 5 register?)
           (parameter-field :a 5 register?)
           (parameter-field :b 5 register?)
           (signed-parameter-field :i 11 literal?)))

; hex digit: literal value
;; t : target register
;; s : source register
;; a : register representing left operand
;; b : register representing right operand
;; x : register representing index (multiplied by operand size)
;; i : immediate signed quantity
;; _ : value is not read. Set to 0 when assembled.

(define-architecture batbridge
      ;; HLT 0x00   000000 _____ _____ _____ ___________
      ;; halts the machine immediately
      (opcode :hlt
              (const-field :icode 6  0)
              (const-field :_0    5  0)
              (const-field :_1    5  0)
              (const-field :_2    5  0)
              (const-field :_3    11 0))

      ;; LD  0x10   010000 ttttt aaaaa xxxxx iiiiiiiiiii
      ;; loads the word (+ a (* 4 x)) to register dst
      (opcode :ld 
              (const-field     :icode 6  0x10)
              (parameter-field :d     5  register?)
              (parameter-field :a     5  register?)
              (parameter-field :x     5  register?)
              (signed-parameter-field :i 11 literal?))

      ;; ST  0x11   010001 sssss aaaaa xxxxx iiiiiiiiiii
      ;; stores the word in register src to (+ a (* 4 x))
      (opcode :st
              (const-field     :icode 6  0x11)
              (parameter-field :s     5  register?)
              (parameter-field :a     5  register?)
              (parameter-field :x     5  register?)
              (signed-parameter-field :i 11 literal?))
 
      ;; IFLT 0x20  100000 _____ aaaaa bbbbb iiiiiiiiiii
      ;; execute the next instruction IFF (< a b)
      (opcode :iflt
              (const-field     :icode 6  0x20)
              (const-field     :_     5  0)
              (parameter-field :a     5  register?)
              (parameter-field :b     5  register?)
              (parameter-field :i     11 literal?))
      
      ;; IFLE 0x21  100001 _____ aaaaa bbbbb iiiiiiiiiii
      ;; execute the next instruction IFF (<= a b)
      (opcode :ifle
              (const-field     :icode 6  0x21)
              (const-field     :_     5  0)
              (parameter-field :a     5  register?)
              (parameter-field :b     5  register?)
              (parameter-field :i     11 literal?))
      
      ;; IFEQ 0x22  100010 _____ aaaaa bbbbb iiiiiiiiiii
      ;; execute the next instruction IFF (= a b)
      (opcode :ifeq
              (const-field     :icode 6  0x22)
              (const-field     :_     5  0)
              (parameter-field :a     5  register?)
              (parameter-field :b     5  register?)
              (parameter-field :i     11 literal?))
      
      ;; IFNE 0x23  100013 _____ aaaaa bbbbb iiiiiiiiiii
      ;; execute the next instruction IFF (!= a b)
      (opcode :ifne
              (const-field     :icode 6  0x23)
              (const-field     :_     5  0)
              (parameter-field :a     5  register?)
              (parameter-field :b     5  register?)
              (parameter-field :i     11 literal?))
      
      ;; ADD  0x30  110000 ttttt aaaaa bbbbb iiiiiiiiiii
      ;; stores (+ a b) to t
      (bb-opcode :add 0x30)
      
      ;; SUB  0x31  110001 ttttt aaaaa bbbbb iiiiiiiiiii
      ;; stores (- a b) to t
      (bb-opcode :sub 0x31)
      
      ;; DIV  0x32  110010 ttttt aaaaa bbbbb iiiiiiiiiii
      ;; stores the integer division of (/ a b) to t
      (bb-opcode :div 0x32)
      
      ;; MOD  0x33  110011 ttttt aaaaa bbbbb iiiiiiiiiii
      ;; stores the integer remainder (mod a b) to t
      (bb-opcode :mod 0x33)
      
      ;; MUL  0x34  110100 ttttt aaaaa bbbbb iiiiiiiiiii
      ;; stores the product (* a b) to t
      (bb-opcode :mul 0x34)
      
      ;; AND  0x35  110101 ttttt aaaaa bbbbb iiiiiiiiiii
      ;; stores (bit-and a b) to t
      (bb-opcode :and 0x35)
      
      ;; OR   0x36  110110 ttttt aaaaa bbbbb iiiiiiiiiii
      ;; stores (bit-or a b) to t
      (bb-opcode :or 0x36)

      ;; NAND 0x37  110111 ttttt aaaaa bbbbb iiiiiiiiiii
      ;; stores (bit-not (bit-and a b)) to t
      (bb-opcode :nand 0x37)
      
      ;; XOR  0x38  111000 ttttt aaaaa bbbbb iiiiiiiiiii
      ;; stores (bit-xor a b) to t
      (bb-opcode :xor 0x38)
      
      ;; SL   0x3a  111010 ttttt aaaaa bbbbb iiiiiiiiiii
      ;; stores (bit-shift-left a b) to t
      (bb-opcode :sl 0x3a)
      
      ;; SR  0x3b  111011 ttttt aaaaa bbbbb iiiiiiiiiii
      ;; stores the right shift of a, b bits to t
      (bb-opcode :sr 0x3b)
      
      ;; SAL  0x3c 111100 ttttt aaaaa bbbbb iiiiiiiiiii
      ;; stores the arithmatic shift left of a b bits to t
      (bb-opcode :sal 0x3c)

      ;; SAR  0x3d 111101 ttttt aaaaa bbbbb iiiiiiiiiii
      ;; stores the arithmatic right shift of a by b bits to t
      (bb-opcode :sar 0x3d))
