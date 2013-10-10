(ns toothpick.batbridge
  (:require [clojure.string :refer [lower-case]]
            [toothpick.core :refer [bit-fmt bit-mask-n]]))

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
;-------------------------------------------------------------------------------
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

;; Idea: 
;;
;;    This is a new notation idea that totally struck me the night of
;;    10/5. The notation used below uses predicates (or a constant)
;;    and a number of bits to create an encoder/decoder pair for every
;;    opcode in the ISA as specified. However the generated code is
;;    not especially meaningful, and will almost certainly discard
;;    notation specifics such as field names because the original
;;    notation does not support it.
;;
;;    The solution to this problem, is to allow the user to detail the
;;    symbolic name as well as the type of each opcode field. One
;;    possible notation is
;;
;;    (opcode htl 32
;;            (const-field :icode 6  0x00)
;;            (const-field :dst   5  0x00)
;;            (const-field :srca  5  0x00)
;;            (const-field :srcb  5  0x00)
;;            (const-field :lit   11 0x00))
;;
;;    But that's an easy instruction, it has a constant encode and
;;    decode. How about another instruction with value ranges?
;;
;;    (opcode ld 32
;;            (const-field     :icode 6 0x10)
;;            (parameter-field :dst   5  register?)
;;            (parameter-field :srca  5  register?)
;;            (parameter-field :srcb  5  register?)
;;            (parameter-field :const 11 integer?))
;;
;;    The great value proposition of this approach is that it allows
;;    each instruction to have its own encode/decode pair which can
;;    round-trip symbolic meaning. So if there's one special case
;;    instruction which must have a bitfield set, or which supports
;;    some other single bit flag that no other icode does this
;;    notation naturally handles it because the decoder is generated
;;    on a per instruction basis and is "fully aware" of the user's
;;    desired notation.


;; subsystem for constructing icode descriptors
;;------------------------------------------------------------------------------
(defn const-field [sym width const]
  {:name  sym
   :type  :const
   :width width
   :value const
   :pred  #(= %1 const)})

(defn parameter-field [sym width pred]
  {:name  sym
   :type  :field
   :width width
   :pred  pred})

(defn add-field [icode field-map]
  (let [field-map (assoc field-map :offset 
                         (->> icode
                              :fields
                              (map :width)
                              (reduce +)))]
    (-> icode
        (update-in [:fields] conj field-map))))

(defn opcode [isa sym & fields]
  (let [opcode-repr (reduce add-field 
                            {} fields)]
    (assoc-in isa [:icodes (keyword (name sym))] opcode-repr)))


;; subsystem for using icode descriptors to assemble code
;;------------------------------------------------------------------------------
(defn encode-field [icode field val]
  (when ((:pred field) val)
    (bit-shift-left (bit-and (bit-mask-n (:width field))
                              val)
                     (:offset field))))

(defn list->bytecode 
  "Compiles a list that a programmer could actually type or generate
  by hand into an assembled word."

  [isa opcode & tail]
  (let [icode (get (:icodes isa) 
                   opcode)
        fields (reverse (:fields icode))
        encoding (map (partial encode-field icode)
                      fields
                      tail)]
    (println encoding)

    (reduce bit-or
            0 encoding)))

(defn map->bytecode 
  "Compiles a map which the disassembler would produce into a word."
  [isa opcode map]
  )

;; subsystem for using icode interpreters to disassemble code
;; ------------------------------------------------------------------------------
;; Basically this is going to be a parser from a single binary word to
;; the internal representation of an attr map


(defmacro bb-opcode [thread name code]
  `(opcode ~thread ~name
           (const-field :icode 6 ~code)
           (parameter-field :dst 5 register?)
           (parameter-field :srca 5 register?)
           (parameter-field :srcb 5 register?)
           (parameter-field :const 11 integer?)))

(def batbridge
  (-> {:word 32}
;-------------------------------------------------------------------------------
;; HLT 0x00   000000 _____ _____ _____ ___________
      (opcode :htl
              (const-field :icode 6 0)
              (const-field :dst   5 0)
              (const-field :srca  5 0)
              (const-field :srcb  5 0)
              (const-field :lit   11 0))

;; LD  0x10   010000 ttttt aaaaa xxxxx iiiiiiiiiii
      (bb-opcode :ld 0x10)

;; ST  0x11   010001 sssss aaaaa xxxxx iiiiiiiiiii
      (bb-opcode :st 0x11)
 
;; IFLT 0x20  100000 _____ aaaaa bbbbb iiiiiiiiiii
      (bb-opcode :iflt 0x20)

;; IFLE 0x21  100001 _____ aaaaa bbbbb iiiiiiiiiii
      (bb-opcode :ifle 0x21)

;; IFEQ 0x22  100010 _____ aaaaa bbbbb iiiiiiiiiii
      (bb-opcode :ifeq 0x22)

;; IFNE 0x23  100013 _____ aaaaa bbbbb iiiiiiiiiii
      (bb-opcode :ifne 0x21)

;; ADD  0x30  110000 ttttt aaaaa bbbbb iiiiiiiiiii
      (bb-opcode :add 0x30)

;; SUB  0x31  110001 ttttt aaaaa bbbbb iiiiiiiiiii
      (bb-opcode :sub 0x31)

;; DIV  0x32  110010 ttttt aaaaa bbbbb iiiiiiiiiii
      (bb-opcode :div 0x32)

;; MOD  0x33  110011 ttttt aaaaa bbbbb iiiiiiiiiii
      (bb-opcode :mod 0x33)

;; MUL  0x34  110100 ttttt aaaaa bbbbb iiiiiiiiiii
      (bb-opcode :mul 0x34)

;; AND  0x35  110101 ttttt aaaaa bbbbb iiiiiiiiiii
      (bb-opcode :and 0x35)

;; OR   0x36  110110 ttttt aaaaa bbbbb iiiiiiiiiii
      (bb-opcode :or 0x36)

;; NAND 0x37  110111 ttttt aaaaa bbbbb iiiiiiiiiii
      (bb-opcode :nand 0x37)

;; XOR  0x38  111000 ttttt aaaaa bbbbb iiiiiiiiiii
      (bb-opcode :xor 0x38)

;; SL   0x3a  111010 ttttt aaaaa bbbbb iiiiiiiiiii
      (bb-opcode :sl 0x3a)

;; SAR  0x3b  111011 ttttt aaaaa bbbbb iiiiiiiiiii
      (bb-opcode :sar 0x3b)

;; SLR  0x3c  111100 ttttt aaaaa bbbbb iiiiiiiiiii
      (bb-opcode :slr 0x3c)))


;--------------------------------------------------------------------------------
; Define a prototype assembler

(defn label? [form]
  (or (and (list? form) 
           (= (first form) 'label)
           (keyword? (second form)))
      (keyword? form)))


(defn label-symbol [form]
  (when (label? form)
    (cond (list? form)
            (second form)
          (keyword? form)
            form)))


(defn byte-count [form]
  4)


(defn compute-labels [start forms]
  (loop [label-addr-map {}
         address start
         forms forms]
    (let [head (first forms)]
      (if (label? head)
        (recur (assoc label-addr-map (label-symbol head) address) 
               address
               (rest forms))
        (if-not (empty? (rest forms))
          (recur label-addr-map
                 (+ address (byte-count head))
                 (rest forms))
          label-addr-map)))))


(defn resolve-param [label-map param]
  (if (label? param)
    (get label-map (label-symbol param))
    param))


(defn assemble-form [label-map form]
  (let [[op & params] form
        fun (get-in batbridge [:assemble (name op)])
        params (map (partial resolve-param label-map) params)]
    (apply fun params)))


(defn assemble
  "Compiles a series of assembler directives into a useable bytecode,
  computing relative jumps and absolute instruction positions naively.
  This is _not_ the final form of the assembler, only a prototype
  therefor which will be used to inform later syntax and design
  decisions."
  [forms & {:keys [start]
            :as options
            :or {start 0}}]
  (let [label-map (compute-labels start forms)]
    (->> forms
         (remove label?)
         (map (partial assemble-form label-map)))))
         
  
