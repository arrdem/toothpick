(ns toothpick.isa.c16
  (:require [toothpick.architecture :refer :all]))

(defn register? [x]
  (and (number? x)
       (integer? x)
       (<= 0 x 7)))

(defn imm5? [x]
  (let [v (dec (bit-shift-left 1 5))]
    (<= (- v) x v)))

(defn imm8? [x]
  (let [v (dec (bit-shift-left 1 8))]
    (<= (- v) x v)))

(defmacro fmt-a [carry name op f]
  `(opcode ~carry ~name
           (->const-field     :icode 4 ~op)
           (->const-field     :f     1 ~f)
           (->parameter-field :d     3 register?)
           (->parameter-field :a     3 register?)
           (->const-field     :_     2 0)
           (->parameter-field :b     3 register?)
           ))

(defmacro fmt-b [carry name op f]
  `(opcode ~carry ~name
           (->const-field     :icode 4 ~op)
           (->const-field     :f     1 ~f)
           (->parameter-field :d     3 register?)
           (->parameter-field :a     3 register?)
           (->parameter-field :i     5 imm5?)
           ))

(defmacro fmt-c [carry name op f]
  `(opcode ~carry ~name
           (->const-field     :icode 4 ~op)
           (->const-field     :f     1 ~f)
           (->parameter-field :d     3 register?)
           (->parameter-field :i     8 imm8?)
           ))

(define-architecture c16
  (assoc :pcfn inc)  
  (fmt-b :add_rri  0  0)
  (fmt-a :add_rrr  0  1)
  (fmt-b :sub_rri  1  0)
  (fmt-a :sub_rrr  1  1)
  (fmt-b :brnz_rri 14 0)
  (fmt-c :brnz_ri  14 1)
  (fmt-b :brz_rri  15 0)
  (fmt-c :brz_ri   15 1)
  (fmt-b :ld_rri   2r1010 0)
  (fmt-c :ld_ri    2r1010 1))
