(ns toothpick.isa.c16
  (:require [toothpick.architecture :refer :all]))

(defn register? [x]
  (and (number? x)
       (integer? x)
       (<= 0 x 7)))

(defn imm5? [x]
  (let [v (dec (bit-shift-left 1 5))]
    (<= (- v) x v)))

(def r_ZERO 7)

(def c16
  (-> {}
      (opcode :add_rri
              (const-field :icode 0 4)
              (const-field :f     0 1)
              (parameter-field :d 3 register?)
              (parameter-field :a 3 register?)
              (signed-parameter-field :i 5 imm5?))

      (opcode :add_rrr
              (const-field :icode 0 4)
              (const-field :f     1 1)
              (parameter-field :d 3 register?)
              (parameter-field :a 3 register?)
              (const-field :_ 2 0)
              (parameter-field :b 3 register?))

      (opcode :sub_rri
              (const-field :icode 1 4)
              (const-field :f     0 1)
              (parameter-field :d 3 register?)
              (parameter-field :a 3 register?)
              (signed-parameter-field :i 5 imm5?))

      (opcode :sub_rrr
              (const-field :icode 1 4)
              (const-field :f     0 1)
              (parameter-field :d 3 register?)
              (parameter-field :a 3 register?)
              (const-field :_     0 2)
              (parameter-field :b 3 register?))

      (opcode :brz_rri
              (const-field :icode 2r1111 4)
              (const-field :f     0      1)
              (parameter-field :d 3 register?)
              (parameter-field :a 3 register?)
              (parameter-field :i 5 imm5?))

      (opcode :brz_ri
              (const-field :icode 2r1111 4)
              (const-field :f     1      1)
              (parameter-field :d 3 register?)
              (parameter-field :i 8 imm8?))

      (opcode :brnz_rri
              (const-field :icode 2r1110 4)
              (const-field :f     0      1)
              (parameter-field :d 3 register?)
              (parameter-field :a 3 register?)
              (parameter-field :i 5 imm5?))

      (opcode :brnz_ri
              (const-field :icode 2r1110 4)
              (const-field :f     1      1)
              (parameter-field :d 3 register?)
              (parameter-field :i 8 imm8?))))
