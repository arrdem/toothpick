(ns toothpick.isa.batbridge-test
  (:require [toothpick.isa [batbridge :as bb]
                           [batbridge-util :refer :all]]
            [toothpick.assembler :as a]
            [clojure.test :refer :all]))


(deftest big-assembler-regression-test
  ;; This test attempts to round trip data through toothpick.assembler using the
  ;; Batbridge ISA var. Note that this tests both positive and negative constant
  ;; encoding, which requires additional assembler support.
  (let [icodes   [:add :sub :mul :div :mod :and :or :xor :sl :sr :sal :sar]
        dst-rng  (range 32)
        srca-rgn (range 32)
        srcb-rng (range 32)
        imm-rng  (range -1024 1024)]
    (doseq [icode  (take 5 (shuffle icodes))
            dst    (take 10 (shuffle dst-rng))
            srca   (take 10 (shuffle srca-rgn))
            srcb   (take 10 (shuffle srcb-rng))
            imm    (take 50 (shuffle imm-rng))]
      (let [m {:icode icode
               :a  srca
               :b  srcb
               :d   dst
               :i imm}]
        (is (= m
               (as-> m v
                     (a/map->bytecode bb/batbridge icode v)
                     (word->symbol-map v))))))))
    
             

