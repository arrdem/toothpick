(ns toothpick.dcpu16
  (:require [toothpick.core :refer :all]))

;; define architectural registers

(define-register "A"  0x00 32)
(define-register "B"  0x01 32)
(define-register "C"  0x02 32)
(define-register "X"  0x03 32)
(define-register "Y"  0x04 32)
(define-register "Z"  0x05 32)
(define-register "I"  0x06 32)
(define-register "J"  0x07 32)
(define-register "SB" 0x1b 32)
(define-register "PC" 0x1c 32)
(define-register "EX" 0x1d 32)

(defn BRACKET [register]
  (assert (number? register))
  (bit-or register 0x08))

(defn DEREF
  ([register]
     (assert (number? register))
     (bit-or register 0x8))

  ([register const]
     (assert (number? register))
     (list (bit-or register 0x18)
           const)))

(defn- op [icode b a]
  (bit-fmt [6 5 5] a b icode))

(def SET (partial op 1))
