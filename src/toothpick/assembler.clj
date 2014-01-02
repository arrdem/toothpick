(ns toothpick.assembler
  (:require [toothpick.core :refer [bit-mask-n]]))


;; Subsystem for using ISA descriptors to assemble instruction descriptor
;; structures into bytes usable by a bytecode machine.
;; ------------------------------------------------------------------------------
(defn encode-field
  "Encodes an instruction field, using the field descriptor map and a map of
  parameter names to values. Returns an integer representing the encoded field."

  [icode field val-map]
  (case (:type field)
    ;; case for encoding a constant field... Note that this does _not_
    ;; make an effort to get a parameter from the parameters map,
    ;; because its a constant!
    (:const)
      (bit-shift-left
       (bit-and (bit-mask-n (:width field))
                (:value field))
       (:offset field))

    ;; Because this case is a parameter that has to be encoded in, a
    ;; parameter is fetched from the params map.
    (:field)
      (let [val (get val-map (:name field) 0)]
        (assert ((:pred field) val)
                (format "Failed to encode parameter %s" (name (:name field))))
        (bit-shift-left
         (bit-and (bit-mask-n (:width field))
                  val)
         (:offset field)))))


(defn map->bytecode
  "Compiles an instruction parameter map which the assembler would
  produce. Returns the integer encoding of the specified opcode with the given
  parameter values."

  [isa opcode val-map]
  (let [icode (get (:icodes isa) opcode)]
    (assert icode 
            (format "Could not get an icode for name %s" opcode))
    (let [fields (:fields icode)]
      (assert fields 
              (format "Could not get icode fields for icode %s"
                             (:name icode)))
      (let [encoding (mapv #(encode-field icode %1 val-map) fields)]
        (reduce bit-or 0 encoding)))))


(defn list->bytecode
  "Compiles a list that a programmer could actually type or generate
  into an assembled word."

  [isa [opcode & tail]]
  (let [opcode (get-in isa [:icodes opcode])]
    (assert opcode (format "Failed to find opcode in isa: %s" opcode))
    (let [{:keys [fields params] :as icode} 
          val-map (zipmap params tail)]
      (map->bytecode isa opcode val-map))))


;; Define a prototype assembler
;; from here on down is all totally broken and shouldn't be used. yet.
;;------------------------------------------------------------------------------
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


(defn assemble-form [isa label-map form]
  (let [[op & params] form
        fun (get-in isa [:assemble (name op)])
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
