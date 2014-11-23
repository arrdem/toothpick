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
    (:const :enforced-const)
      (bit-shift-left
       (bit-and (bit-mask-n (:width field))
                (:value field))
       (:offset field))

    ;; Because this case is a parameter that has to be encoded in, a
    ;; parameter is fetched from the params map.
    (:unsigned-field :field :signed-field)
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

  [isa [name & tail]]
  (let [opcode (get-in isa [:icodes name])]
    (assert opcode (format "Failed to find opcode in isa: %s" name))
    (let [{:keys [fields params] :as icode} opcode
          val-map (zipmap params tail)]
      (map->bytecode isa name val-map))))


;; Define a prototype assembler.
;;------------------------------------------------------------------------------
(defn label?

  [form]
  (or  (and (or (vector? form)
                (list? form))
            (= (first form) :label)
            (keyword? (second form))
            (= 2 (count form)))

       (and (or (vector? form)
                (list? form))
            (= (first form) :relative)
            (keyword? (second form))
            (= 2 (count form)))

       (keyword? form)))


(defn label-symbol

  [form]
  (when (label? form)
    (cond (list? form)
          (second form)
          (keyword? form)
            form)))


(defn byte-count [form]
  4)


(defn compute-labels

  [start forms]
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


(defn resolve-param
  "Assumes that all explicit parameters are integers and that keywords are
  labels. Looks up keywords in the argument translation table and returns either
  the translation or the existing parameter. Assertion fails if the keyword is
  not found."

  [label-map pc param]
  (if (label? param)
    (let [label (if (keyword? param)
                  param
                  (second param))]
      (assert (contains? label-map label)
              (format "Label %s undefined!" param))
      (if (= :relative (first param))
        (- (get label-map label) pc)
        (get label-map label)))
    param))


(defn resolve-params
  "Resolves the parameters of an opcode, being the _tail_ of the opcode. The
  head element is ignored and assumed to be an instruction keyword. Returns a
  new sequence representing the resolved opcode."

  [label-map pc icode]
  (let [op (first icode)
        more (rest icode)]
    (cons op (map #(resolve-param label-map pc %1) more))))


    (defn assemble
  "Compiles a series of assembler directives into a useable bytecode,
  computing relative jumps and absolute instruction positions naively.
  This is _not_ the final form of the assembler, only a prototype
  therefor which will be used to inform later syntax and design
  decisions."

  [isa forms & {:keys [start]
                :as   options
                :or   {start 0}}]
  (let [label-map (compute-labels start forms)]
    (as-> forms v
          (remove label? v)
          (map #(resolve-params label-map %2 %1) v (drop start (range)))
          (map #(list->bytecode isa %1) v))))


    (defn generate-mif [isa word-width mem-width forms]
      (let [bytecode (assemble isa forms)
            words    (/ mem-width word-width)
            segments (partition words words (repeat words 0) bytecode)
            pattern  (let [bytes    (/ word-width 4)
                           x-substr (str "%0" bytes "x")]
                       (str (apply str "%d:	" (repeat words x-substr)) ";\n"))
            template (str "DEPTH = %d;\n"
                          "WIDTH = %d;\n"
                          "ADDRESS_RADIX = DEC;\n"
                          "DATA_RADIX = HEX;\n"
                          "CONTENT\nBEGIN\n%sEND;")]
        (->> segments
             (map vector (range))
             (map (fn [[x args]]
                    (->> args
                         (map #(or %1 0))
                         (apply format pattern x))))
             (apply str)
             (format template
                     (inc (count segments))
                     mem-width))))
