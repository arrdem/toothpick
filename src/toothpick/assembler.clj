(ns toothpick.assembler
  (:require [clojure.core.match :refer [match]]
            [toothpick.core :refer [bit-mask-n]]
            [toothpick.architecture :refer [icode-field?] :as a]))

;; Subsystem for using ISA descriptors to assemble instruction descriptor
;; structures into bytes usable by a bytecode machine.
;; ------------------------------------------------------------------------------
(defn encode-field
  "Encodes an instruction field, using the field descriptor map and a map of
  parameter names to values. Returns an integer representing the encoded field."

  [icode [tag {:keys [width offset const name pred] :as body} :as field] val-map]
  {:pre [(icode-field? field)
         (integer? width)
         (integer? offset)
         (> width 0)
         (or (number? const)
             (and name pred))]}
  (bit-shift-left
   (bit-and (bit-mask-n width)
            (case tag
              (::a/const-field)
              ,,const

              (::a/unsigned-param-field ::a/signed-param-field)
              ,,(let [val (get val-map name)]
                  (assert val        (str "No value for parameter " name))
                  (assert (pred val) (str "Predicate failure on parameter " name))
                  val)))
   offset))

(defn map->bytecode
  "Compiles an instruction parameter map which the assembler would
  produce. Returns the integer encoding of the specified opcode with the given
  parameter values."

  [isa opcode val-map]
  (let [icode  (get (:icodes isa) opcode)
        fields (:fields icode)]
    (assert icode (format "Could not get an icode for name " opcode))
    (assert fields (str "Could not get icode fields for icode " opcode))
    (let [encoding (mapv #(encode-field icode %1 val-map) fields)]
      (reduce bit-or 0 encoding))))

(defn list->bytecode
  "Compiles a list that a programmer could actually type or generate
  into an assembled word."

  [isa [name & tail]]
  (let [opcode (get-in isa [:icodes name])]
    (assert opcode (format "Failed to find opcode in isa: %s" name))
    (let [{:keys [fields params] :as icode} opcode
          val-map                           (zipmap params tail)]
      (assert (= (count params) (count tail))
              (str "Could not encode op, " name ", missmatched param counts!"))
      (map->bytecode isa name val-map))))


;; Define a prototype assembler.
;;------------------------------------------------------------------------------
(defn label?
  "Predicate indicating whether an opcode represents a label. Returns the
  label's name or false."
  [form]
  (match [form]
         [[:label x]] x
         :else false))

;; FIXME: Not a constant! Depends on the ISA
(defn byte-count [isa form]
  4)

(defn compute-labels
  "Walks a seq of forms given an ISA and a starting address, computing the
  location of labels in the given program and returning a mapping from labels to
  their addresses in the program."

  [isa start forms]
  (loop [label-addr-map   {}
         address          start
         [head :as forms] forms]
    (if-let [target (label? head)]
      (recur (assoc label-addr-map target address)
             address
             (rest forms))
      (if-not (empty? (rest forms))
        (recur label-addr-map
               (+ address (byte-count isa head))
               (rest forms))
        label-addr-map))))

;; FIXME: do I need the ISA here?
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

;; FIXME: do I need the ISA here?
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
  (let [label-map (compute-labels isa start forms)]
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
                   (str (apply str "%d: " (repeat words x-substr)) ";\n"))
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
