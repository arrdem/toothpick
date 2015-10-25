(ns toothpick.architecture
  (:require [toothpick.core :refer [bit-fmt bit-mask-n]]
            [guten-tag.core :refer [deftag]]
            [detritus.update :refer [take-when]]))

(defn update-in-only-when [map path pred f & args]
  (if (pred map)
    (apply update-in map path f args)
    map))

(defmacro define-architecture [name
                               word-size
                               pointer-interval
                               & forms]
  {:pre [(integer? word-size)
         (integer? pointer-interval)]}
  `(def ~name (-> {:icodes           {}
                   :word-size        ~word-size         ;; units of bits
                   :pointer-interval ~pointer-interval} ;; units of words
                  ~@forms)))

(defn instr-size [isa [op & tail :as opcode]]
  (let [pointer-interval (:pointer-interval isa)
        word-size        (:word-size isa)
        instr            (get-in isa [:icodes op])
        instr-width      (:width instr)]
    (/ instr-width word-size pointer-interval)))

;; subsystem for constructing icode descriptors
;;------------------------------------------------------------------------------
(deftag const-field
  "Represents a constant field in a single opcode. While it is named via `sym',
  the value of this field cannot be set by a user and will be ignored if present
  in a values map."
  [name width const]
  {:pre [(keyword? name)
         (integer? width)
         (number? const)]})

(deftag enforced-const-field
  "DEPRECATED, use const-field instead.

  Represents a constant field in a single opcode. While it is named via `name', the
  value of this field cannot be set by a user and will be ignored if present in
  a values map."
  {:deprecated true}
  [name width const]
  {:pre [(keyword? name)
         (integer? width)
         (number? const)]})

(deftag unsigned-param-field
  "Represents an unsigned parameter field in a single opcode. The `name' of the
  parameter will be used to extract a value from the arguments map when bit
  encoding this parameter.

  The value must satisfy `pred' (which should check for truncation and soforth)
  in order for encoding to succeed."
  [name width pred]
  {:pre [(keyword? name)
         (integer? width)
         (instance? clojure.lang.IFn pred)]})

(deftag signed-param-field
  "Represents a signed parameter field in a single opcode. The `name' of the
  parameter will be used to extract a value from the arguments map when bit
  encoding this parameter.

  The value must satisfy `pred' (which should check for truncation and soforth)
  in order for encoding to succeed."
  [name width pred]
  {:pre [(keyword? name)
         (integer? width)
         (instance? clojure.lang.IFn pred)]})

(defn icode-field?
  "Predicate indicating whether the given value is any kind of icode field
  tagged value."
  [x]
  (or (signed-param-field? x)
      (unsigned-param-field? x)
      (const-field? x)
      (enforced-const-field? x)))

;; FIXME: naming is le hard
(defn field-param?
  "Predicate indicating whether the given value is a tagged value representing
  an icode field which requires a user supplied parameter."
  [x]
  (or (unsigned-param-field? x)
      (signed-param-field? x)))

(def n+
  "A wrapper around + which treats nil as having a value of 0."
  (fnil + 0))

(defn add-field 
  "Adds a bit field to an opcode. Bit fields are specified with a name, a type,
  a width, an optional test predicate and an optional value. Bit fields are
  packed atop previously installed fields."

  [icode field]
  {:pre [(icode-field? field)]}
  (let [field-offset (:width icode 0)
        field-width  (:width field)
        field        (assoc field :offset field-offset)]
    (-> icode
        (update-in [:width]  n+ field-width)
        (update-in [:fields] conj field)
        (cond-> (field-param? field)
          (update-in [:params] conj (:name field))))))

(defn opcode
  "Creates an opcode representation by composing several parameter field
  specifiers onto a single map. These field specifiers, provided in the fields
  parameter, are ordered from highest bit to lowest. Sym is the symbolic name of
  the opcode being defined. Isa is the instruction set architecture into which
  the final compiled opcode representation is to be installed. Returns an
  updated ISA."

  [isa sym & fields]
  (let [fields      (reverse fields)
        opcode-repr (reduce add-field {} fields)]
    (assoc-in isa [:icodes (keyword (name sym))] opcode-repr)))
