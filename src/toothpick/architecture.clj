(ns toothpick.architecture
  (:require [toothpick.core :refer [bit-fmt bit-mask-n]]
            [detritus.variants :refer [defvariant]]))

(defn update-in-only-when [map path pred f & args]
  (if (pred map)
    (apply update-in map path f args)
    map))

(defmacro define-architecture [name & forms]
  `(def ~name (-> {} ~@forms)))

;; subsystem for constructing icode descriptors
;;------------------------------------------------------------------------------
(defvariant const-field
  "Represents a constant field in a single opcode. While it is named via `sym',
  the value of this field cannot be set by a user and will be ignored if present
  in a values map."
  [sym width const])

(defvariant enforced-const-field
  "DEPRECATED, use const-field instead.

  Represents a constant field in a single opcode. While it is named via `sym', the
  value of this field cannot be set by a user and will be ignored if present in
  a values map."
  {:deprecated true}
  [sym width const])

(defvariant unsigned-param-field
  "Represents an unsigned parameter field in a single opcode. The `sym' of the
  parameter will be used to extract a value from the arguments map when bit
  encoding this parameter.

  The value must satisfy `pred' (which should check for truncation and soforth)
  in order for encoding to succeed."
  [sym width pred])

(defvariant signed-param-field
  "Represents a signed parameter field in a single opcode. The `sym' of the
  parameter will be used to extract a value from the arguments map when bit
  encoding this parameter.

  The value must satisfy `pred' (which should check for truncation and soforth)
  in order for encoding to succeed."
  [sym width pred])

(def n+
  "A wrapper around + which treats nil as having a value of 0."
  (fnil + 0))

(defn add-field 
  "Adds a bit field to an opcode. Bit fields are specified with a name, a type,
  a width, an optional test predicate and an optional value. Bit fields are
  packed atop previously installed fields."

  [icode field-map]
  (let [field-map (assoc field-map :offset 
                         (->> icode
                              :fields
                              (map :width)
                              (reduce +)))]
    (-> icode
        (update-in [:width]  n+ (:width field-map))
        (update-in [:fields] conj field-map)
        (update-in-only-when [:params] 
                             (fn [_] (#{:signed-field :unsigned-field :enforced-const}
                                     (:type field-map)))
                             conj (:name field-map)))))

(defn opcode
  "Creates an opcode representation by composing several parameter field
  specifiers onto a single map. These field specifiers, provided in the fields
  parameter, are ordered from highest bit to lowest. Sym is the symbolic name of
  the opcode being defined. Isa is the instruction set architecture into which
  the final compiled opcode representation is to be installed. Returns an
  updated ISA."

  [isa sym & fields]
  (let [fields (reverse fields)
        opcode-repr (reduce add-field {} fields)]
    (assoc-in isa [:icodes (keyword (name sym))] opcode-repr)))
