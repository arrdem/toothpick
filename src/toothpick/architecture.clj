(ns toothpick.architecture
  (:require [toothpick.core :refer [bit-fmt bit-mask-n]]))

(defn update-in-only-when [map path pred f & args]
  (if (pred map)
    (apply update-in map path f args)
    map))

(defmacro define-architecture [name & forms]
  `(def ~name (-> {} ~@forms)))

;; subsystem for constructing icode descriptors
;;------------------------------------------------------------------------------
(defn const-field [sym width const]
  {:name  sym
   :type  :const
   :width width
   :value const})

(defn enforced-const-field [sym width const]
  {:name  sym
   :type  :enforced-const
   :width width
   :value const})

(defn parameter-field [sym width pred]
  {:name  sym
   :type  :unsigned-field
   :width width
   :pred  pred})


(defn signed-parameter-field [sym width pred]
  {:name  sym
   :type  :signed-field
   :width width
   :pred  pred})

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
