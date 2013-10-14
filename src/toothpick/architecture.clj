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


(defn parameter-field [sym width pred]
  {:name  sym
   :type  :field
   :width width
   :pred  pred})


(defn add-field [icode field-map]
  (let [field-map (assoc field-map :offset 
                         (->> icode
                              :fields
                              (map :width)
                              (reduce +)))]
    (-> icode
        (update-in [:fields] conj field-map)
        (update-in-only-when [:params] 
                             (fn [_] (= :field (:type field-map)))
                             conj (:name field-map)))))


(defn opcode [isa sym & fields]
  (let [opcode-repr (reduce add-field 
                            {} fields)]
    (assoc-in isa [:icodes (keyword (name sym))] opcode-repr)))


;; subsystem for using icode descriptors to assemble code
;;------------------------------------------------------------------------------
(defn encode-field 
  "Encodes an instruction field, using the field descriptor map and a
  map of parameter names to values."

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
        (when ((:pred field) val)
          (bit-shift-left 
           (bit-and (bit-mask-n (:width field))
                    val)
           (:offset field))))))


(defn map->bytecode 
  "Compiles a map which the disassembler would produce into a word."

  [isa opcode val-map]
  (let [icode (get (:icodes isa) opcode)
        fields (reverse (:fields icode))
        encoding (map #(encode-field icode %1 val-map)
                      fields)]
    (println (map #(format "0x%X" %1)
                  encoding))
    (reduce bit-or
            0 encoding)))


(defn list->bytecode
  "Compiles a list that a programmer could actually type or generate
  by hand into an assembled word."

  [isa opcode & tail]
  (let [icode (get (:icodes isa) 
                   opcode)
        fields (reverse (:fields icode))
        val-map (zipmap fields tail)]
    (map->bytecode isa opcode val-map)))
