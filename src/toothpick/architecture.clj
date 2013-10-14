(ns toothpick.architecture
  (:require [toothpick.core :refer [bit-fmt bit-mask-n]]))


(defn update-in-only-when [map path pred f & args]
  (if (pred map)
    (apply update-in map path f args)
    map))


(defmacro define-architecture [name & forms]
  `(def ~name (-> {} ~@forms)))

;; Idea: 
;;
;;    This is a new notation idea that totally struck me the night of
;;    10/5. The notation used below uses predicates (or a constant)
;;    and a number of bits to create an encoder/decoder pair for every
;;    opcode in the ISA as specified. However the generated code is
;;    not especially meaningful, and will almost certainly discard
;;    notation specifics such as field names because the original
;;    notation does not support it.
;;
;;    The solution to this problem, is to allow the user to detail the
;;    symbolic name as well as the type of each opcode field. One
;;    possible notation is
;;
;;    (opcode htl 32
;;            (const-field :icode 6  0x00)
;;            (const-field :dst   5  0x00)
;;            (const-field :srca  5  0x00)
;;            (const-field :srcb  5  0x00)
;;            (const-field :lit   11 0x00))
;;
;;    But that's an easy instruction, it has a constant encode and
;;    decode. How about another instruction with value ranges?
;;
;;    (opcode ld 32
;;            (const-field     :icode 6 0x10)
;;            (parameter-field :dst   5  register?)
;;            (parameter-field :srca  5  register?)
;;            (parameter-field :srcb  5  register?)
;;            (parameter-field :const 11 integer?))
;;
;;    The great value proposition of this approach is that it allows
;;    each instruction to have its own encode/decode pair which can
;;    round-trip symbolic meaning. So if there's one special case
;;    instruction which must have a bitfield set, or which supports
;;    some other single bit flag that no other icode does this
;;    notation naturally handles it because the decoder is generated
;;    on a per instruction basis and is "fully aware" of the user's
;;    desired notation.


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
