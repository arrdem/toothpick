(ns toothpick.core
  (:import (java.io File FileOutputStream)
           (java.nio ByteBuffer ByteOrder)))

(defn exp [b n]
  (reduce * 1 (repeat n b)))

(defn bit-mask-n [n]
  (- (exp 2 n) 1))

(defn bit-fmt
  "Takes a seq format parameter, followed by an equal number of format values
   with extra format values being ignored. Produces a bit vector by anding the
   n+1th argument expression value with a bitmask for m bits where m is the nth
   value in the first parameter.
   Eg. [30 2] 0 1 -> 0x1
       [30 2] 0 2 -> 0x2
       [30 2] 0 3 -> 0x3
       [1 31] 1 0 -> 0x80000000"
  [layout & args]
  (reduce (fn [vec [bits value]]
            (bit-or (bit-shift-left vec bits)
                    (bit-and value (bit-mask-n bits))))
          0 (map vector layout args)))

(defmacro define-architecture [name & forms]
  `(def ~name (-> {} ~@forms)))

(defn reg
  ([m i s w]
     (let [sym (symbol s)]
       (eval `(def ~sym {:type :register
                         :key :registers
                         :value ~s}))
       (assoc-in m [:registers s]
                 {:name sym
                  :code i
                  :width w})))
  ([m i sym]
     (reg m i sym 32)))

(defn op
  ([m i s fmt & docs]
     (let [sym (symbol s)]
       (eval `(defn ~sym [& args#]
                {:type :opcode
                 :key :opcodes
                 :value ~s
                 :params args#}))
       (assoc-in m [:opcodes s]
                 {:name sym
                  :code i
                  :fmt fmt
                  :docs (apply str docs)}))))


(defn assemble-op [isa state [op & params]]
  ;; FIXME
  ;;  This assemble-op doesn't deal with labels as rvalues.
  ;;  Implement a special case where if an rvalue is a fn, it will be evaluated
  ;;  with the ISA and the state.This allows for labels and some other crud to
  ;; be implemented as partials and evaled as needed, rather than doing ATO expa
  (let [words (reduce (fn [wrds param]
                        (if (vector? param)
                          (concat wrds (rest param))
                          wrds))
                      (list) params)
        params (map (fn [x]
                      (cond (vector? x)
                              (first x)
                            (string? x)
                              (get-in isa [:registers x :code])
                            (number? x)
                              x))
                    params)]
    (-> bit-fmt
        (apply (get-in isa [:opcodes op :fmt])
               (get-in isa [:opcodes op :code])
               params)
        (list)
        (concat words))))

(defn assemble-form [[isa state words] form]
  (let [[op & args] form]
    (case op
      (LABEL label)
      (let [[_label id] args]
        [isa (assoc-in state [:labels id] (count words)) words])

      ;; FIXME
      ;;  needs to support an ALIGN directive

      ;; FIXME
      ;;  needs to support a PAD or SPACE directive

      ;; FIXME
      ;;  needs to support a WORD directive for literal values

      ;; else case
      [isa state (concat words (assemble-op isa state form))])))

(defn assemble [isa state & forms]
  (reduce assemble-form
          [isa state (list)] forms))
