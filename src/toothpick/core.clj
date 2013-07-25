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
       (eval `(def ~sym ~s))
       (assoc-in m [:registers sym]
                 {:name sym
                  :code i
                  :width w})))
  ([m i sym] 
     (reg m i sym 32)))

(defn op
  ([m i s fmt & docs]
     (let [sym (symbol s)]
       (eval `(defn ~sym [& args#] (apply vector ~s args#)))
       (assoc-in m [:opcodes sym]
                 {:name sym
                  :code i
                  :fmt fmt
                  :docs (apply str docs)}))))
