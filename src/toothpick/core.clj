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
                (apply vector ~s args#)))
       (assoc-in m [:opcodes s]
                 {:name sym
                  :code i
                  :fmt fmt
                  :docs (apply str docs)}))))

(declare assemble)

(defn assemble-op [isa state [op & params]]
  ;; FIXME
  ;;  This assemble-op is pretty targeted towards the DCPU16 and may not do
  ;;  too well when faced with building code for ARM and x86. The main weirdness
  ;;  with the DCPU16 is that instructions can be multi-word, and specific
  ;;  parameters can actually be a + next word meaning that one op can streach
  ;;  over three words :/
  ;;
  ;;  I deal with this by assuming that parameters are either standalone numbers
  ;;  or are a list of (register . consts) where consts are to be postfixed
  ;;  words. This behavior may have to change.

  ;; FIXME
  ;;  This assemble-op doesn't deal with labels as rvalues. rvalues probably
  ;;  need to become op-like prefix expressions :/
  (let [words (reduce (fn [wrds param]
                        (if (list? param)
                          (concat wrds (rest param))
                          wrds))
                      (list) params)
        params (map (fn [x]
                      (cond (list? x)
                              (first x)
                            (string? x)
                              (get-in isa [:registers x :code])
                            true
                               x))
                    params)]
    (println params)
    (concat (list (apply bit-fmt (get-in isa [:opcodes op :fmt])
                                 (get-in isa [:opcodes op :code])
                                 params))
            words)))

(defn assemble-form [[isa state words] form]
  (if (vector? form)
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
          [isa state (concat words (assemble-op isa state form))]))

      (let [[_ state new-words] (apply assemble isa state form)]
        [isa state (concat words new-words)])))

(defn assemble [isa state & forms]
  (reduce assemble-form
          [isa state (list)] forms))
