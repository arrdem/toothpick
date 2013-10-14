(ns toothpick.assembler)

;--------------------------------------------------------------------------------
; Define a prototype assembler

(defn label? [form]
  (or (and (list? form) 
           (= (first form) 'label)
           (keyword? (second form)))
      (keyword? form)))


(defn label-symbol [form]
  (when (label? form)
    (cond (list? form)
            (second form)
          (keyword? form)
            form)))


(defn byte-count [form]
  4)


(defn compute-labels [start forms]
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


(defn resolve-param [label-map param]
  (if (label? param)
    (get label-map (label-symbol param))
    param))


(defn assemble-form [isa label-map form]
  (let [[op & params] form
        fun (get-in isa[:assemble (name op)])
        params (map (partial resolve-param label-map) params)]
    (apply fun params)))


(defn assemble
  "Compiles a series of assembler directives into a useable bytecode,
  computing relative jumps and absolute instruction positions naively.
  This is _not_ the final form of the assembler, only a prototype
  therefor which will be used to inform later syntax and design
  decisions."
  [forms & {:keys [start]
            :as options
            :or {start 0}}]
  (let [label-map (compute-labels start forms)]
    (->> forms
         (remove label?)
         (map (partial assemble-form label-map)))))
