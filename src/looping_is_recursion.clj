(ns looping-is-recursion)

(defn power [base exp]
  (let [go (fn [acc n]
             (if (zero? n)
               acc
               (recur (* acc base) (dec n))))]
    (if (zero? base)
      base
      (go 1 exp))))

(defn last-element [a-seq]
  (let [go (fn [lst]
             (let [fst (first lst)
                   rst (rest  lst)]
               (if (empty? rst)
                 fst
                 (recur rst))))]
    (if (empty? a-seq)
      nil
      (go a-seq))))

(defn seq= [seq1 seq2]
  (let [go (fn [aseq bseq]
             (let [a  (first aseq)
                   b  (first bseq)
                   as (rest aseq)
                   bs (rest bseq)]
               (cond
                (and (empty? as) (not (empty? bs))) false
                (and (empty? bs) (not (empty? as))) false
                :else (if (not= a b)
                        false
                        (if (and (empty? as) (empty? bs))
                          true
                          (recur as bs))))))]
    (if (and (empty? seq1) (empty? seq2))
      true
      (go seq1 seq2))))

(defn find-first-index [pred a-seq]
  (loop [acc a-seq
         n 0]
    (if (empty? acc)
      nil
      (if (pred (first acc))
        n
        (recur (rest acc) (inc n))))))

(defn avg [a-seq]
  (loop [acc 0
         lst a-seq
         n 0]
    (if (empty? lst)
      (/ acc n)
      (recur (+ (first lst) acc) (rest lst) (inc n)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [acc #{}
         lst a-seq]
    (if (empty? lst)
      acc
      (recur (toggle acc (first lst)) (rest lst)))))

(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])

