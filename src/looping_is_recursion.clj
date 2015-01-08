(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [a-elem a-seq]
                 (if (empty? a-seq)
                   a-elem
                   (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [acc seq1 seq2]
                 (cond
                  (and (empty? seq1) (empty? seq2)) acc
                  (or (empty? seq1) (empty? seq2)) false
                  (== (first seq1) (first seq2)) (recur true (rest seq1) (rest seq2))
                  :else false))]
    (helper true seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         s a-seq]
    (cond
     (empty? s) nil
     (pred (first s)) acc
     :else (recur (inc acc) (rest s)))))

(defn avg [a-seq]
  (loop [acc 0
         count 0
         s a-seq]
    (if (empty? s)
      (/ acc count)
      (recur (+ acc (first s)) (inc count) (rest s)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (loop [acc-set #{}
           s a-seq]
      (if (empty? s)
        acc-set
        (recur (toggle acc-set (first s)) (rest s))))))

(defn fast-fibo [n]
  (loop [fn-1 0
         fn 0
         n2 0]
    (cond
     (== n2 n) (+ fn-1 fn)
     (<= n2 1) (recur 0 1 (inc n2))
     :else (recur fn (+ fn-1 fn) (inc n2)))))

(defn cut-at-repetition [a-seq]
  (loop [acc-seq []
         s a-seq]
    (cond
     (empty? s) acc-seq
     (contains? (set acc-seq) (first s)) acc-seq
     :else (recur (conj acc-seq (first s)) (rest s)))))

