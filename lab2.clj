;;;2.1----------------------

(defn delete-multiples [prime increasing-prime bad-list]
  (if (<= increasing-prime (last bad-list))
    (recur prime (+ prime increasing-prime)(remove #{increasing-prime} bad-list))
    bad-list))

(defn sieve [list-num, index]
  (let [i (nth list-num index)
        h (+ i i)
        new-list (delete-multiples i h list-num)
        next-index (+ index 1)
        j (* (nth new-list next-index) (nth new-list next-index))]
    (if (<= j (last new-list))
      (recur new-list next-index)
      new-list)
    ))

; https://en.wikipedia.org/wiki/Prime_number_theorem

(defn create-sequence [n]
  (let [top-bound (Math/ceil (+ (* 1.5 n (Math/log n)) (Math/log (Math/log n)))) ; 1.5 log n + log (log n)
        num-seq (range 2 top-bound)]
       (println (nth (sieve num-seq 0) (- n 1))))
  )
(create-sequence 2000)

;;;;2.2----------------------

; проверка делимости с помощью перебора делителей (https://ru.wikipedia.org/wiki/%D0%9F%D0%B5%D1%80%D0%B5%D0%B1%D0%BE%D1%80_%D0%B4%D0%B5%D0%BB%D0%B8%D1%82%D0%B5%D0%BB%D0%B5%D0%B9)

(def inf-sieve
  (let [inf-seq (iterate inc 2)
        check-prime (fn[c] (not-any? #(zero? (rem c %)) (take-while #(<= % (int (Math/sqrt c))) inf-sieve)))]
    (filter check-prime inf-seq)))

(def n 10000)

(println (nth inf-sieve (- n 1)))