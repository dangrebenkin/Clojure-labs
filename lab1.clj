;;1.1-1.2---------------------

(defn concat-letters ([alphabet-letter combinations-list result-list]
                      (if (>= (count combinations-list) 1)
                        (let [first-combination (first combinations-list)
                              rest-combinations (rest combinations-list)
                              first-letter (first first-combination)]
                          (if (= (first alphabet-letter)  first-letter)
                            (recur alphabet-letter rest-combinations result-list)
                            (recur alphabet-letter rest-combinations (cons(str alphabet-letter first-combination) result-list))))
                        result-list)
                      )
                      ([alphabet-letter combinations-list] (concat-letters alphabet-letter combinations-list ())))

(defn making-permutations ([combinations alphabet results]
                           (if (>= (count alphabet) 1)
                             (let [letter (first alphabet)
                                   rest-alphabet (rest alphabet)
                                   new-results (concat-letters letter combinations)]
                               (recur combinations rest-alphabet (concat new-results results)))
                             results))
                          ([combinations alphabet] (making-permutations combinations alphabet ())))

(defn permutations [alphabet permutations-list k]
  (if (> k 0)
    (let [new-permutations(making-permutations permutations-list alphabet)]
      (recur alphabet new-permutations (- k 1)))
    permutations-list)
    )

(println (permutations '["a" "b" "c" "d"] '("") 4))


;;1.3---------------------

(defn my-map [func seq]
  (reduce (fn [result item]
            (conj result (func item)))
          []
          seq))

;(println (my-map dec [1 2 3 4]))

(defn check [number]
  (> number 0)
  )

(defn my-filter [func seq]
  (reduce (fn [result item]
            (if (func item)
              (conj result item)
              result)
            )
          []
          seq)
  )

(println (my-filter check '[3 -2 4 1 ]))

;;1.4---------------------

(defn concat-letters ([alphabet-letter combinations-list result-list]
                      (reduce (fn [result item]
                                (if (= (first alphabet-letter) (first item))
                                  result
                                  (conj result (str alphabet-letter item))
                                  )
                                )
                              result-list
                              combinations-list
                              )
                      )
                      ([alphabet-letter combinations-list] (concat-letters alphabet-letter combinations-list ())))
(defn making-permutations ([combinations alphabet results]
                           (reduce (fn [result letter]
                                     (concat result (concat-letters letter combinations)))
                                   results
                                   alphabet)
                           )
                          ([combinations alphabet] (making-permutations combinations alphabet ())))
(defn permutations ([alphabet permutations-list k]
                    (let [counter (range k)]
                      (reduce (fn [result _]
                                (making-permutations result alphabet)
                                )
                              permutations-list
                              counter
                              )
                      )
                    )
  )
(println (permutations '["a" "b" "c" "d"] '[""] 4))
