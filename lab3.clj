;3.1----------------------

(def step 0.0123)
(defn square [x] (Thread/sleep 1) (* x x))
(defn calc-last-step [x] (- x (* step (int (/ x step)))))

(def fi-sum-with-memoize
  (memoize (fn [i f]
             ;(println i)
             (if (zero? i)
               (/ (f 0) 2)
               (+ (f (* step i)) (fi-sum-with-memoize (dec i) f))))))

(defn integral [f]
  (fn [x]
    (let [steps-number (int (/ x step))
          last-step-val (calc-last-step x)]
      (+ (* step (fi-sum-with-memoize steps-number f)) (* last-step-val (/ (f x) 2)))
      )))

(def g2 (integral square))
(println (time (g2 2)))
(println (time (g2 3)))
;(println (time (g2 7)))
(println (time (g2 1.2524)))
(println (time (g2 2.2524)))

;;;3.2----------------------

(def step 0.0123)
(defn square [x] (Thread/sleep 1) (* x x))
(defn calc-last-step [x] (- x (* step (int (/ x step)))))

(defn infinite-integral
  ([f]
   (let [fi-sum-calc (fn [[previous-sum current-step]] [(+ previous-sum (f current-step)) (+ current-step step)])
         fi-sum-seq (iterate fi-sum-calc [(f 0) step])
         integrals-seq (fn [x]
                         (+ (* step (first (nth fi-sum-seq (int (/ x step)))))
                            (* step -1 (/ (f 0) 2) )   ; поправка на первую трапецию
                            (* (calc-last-step x) (/ (f x) 2) ))) ; последняя трапеция
         ]
     integrals-seq)))

(def g4 (infinite-integral square))

(println (time (g4 1)))
(println (time (g4 2)))
(println (time (g4 7)))
(println (time (g4 5)))
(println (time (g4 100)))
(println (time (g4 1.2524)))
(println (time (g4 2.2524)))
(println (time (g4 7.26524)))
(println (time (g4 4.2524)))