(ns prime.core)

(defn is-prime [n]
  (cond
    (< n 2) false
    (= n 2) true
    (zero? (mod n 2)) false
    :else
    (loop [i 3]
      (cond
        (>= (* i i) n) true
        (zero? (mod n i)) false
        :else (recur (+ i 2))))))

(defn -main [& args]
  (let [number (Integer/parseInt (first args))]
    (println (is-prime number))))
