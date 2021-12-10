(ns three.three
  (:require [clojure.string :as str]))

(defn read-input []
  (slurp "src/three/input.txt"))

(defn get-input-string-vector []
  (-> (read-input)
      (str/split-lines)))

(defn get-gamma-rate-binary [input]
  (into [] (map (fn [x] (first (map first (reverse (sort-by val x))))) (map frequencies (reduce (fn [x y] (mapv str x y)) ["" "" "" "" "" "" "" "" "" "" "" ""] input)))))

(defn get-epsilon-rate-binary [gamma-rate-binary]
  (mapv (fn [x] (if (= \1 x) \0 \1)) gamma-rate-binary))

(defn get-life-support-rating-binary [input current-digit prefered-bit]
  (if (= 1 (count input))
    (first input)
    (let [bits (mapv #(get % current-digit) input)
          number-of-occurences (sort-by val (merge-with + {\1 0 \0 0} (frequencies bits)))
          the-bit (if (apply = (map val number-of-occurences))
                    prefered-bit
                    (first (map first (case prefered-bit
                                        \1 (reverse number-of-occurences)
                                        \0 number-of-occurences))))
          filtered-input (filterv (fn [x] (= (get x current-digit) the-bit)) input)]
      (get-life-support-rating-binary
       filtered-input
       (+ 1 current-digit)
       prefered-bit))))

(defn parse-decimal-from-binary [binary]
  (Integer/parseInt (str/join binary) 2))

(defn part-one [input]
  (let [gamma-rate-binary (get-gamma-rate-binary input)
        gamma-rate-decimal (parse-decimal-from-binary gamma-rate-binary)
        epsilon-rate-binary (get-epsilon-rate-binary gamma-rate-binary)
        epsilon-rate-decimal (parse-decimal-from-binary epsilon-rate-binary)]
    (* gamma-rate-decimal epsilon-rate-decimal)))

(defn part-two [input]
  (let [oxygen-generator-rating-binary (get-life-support-rating-binary input 0 \1)
        oxygen-generator-rating-decimal (parse-decimal-from-binary oxygen-generator-rating-binary)
        c02-scrubber-rating-binary (get-life-support-rating-binary input 0 \0)
        c02-scrubber-rating-decimal (parse-decimal-from-binary c02-scrubber-rating-binary)]
    (* oxygen-generator-rating-decimal c02-scrubber-rating-decimal)))

(defn main []
  (let [input (get-input-string-vector)]
    (println "Part one:" (part-one input))
    (println "Part two:" (part-two input))))

(main)