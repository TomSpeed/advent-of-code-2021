(ns one.one
  (:require [clojure.string :as str])
  (:require [clojure.edn :as edn]))

(defn read-input []
  (slurp "src/one/input.txt"))

(defn get-input-string-vector []
  (-> (read-input)
      (str/split-lines)))

(defn get-input-int-vector []
  (mapv edn/read-string (get-input-string-vector)))

(defn part-one [input]
  (let [x (subvec input 1)]
    (count (filter #(> % 0) (mapv - x input)))))

(defn part-two [input]
  (part-one (into [] (map #(apply + %) (partition 3 1 input)))))

(defn main []
  (let [input (get-input-int-vector)]
    (println "Part one:" (part-one input))
    (println "Part two:" (part-two input))))

(main)