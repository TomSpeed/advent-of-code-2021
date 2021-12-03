(ns two.two
  (:require [clojure.string :as str])
  (:require [clojure.edn :as edn]))

(defn read-input []
  (slurp "src/two/input.txt"))

(defn get-input-string-vector []
  (-> (read-input)
      (str/split-lines)))

(defn split-command [command]
  (str/split command #" "))


(defn get-position-from-command [command]
  (let [name (first command) amount (edn/read-string (last command))]
    (case name
      "forward" {:horizontal-pos amount :depth 0}
      "down" {:horizontal-pos 0 :depth amount}
      "up" {:horizontal-pos 0 :depth (- 0 amount)})))

(defn get-position-and-aim-from-command [command aim]
  (let [name (first command) amount (edn/read-string (last command))]
    (case name
      "forward" {:horizontal-pos amount :depth (* amount aim) :aim 0}
      "down" {:horizontal-pos 0 :depth 0 :aim amount}
      "up" {:horizontal-pos 0 :depth 0 :aim (- 0 amount)})))

(defn merge-position [acc val]
  (let [command (split-command val)]
    (merge-with + acc (get-position-from-command command))))

(defn merge-position-and-aim [acc val]
  (let [command (split-command val)]
    (merge-with + acc (get-position-and-aim-from-command command (acc :aim)))))

(defn part-one [input]
  (let [parsed-input (reduce merge-position {:horizontal-pos 0 :depth 0} input)]
    (* (parsed-input :horizontal-pos) (parsed-input :depth))))

(defn part-two [input]
  (let [parsed-input (reduce merge-position-and-aim {:horizontal-pos 0 :depth 0 :aim 0} input)]
    (* (parsed-input :horizontal-pos) (parsed-input :depth))))

(defn main []
  (let [input (get-input-string-vector)]
    (println "Part one:" (part-one input))
    (println "Part two:" (part-two input))))

(main)