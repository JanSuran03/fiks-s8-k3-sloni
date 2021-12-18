(ns fiks-s8-k3-sloni.core
  (:require [clojure.string :as str]))

(defn read-and-process-input []
  (->> "input.txt" slurp
       str/split-lines
       rest
       (map #(as-> % input-line
                   (str/split input-line #"\ ")
                   (map read-string input-line)))))

(defn solve [[width height x y moves]]
  (println width height x y moves)
  (let [x-left (- x moves)
        x-right (+ x moves)
        y-bottom (- y moves)
        y-top (+ y moves)]
    [x-left
     x-right
     y-bottom
     y-top]))

(defn -main [& _args]
  (let [processed-input (read-and-process-input)]
    (solve (first processed-input))))