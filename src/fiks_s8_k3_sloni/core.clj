(ns fiks-s8-k3-sloni.core
  (:require [clojure.string :as str]))

(defn read-and-process-input []
  (->> "input.txt" slurp
       str/split-lines
       rest
       (map #(as-> % input-line
                   (str/split input-line #"\ ")
                   (map read-string input-line)))))

(defmulti analyze-intersection-part-1
          (fn [[x-left x-right] [y-bottom y-top] [width height]]
            [(> x-left 0)                                   ; :left-out?
             (< x-right width)                              ; :right-out?
             (> y-bottom 0)                                 ; :bottom-out?
             (< y-top height)]))                            ; :top-out?

(defmethod analyze-intersection-part-1 [true true true true]
  [& s]
  (println "pepeha"))

(defn rhombus-vertices-and-quarters [x y moves]
  (let [x-left (- x moves)
        x-right (+ x moves)
        y-bottom (- y moves)
        y-top (+ y moves)
        horizontal-line [x-left x-right]
        vertical-line [y-bottom y-top]
        quad-1-triangle [[(inc x) (inc y)] [(dec x-right) (inc y)] [(inc x) (dec y-top)]]
        quad-2-triangle [[(dec x) (inc y)] [(dec x) (dec y-top)] [(inc x-left) (inc y)]]
        quad-3-triangle [[(dec x) (dec y)] [(inc x-left) (dec y)] [(dec x) (inc y-bottom)]]
        quad-4-triangle [[(inc x) (dec y)] [(inc x) (inc y-bottom)] [(dec x-right) (dec y)]]]
    [[x-left x-right y-bottom y-top]
     [quad-1-triangle quad-2-triangle quad-3-triangle quad-4-triangle]]))

(defn solve [[width height x y moves]]
  (println width height x y moves)
  (let [{:keys [x-left x-right y-bottom y-top]} (rhombus-vertices-and-quarters x y moves)]
    (analyze-intersection-part-1 [x-left x-right] [y-bottom y-top] [width height])))

(defn -main [& _args]
  (let [processed-input (read-and-process-input)]
    (solve (first processed-input))))