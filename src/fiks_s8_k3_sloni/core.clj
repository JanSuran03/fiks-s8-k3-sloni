(ns fiks-s8-k3-sloni.core
  (:require [clojure.string :as str]))

(defn read-and-process-input []
  (->> "input.txt" slurp
       str/split-lines
       rest
       (map #(as-> % input-line
                   (str/split input-line #"\ ")
                   (map read-string input-line)))))

(defn pprint-array [[width height x y quad-triangle]]
  (let [s (set quad-triangle)]
    (newline)
    (doseq [row (reverse (range width))]
      (doseq [col (range height)]
        (printf "%3s" (cond (and (= x row)
                                 (= y col)) "X"
                            (contains? s [row col]) "O"
                            :else \u00b7)))
      (newline))))

(defmulti compute-intersection
          (fn [[width height center-x center-y [[x1 y1] [x2 y2] [x3 y3]]]]))

(defn rhombus-quarters-and-diagonals [x y moves]
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
    [[quad-1-triangle quad-2-triangle quad-3-triangle quad-4-triangle]
     [horizontal-line vertical-line]]))

(defn flip-along-x-axis [width height center-x center-y [[x1 y1] [x2 y2] [x3 y3]]]
  (let [[new-center-y new-y1 new-y2 new-y3] (map (fn [x]
                                                   (- height 1 x)) [center-y y1 y2 y3])]
    [width height center-x new-center-y [[x1 new-y1] [x2 new-y2] [x3 new-y3]]]))

(defn flip-along-y-axis [width height center-x center-y [[x1 y1] [x2 y2] [x3 y3]]]
  (let [[new-center-x new-x1 new-x2 new-x3] (map (fn [x]
                                                   (- width 1 x)) [center-x x1 x2 x3])]
    [width height new-center-x center-y [[new-x1 y1] [new-x2 y2] [new-x3 y3]]]))


(defn solve [[width height x y moves]]
  (println width height x y moves)
  (let [[[quad-1-triangle quad-2-triangle quad-3-triangle quad-4-triangle]
         [horizontal-line vertical-line]] (rhombus-quarters-and-diagonals x y moves)
        normalized-quad-1 [width height x y quad-1-triangle]
        normalized-quad-2-along-x (flip-along-y-axis width height x y quad-2-triangle)
        normalized-quad-3-along-x-y (->> [width height x y quad-3-triangle]
                                         (apply flip-along-x-axis)
                                         (apply flip-along-y-axis))
        normalized-quad-4-along-y (flip-along-x-axis width height x y quad-4-triangle)]
    (pprint-array normalized-quad-1)
    (pprint-array normalized-quad-4-along-y)
    (pprint-array normalized-quad-2-along-x)
    (pprint-array normalized-quad-3-along-x-y)
    (doseq [x [normalized-quad-1 normalized-quad-2-along-x normalized-quad-3-along-x-y normalized-quad-4-along-y]]
      (println x))))

(defn -main [& _args]
  (let [processed-input (read-and-process-input)]
    (solve (first processed-input))))