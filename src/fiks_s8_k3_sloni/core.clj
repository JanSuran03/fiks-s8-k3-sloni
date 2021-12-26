(ns fiks-s8-k3-sloni.core
  (:require [clojure.string :as str]))

(defn read-and-process-input []
  (->> "input.txt" slurp
       str/split-lines
       rest
       (map #(as-> % input-line
                   (str/split input-line #"\ ")
                   (map read-string input-line)))))

(defn print-array [[width height x y quad-triangle]]
  #_(let [s (set quad-triangle)]
      (newline)
      (doseq [row (reverse (range width))]
        (doseq [col (range height)]
          (printf "%3s" (cond (and (= x row)
                                   (= y col)) "X"
                              (contains? s [row col]) "O"
                              :else \u00b7)))
        (newline)))
  (let [s (set quad-triangle)]
    (newline)
    (doseq [row (range height)]
      (doseq [col (range width)]
        (printf "%3s" (cond (and (= x row)
                                 (= y col)) "X"
                            (contains? s [row col]) "O"
                            :else \u00b7)))
      (newline))))

(defmulti compute-intersection
          ^{:doc "Takes data of a rectangle and a normalized triangle so
                  that all the coordinates are always positive:
                  [[x1 y1] [_x2 _y2] [_x3 _y3]]
                  x1 < x2 > x2
                  y1 = y2 < y3"}
          (fn [[width height _center-x _center-y [[x1 y1] [x2 y2] [x3 y3]]]]
            (println (dec width) (dec height) _center-x _center-y [[x1 y1] [x2 y2] [x3 y3]])
            (let [x-out-of-field (> x2 (dec height))
                  y-out-of-field (> y3 (dec width))
                  ret [x-out-of-field y-out-of-field]]
              (println ret)
              ret)))

(defmethod compute-intersection [false false]
  [[width height center-x center-y [[x1 y1] [x2 y2] [x3 y3]]]]
  (let [leg-1 (inc (- x2 x1))
        leg-2 (inc (- x3 x1))]
    (* leg-1 leg-2)))

(defmethod compute-intersection [false true]
  [[width height center-x center-y [[x1 y1] [x2 y2] [x3 y3]]]])

(defmethod compute-intersection [true false]
  [[width height center-x center-y [[x1 y1] [x2 y2] [x3 y3]]]])

(defmethod compute-intersection [true true]
  [[width height center-x center-y [[x1 y1] [x2 y2] [x3 y3]]]])

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
  (let [[new-center-y new-y1 new-y2 new-y3] (map (fn [y]
                                                   (- (dec width) y)) [center-y y1 y2 y3])]
    [width height center-x new-center-y [[x1 new-y1] [x2 new-y2] [x3 new-y3]]]))

(defn flip-along-y-axis [width height center-x center-y [[x1 y1] [x2 y2] [x3 y3]]]
  (let [[new-center-x new-x1 new-x2 new-x3] (map (fn [x]
                                                   (- (dec height) x)) [center-x x1 x2 x3])]
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
        normalized-quad-4-along-y (flip-along-x-axis width height x y quad-4-triangle)
        normalized-quadrants-triangles [normalized-quad-1 normalized-quad-4-along-y
                                        normalized-quad-2-along-x normalized-quad-3-along-x-y]]
    (doseq [arr #_normalized-quadrants-triangles
            (map #(vector width height x y %) [quad-1-triangle quad-2-triangle
                                               quad-3-triangle quad-4-triangle])]
      (print-array arr))
    (doseq [x [quad-1-triangle quad-2-triangle
               quad-3-triangle quad-4-triangle]]
      (prn x))
    (doseq [x normalized-quadrants-triangles]
      (print-array x))))

(defn -main [& _args]
  (let [processed-input (read-and-process-input)]
    (solve (first processed-input))))