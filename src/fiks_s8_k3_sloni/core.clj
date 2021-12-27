(ns fiks-s8-k3-sloni.core
  (:require [clojure.string :as str]))

(def dot-in-the-middle \u00b7)

(defn read-and-process-input []
  (->> "input.txt" slurp
       str/split-lines
       rest
       (map #(as-> % input-line
                   (str/split input-line #"\ ")
                   (map read-string input-line)))))

(defn print-array [[height width x y quad-triangle]]
  (let [s (set quad-triangle)]
    (newline)
    (doseq [row (range height)]
      (doseq [col (range width)]
        (printf "%3s" (cond (and (= x row)
                                 (= y col)) "X"
                            (contains? s [row col]) "O"
                            :else dot-in-the-middle)))
      (newline))))

(defmulti compute-intersection
          "Takes data of a rectangle and a normalized triangle so that the triangle should
           be in the first quadrant from the center and all the coordinates positive:
           y [x3 y3]

           y [x1 y1]    [x2 y2]
                x          x

           x2 > rest
           y3 > rest"
          (fn [[height width _center-x _center-y [[x1 y1] [x2 y2] [x3 y3]] :as in]]
            ;(println (str "max-x: " (dec width) " (x2 = " x2 "), max-y: " (dec height) " (y3 = " y3 ")"))
            ;(println (str "center-x: " _center-x ", center-y: " _center-y ", vertices: " [[x1 y1] [x2 y2] [x3 y3]]))
            (print-array in)
            (println [x1 y1] [x2 y2] [x3 y3])
            (let [x-out-of-field (>= x2 (dec width))
                  y-out-of-field (>= y3 (dec height))
                  ret [x-out-of-field y-out-of-field]]
              (println (str x2 " >? " (dec width) ", " y3 " >? " (dec height)))
              (println ret)
              ret)))

(defmethod compute-intersection [false false]               ; the entire triangle is inside the rectangle
  [[width height center-x center-y [[x1 _y1] [x2 _y2] [_x3 _y3]] :as in]]
  ;    x · · · ·    obviously the flat here is (apply + (range 1 (+ 1 5)))
  ;    · · · · ·    (apply + (range 1 (+ 1 n)))
  ;    · · · · ·    { Σ(1 -> k): k = n } == (n^2 + n)/2
  ;    · · · · ·    in Clojure: (/ (* n (inc n))
  ;    x · · · x                 2)
  (let [leg-x-y (inc (- x2 x1))]
    (/ (* leg-x-y (inc leg-x-y))
       2)))

(defmethod compute-intersection [false true]
  [[width height center-x center-y [[x1 y1] [x2 y2] [x3 y3]]]])

(defmethod compute-intersection [true false]
  [[width height center-x center-y [[x1 y1] [x2 y2] [x3 y3]]]])

(defmethod compute-intersection [true true]                 ; both of the hypotenuse's vertices end outside the rectangle
  [[width height center-x center-y [[x1 y1] [x2 y2] [x3 y3]]]])

#_(defn compute-intersection [[width height _center-x _center-y [[x1 y1] [x2 y2] [x3 y3]]]]
    (let [x-out-of-field (>= x2 (dec width))
          y-out-of-field (>= y3 (dec height))
          ret [x-out-of-field y-out-of-field]]
      (println (str x2 " >? " (dec width) ", " y3 " >? " (dec height)))
      (println ret)
      (newline)
      (dorun (map #(if % 1 false) ret))))
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

(defn flip-along-x-axis [height width center-x center-y [[x1 y1] [x2 y2] [x3 y3]]]
  (let [[new-center-y new-y1 new-y2 new-y3] (map (fn [y]
                                                   (- (dec width) y)) [center-y y1 y2 y3])]
    [height width center-x new-center-y [[x1 new-y1] [x2 new-y2] [x3 new-y3]]]))

(defn flip-along-y-axis [height width center-x center-y [[x1 y1] [x2 y2] [x3 y3]]]
  (let [[new-center-x new-x1 new-x2 new-x3] (map (fn [x]
                                                   (- (dec height) x)) [center-x x1 x2 x3])]
    [height width new-center-x center-y [[new-x1 y1] [new-x2 y2] [new-x3 y3]]]))

(defn rotate [[height width center-x center-y [[x1 y1] [x2 y2] [x3 y3]]]]
  (let [[new-center-x new-x1 new-x2 new-x3] (map (fn [y]
                                                   (- width y 1))
                                                 [center-y y1 y2 y3])
        [new-center-y new-y1 new-y2 new-y3] [center-x x1 x2 x3]]
    [width height new-center-x new-center-y [[new-x1 new-y1] [new-x2 new-y2] [new-x3 new-y3]]]))

(defn solve [[height width x y moves]]
  (println height width x y moves)
  (let [[[quad-1-triangle quad-2-triangle quad-3-triangle quad-4-triangle]
         [horizontal-line vertical-line]] (rhombus-quarters-and-diagonals x y moves)
        normalized-quad-1 [height width x y quad-1-triangle]
        normalized-quad-2-along-x (flip-along-y-axis height width x y quad-2-triangle)
        normalized-quad-3-along-x-y (->> [height width x y quad-3-triangle]
                                         (apply flip-along-x-axis)
                                         (apply flip-along-y-axis))
        normalized-quad-4-along-y (flip-along-x-axis height width x y quad-4-triangle)
        normalized-quadrants-triangles [normalized-quad-1 normalized-quad-2-along-x
                                        normalized-quad-3-along-x-y normalized-quad-4-along-y]]
    #_(doseq [arr #_normalized-quadrants-triangles
              (map #(vector width height x y %) [quad-1-triangle quad-2-triangle
                                                 quad-3-triangle quad-4-triangle])]
        (print-array arr))
    #_(doseq [x [quad-1-triangle quad-2-triangle
                 quad-3-triangle quad-4-triangle]]
        (prn x))
    #_(doseq [x normalized-quadrants-triangles]
        (print-array x))
    #_(map compute-intersection normalized-quadrants-triangles)
    (println normalized-quad-1)
    (print-array normalized-quad-1)
    (-> normalized-quad-1 rotate print-array)
    (-> normalized-quad-1 rotate rotate print-array)
    (-> normalized-quad-1 rotate rotate rotate print-array)))

(defn -main [& _args]
  (let [processed-input (read-and-process-input)]
    (solve (first processed-input))))