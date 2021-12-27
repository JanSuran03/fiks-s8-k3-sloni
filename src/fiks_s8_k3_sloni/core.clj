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
          (fn [[height width _center-x _center-y [[x1 y1] [max-x y2] [x3 max-y]]]]
            (let [max-row (dec height)
                  max-col (dec width)
                  x-out-of-field (> max-x max-row)
                  y-out-of-field (> max-y max-col)
                  ret [x-out-of-field y-out-of-field]]
              ;(println [x1 y1] [max-x y2] [x3 max-y])
              ;(println (str "max-x: " max-row " (x2 = " max-x "), max-y: " max-col " (y3 = " max-y ")"))
              ;(println (str "center-x: " _center-x ", center-y: " _center-y ", vertices: " [[x1 y1] [max-x y2] [x3 max-y]]))
              ;(newline)
              ;(println (str max-x " >? " max-row ", " max-y " >? " max-col))
              ;(println ret)
              ret)))

(comment
  (let [data [12 10 5 6 [[6 7] [11 7] [6 12]]]]
    (print-array data)))

(defmethod compute-intersection [false false]               ; the entire triangle is inside the rectangle
  [[height width center-x center-y [[x1 _y1] [x2 _y2] [_x3 _y3]]]]
  ;    x · · · ·    obviously the flat of the triangle here is (apply + (range 1 (+ 1 5)))
  ;    · · · · ·    => (apply + (range 1 (+ 1 n)))
  ;    · · · · ·    { Σ(1 -> k): k = n } == (n^2 + n)/2
  ;    · · · · ·    in Clojure: (/ (* n (inc n))
  ;    x · · · x                 2)
  (let [leg-x-y (inc (- x2 x1))]
    (/ (* leg-x-y (inc leg-x-y))
       2)))

(defmethod compute-intersection [false true]                ; X-greatest vertex INside, Y-greatest OUTside
  [[height width center-x center-y [[x1 y1] [x2 y2] [x3 y3]]]]
  (println width y3))

(defmethod compute-intersection [true false]                ; X-greatest vertex OUTside, Y-greatest OUTside
  [[height width center-x center-y [[x1 y1] [x2 y2] [x3 y3]]]])

(defmethod compute-intersection [true true]                 ; X-greatest vertex OUTside, Y-greatest OUTside
  [[height width center-x center-y [[x1 y1] [x2 y2] [x3 y3]]]])

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

(defn rotate+90 [[height width center-x center-y [[x1 y1] [x2 y2] [x3 y3]]]]
  (let [[new-center-x new-x1 new-x2 new-x3] (map #(- width % 1) [center-y y1 y2 y3])
        [new-center-y new-y1 new-y2 new-y3] [center-x x1 x2 x3]]
    [width height new-center-x new-center-y [[new-x1 new-y1] [new-x2 new-y2] [new-x3 new-y3]]]))

(defn rotate+180 [[height width center-x center-y [[x1 y1] [x2 y2] [x3 y3]]]]
  (let [[new-center-x new-x1 new-x2 new-x3] (map #(- height % 1) [center-x x1 x2 x3])
        [new-center-y new-y1 new-y2 new-y3] (map #(- width % 1) [center-y y1 y2 y3])]
    [height width new-center-x new-center-y [[new-x1 new-y1] [new-x2 new-y2] [new-x3 new-y3]]]))

(defn rotate+270 [[height width center-x center-y [[x1 y1] [x2 y2] [x3 y3]]]]
  (let [[new-center-x new-x1 new-x2 new-x3] [center-y y1 y2 y3]
        [new-center-y new-y1 new-y2 new-y3] (map #(- height % 1) [center-x x1 x2 x3])]
    [width height new-center-x new-center-y [[new-x1 new-y1] [new-x2 new-y2] [new-x3 new-y3]]]))

(defn solve [[height width x y moves]]
  (println height width x y moves)
  (let [[[quad-1-triangle quad-2-triangle quad-3-triangle quad-4-triangle]
         [horizontal-line vertical-line]] (rhombus-quarters-and-diagonals x y moves)
        normalized-quad-1 [height width x y quad-1-triangle]
        normalized-quad-2-along-x (rotate+270 [height width x y quad-2-triangle])
        normalized-quad-3-along-x-y (rotate+180 [height width x y quad-3-triangle])
        normalized-quad-4-along-y (rotate+90 [height width x y quad-4-triangle])
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
    (map compute-intersection normalized-quadrants-triangles)))

(defn -main [& _args]
  (let [processed-input (read-and-process-input)]
    (solve (first processed-input))))