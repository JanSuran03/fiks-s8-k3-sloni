(ns fiks-s8-k3-sloni.core
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as nt])
  (:refer-clojure :exclude [+ - * / inc dec quot mod even?]))

(defmacro defbigdecop [f]
  `(defn ~f [& args#]
     (apply ~(symbol (str "clojure.core/" f))
            (map bigdec args#))))

(defbigdecop +)
(defbigdecop -)
(defbigdecop *)
(defbigdecop /)
(defbigdecop inc)
(defbigdecop dec)
(defbigdecop quot)
(defbigdecop mod)
(defn even? [n]
  (= (mod n 2) 2))

(def evenness-complement {:even :odd
                          :odd  :even})

(defn even-or-odd-field [[start-x start-y] [target-x target-y] evenness]
  (if (even? (+ start-x start-y target-x target-y))
    evenness
    (evenness-complement evenness)))

(defn read-and-process-input []
  (->> "input-2.txt" slurp
       str/split-lines
       rest
       (map #(as-> % input-line
                   (str/split input-line #"\ ")
                   (map read-string input-line)))))

(defn odd-and-even
  "Based on the evenness in the starting corner and the number of squares on one
  side of the triangle, returns a hash-map of number of squares visitable exactly
  after the given number of moves and the other one, taking the starting corner
  evenness into consideration."
  [corner-evenness overhang]
  (let [corner-evenness-compl (evenness-complement corner-evenness)
        corner-evenness-fields (nt/expt (quot (inc overhang) 2) 2)
        overhang-half (quot overhang 2)
        ;; divided by 2, but multiplied by 2 right after
        not-corner-evenness-fields (* overhang-half (inc overhang-half))]
    {corner-evenness       corner-evenness-fields
     corner-evenness-compl not-corner-evenness-fields}))

(defmulti compute-intersection
          "Takes data of a rectangle and a normalized triangle so that the triangle should
           be in the first quadrant from the center and all the coordinates positive:
           y [x3 y3]
           y [x1 y1]    [x2 y2]
                x          x
           x2 > rest
           y3 > rest"
          (fn [corner-evenness [height width center-x center-y [[x1 y1] [max-x y2] [x3 max-y]] :as in]]
            (let [max-row (dec height)
                  max-col (dec width)
                  x-out-of-field (> max-x max-row)
                  y-out-of-field (> max-y max-col)
                  ret [x-out-of-field y-out-of-field]]
              ret)))

(defn merge-with-minus [& maps]
  (apply merge-with - maps))

(defmethod compute-intersection [false false]               ; the entire triangle is inside the rectangle
  [corner-evenness [height width center-x center-y [[x1 y1] [x2 y2] [x3 y3]]]]
  ;    x · · · ·    obviously the flat of the triangle here is (apply + (range 1 (+ 1 5)))
  ;    · · · · ·    => (apply + (range 1 (+ 1 n)))
  ;    · · · · ·    { Σ(1 -> k): k = n } == (n^2 + n)/2
  ;    · · · · ·    in Clojure: (/ (* n (inc n))
  ;    x · · · x                 2)
  (let [overhang (inc (- x2 x1))]
    (odd-and-even corner-evenness overhang)))

(defmethod compute-intersection [false true]                ; X-greatest vertex INside, Y-greatest OUTside
  [corner-evenness [height width center-x center-y [[x1 y1] [x2 y2] [x3 y3]]]]
  (let [inside-overhang (inc (- x2 x1))
        outside-overhang (- y3 (dec width))
        outside-y-evenness (even-or-odd-field [x1 y1] [x1 width] corner-evenness)
        minusY (odd-and-even outside-y-evenness outside-overhang)]
    (merge-with-minus (odd-and-even corner-evenness inside-overhang)
                      minusY)))

(defmethod compute-intersection [true false]                ; X-greatest vertex OUTside, Y-greatest OUTside
  [corner-evenness [height width center-x center-y [[x1 y1] [x2 y2] [x3 y3]]]]
  (let [inside-overhang (inc (- x2 x1))
        outside-overhang (- x2 (dec height))
        outside-x-evenness (even-or-odd-field [x1 y1] [height y1] corner-evenness)
        minusX (odd-and-even outside-x-evenness outside-overhang)]
    (merge-with-minus (odd-and-even corner-evenness inside-overhang)
                      minusX)))

(defmethod compute-intersection [true true]                 ; X-greatest vertex OUTside, Y-greatest OUTside
  [corner-evenness [height width center-x center-y [[x1 y1] [x2 y2] [x3 y3]] :as arr]]
  (let [inside-overhang (inc (- x2 x1))
        outside-x-overhang (- x2 (dec height))
        outside-y-overhang (- y3 (dec width))
        opposite-corner-evenness (evenness-complement corner-evenness)]
    (if (> (+ outside-x-overhang outside-y-overhang)
           inside-overhang)
      (let [total-half (/ (* (- height x1)
                             (- width y1))
                          2)]
        {corner-evenness          (nt/ceil (bigdec total-half))
         opposite-corner-evenness (nt/floor (bigdec total-half))})
      (let [outside-x-evenness (even-or-odd-field [x1 y1] [height y1] corner-evenness)
            outside-y-evenness (even-or-odd-field [x1 y1] [x1 width] corner-evenness)
            minusX (odd-and-even outside-x-evenness outside-x-overhang)
            minusY (odd-and-even outside-y-evenness outside-y-overhang)]
        (merge-with-minus (odd-and-even corner-evenness inside-overhang)
                          minusX
                          minusY)))))

(defn rhombus-quarters-and-diagonals [x y moves]
  (let [x-min (- x moves)
        x-max (+ x moves)
        y-min (- y moves)
        y-max (+ y moves)
        x-to-plus {:x-1 (inc x) :x-2 x-max :start :x-1}
        x-to-minus {:x-1 x-min :x-2 (dec x) :start :x-2}
        y-to-plus {:y-1 (inc y) :y-2 y-max :start :y-1}
        y-to-minus {:y-1 y-min :y-2 (dec y) :start :y-2}
        quad-1-triangle [[(inc x) (inc y)] [(dec x-max) (inc y)] [(inc x) (dec y-max)]]
        quad-2-triangle [[(dec x) (inc y)] [(dec x) (dec y-max)] [(inc x-min) (inc y)]]
        quad-3-triangle [[(dec x) (dec y)] [(inc x-min) (dec y)] [(dec x) (inc y-min)]]
        quad-4-triangle [[(inc x) (dec y)] [(inc x) (inc y-min)] [(dec x-max) (dec y)]]]
    [[quad-1-triangle quad-2-triangle quad-3-triangle quad-4-triangle]
     [x-to-plus x-to-minus y-to-plus y-to-minus]]))

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

(defn solve-line [{:keys [x-1 x-2 y-1 y-2 start]} height-or-width]
  (let [[start* end*] (if x-1 [x-1 x-2] [y-1 y-2])
        start* (if (neg? start*) 0 start*)
        end* (if (> end* (dec height-or-width)) (dec height-or-width) end*)
        len-half (/ (inc (- end* start*)) 2)]
    {:odd  (Math/ceil len-half)
     :even (Math/floor len-half)}))

(defn solve [[height width x y moves]]
  (let [[height width x y moves] (map bigdec [height width x y moves])
        [[quad-1-triangle quad-2-triangle quad-3-triangle quad-4-triangle]
         [x-to-plus x-to-minus y-to-plus y-to-minus]] (rhombus-quarters-and-diagonals x y moves)
        normalized-quad-1 [height width x y quad-1-triangle]
        normalized-quad-2-along-x (rotate+270 [height width x y quad-2-triangle])
        normalized-quad-3-along-x-y (rotate+180 [height width x y quad-3-triangle])
        normalized-quad-4-along-y (rotate+90 [height width x y quad-4-triangle])
        normalized-quadrants-triangles [normalized-quad-1 normalized-quad-2-along-x
                                        normalized-quad-3-along-x-y normalized-quad-4-along-y]
        x-to-plus (solve-line x-to-plus height)
        x-to-minus (solve-line x-to-minus height)
        y-to-plus (solve-line y-to-plus width)
        y-to-minus (solve-line y-to-minus width)
        diags-intersections (-> + (merge-with x-to-plus x-to-minus y-to-plus y-to-minus)
                                (update :even inc))
        possible-cells (->> normalized-quadrants-triangles (map #(vector :even %))
                            (map #(apply compute-intersection %))
                            (apply merge-with +)
                            (merge-with + diags-intersections))]
    (if (even? moves)
      (:even possible-cells)
      (:odd possible-cells))))

(defn -main [& args]
  (let [processed-input (read-and-process-input)]
    (->> processed-input
         ;first solve
         last solve vector
         ;(map solve)
         ;(str/join "\n")
         ;(spit "output.txt")
         )))