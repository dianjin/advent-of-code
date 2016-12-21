(ns advent-of-code.day2
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.java.io :as io]
    )
  )

(defn file->chars
  []
  (let [input-file (io/file "input/day2")]
    (str/split (slurp input-file) #"\n")
    )
  )

; [[1 2 3] [4 5 6] [7 8 9]]
(def plain-num-pad
  (vec (map vec (partition 3 (range 1 10))))
  )

(def plain-num-pad-pair
  [plain-num-pad [1 1]]
  )

;    1
;  2 3 4
;5 6 7 8 9
;  A B C
;    D
(def fancy-num-pad {
  0 {2 1}
  1 {1 2 2 3 3 4}
  2 {0 5 1 6 2 7 3 8 4 9}
  3 {1 \A 2 \B 3 \C}
  4 {2 \D}
  })

(def fancy-num-pad-pair
  [fancy-num-pad [2 0]]
  )

(defn path-differential
  [letter]
  (case letter
    \R [0 1]
    \L [0 -1]
    \U [-1 0]
    \D [1 0]
    )
  )

(defn new-path
  [[x y] letter]
  (let [[dx dy] (path-differential letter)]
    [(+ x dx) (+ y dy)]
    )
  )

(defn button-recur
  [num-pad current-path last-button [instruction & rest-instructions]]
  (if (not instruction)
    {:button last-button :path current-path}
    (let [
      path-prime (new-path current-path instruction)
      next-button (get-in num-pad path-prime)
      next-path (if next-button path-prime current-path)
      button-prime (if next-button next-button last-button)
      ]
      (recur
        num-pad
        next-path
        button-prime
        rest-instructions
        )
      )
    )
  )

(defn final-button-recur
  [num-pad current-path button-list [line & rest-lines]]
  (if (not line)
    button-list
    (let [
      {:keys [button path]} (button-recur num-pad current-path (get-in num-pad current-path) line)
      ]
      (recur
        num-pad
        path
        (conj button-list button)
        rest-lines
        )
      )
    )
  )

(defn final-button
  [[num-pad initial-path] lines]
  (final-button-recur num-pad initial-path [] lines)
  )
