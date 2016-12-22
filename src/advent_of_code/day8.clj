(ns advent-of-code.day8
  (:require
    [clojure.string :as str]
    [clojure.pprint :as pp]
    [clojure.set :as set]
    [clojure.java.io :as io]
    )
  )

(def re-rect #"rect (\d+)x(\d+)")
(def re-row #"rotate row y=(\d+) by (\d+)")
(def re-col #"rotate column x=(\d+) by (\d+)")
(def width 50)
(def height 6)
(def initial-screen (vec (repeat height (vec (repeat width nil)))))

(defn num-pair
  [re s]
  (some->> (re-find re s)
    (drop 1)
    (map #(Integer/parseInt %))
    )
  )

(defn product
  [x-list y-list]
  (reduce
    (fn [current-pairs x-coord]
      (concat
        current-pairs
        (map #(vec [x-coord %]) y-list)
        )
      )
    []
    x-list
    )
  )

(defn add-rect
  [[x y] screen]
  (reduce
    (fn [current-screen path]
      (assoc-in current-screen path 1)
      )
    screen
    (product (range y) (range x))
    )
  )

(defn transpose
  [screen]
  (mapv
    (fn [y-coord]
      (mapv #(get-in screen [% y-coord]) (range (count screen)))
      )
    (range (count (first screen)))
    )
  )

(defn shift-line
  [bound [y-coord units] screen]
  (let [initial-row (screen y-coord)]
    (reduce
      (fn [current-screen [idx e]]
        (let [new-x (mod (+ units idx) bound)]
          (assoc-in current-screen [y-coord new-x] e)
          )
        )
      screen
      (map-indexed (fn [idx e] [idx e]) initial-row)
      )
    )
  )

(defn line->updater
  [s]
  (if-let [pair (num-pair re-rect s)]
    (partial add-rect pair)
    (if-let [pair (num-pair re-row s)]
      (partial shift-line width pair)
      (if-let [pair (num-pair re-col s)]
        #(transpose (shift-line height pair (transpose %)))
        )
      )
    )
  )

(def test-instrs
  (map parse-line [
    "rect 3x2"
    "rotate column x=1 by 1"
    "rotate row y=0 by 4"
    "rotate column x=1 by 1"
    ])
  )

(defn updaters->output
  [updaters]
  (reduce
    (fn [current-screen updater]
      (updater current-screen)
      )
    initial-screen
    updaters
    )
  )

(defn write-output!
  [screen]
  (spit
    "output/day8"
    (str/join "\n"
      (map
        (fn [line]
          (apply str
            (map #(if (nil? %) "_" "8") line)
            )
          )
        screen
        )
      )
    )
  )

(defn file->output
  []
  (let [input-file (io/file "input/day8")]
    (->> (slurp input-file)
      (#(str/split % #"\n"))
      (map line->updater)
      (updaters->output)
      (write-output!)
      ; (apply concat)
      ; (filter identity)
      ; (count)
      )
    )
  )
