(ns advent-of-code.day1
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.java.io :as io]
    )
  )

(defn file->tokens
  []
  (let [input-file (io/file "input/day1")]
    (-> (slurp input-file)
      (str/split #",")
      (#(map str/trim %))
      )
    )
  )

(def initial-position
  {:orientation 0 :x 0 :y 0}
  )

(def char->rotation
  {\L -1 \R 1}
  )

(defn token->direction
  [[direction-char & steps]]
  (cons
    (char->rotation direction-char)
    (repeat (dec (Integer/parseInt (str/join steps))) 0)
    )
  )

(defn apply-rotation
  [rotate orientation]
  (mod (+ orientation rotate) 4)
  )

(def multiplier {
  0 {:dx 0 :dy -1} ; north
  1 {:dx 1 :dy 0} ; east
  2 {:dx 0 :dy 1} ; south
  3 {:dx -1 :dy 0} ; west
  })

(defn update-position
  [rotate {:keys [orientation x y]}]
  (let [
    o-prime (apply-rotation rotate orientation)
    {:keys [dx dy]} (multiplier o-prime)
    ] {
    :orientation o-prime
    :x (+ x dx)
    :y (+ y dy)
    })
  )

(defn distance
  [{:keys [x y]}]
  (+ (Math/abs x) (Math/abs y))
  )

(defn final-position
  [directions]
  (reduce
    (fn [current-position rotate]
      (update-position rotate current-position)
      )
    initial-position
    directions
    )
  )

(defn final-distance
  []
  (distance
    (final-position
      (apply concat (map token->direction (file->tokens)))
      )
    )
  )

(defn first-visited-twice-recur
  [visited-set current-pos [current-direction & rest-directions]]
  (let [
    {:keys [x y] :as next-pos} (update-position current-direction current-pos)
    ]
    (if (contains? visited-set [x y])
      (distance next-pos)
      (recur
        (set/union visited-set #{[x y]})
        next-pos
        rest-directions
        )
      )
    )
  )

(defn first-visited-twice
  []
  (first-visited-twice-recur
    #{[0 0]}
    initial-position
    (apply concat (map token->direction (file->tokens)))
    )
  )
