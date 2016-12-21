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
  {:orientation 0
   :x 0
   :y 0
  }
  )

(defn char->rotation
  [letter]
  (case letter
    \L -1
    \R 1
    )
  )

(defn token->direction
  [token]
  (let [
    x (first token)
    xs (Integer/parseInt (last (str/split token #"[A-Z]")))
    ]
    {:rotate (char->rotation x)
     :steps xs
    }
    )
  )

; 0 -> north
; 1 -> east
; 2 -> south
; 3 -> west
(defn apply-rotation
  [orientation rotate]
  (mod (+ orientation rotate) 4)
  )

(defn multiplier
  [orientation]
  (case orientation
    0 -1
    1 1
    2 1
    3 -1
    )
  )

(defn intermediate-position-keys
  [{:keys [orientation x y]} {:keys [rotate steps]}]
  (let [
    o-prime (apply-rotation orientation rotate)
    steps-factor (multiplier o-prime)
    x-prime (* steps-factor (if (even? o-prime) 0 1))
    y-prime (* steps-factor (if (even? o-prime) 1 0))
    intemediates (range 1 (inc steps))
    ]
    (reduce
      (fn [visited-set increment]
        (set/union
          visited-set
          #{(seq [
            (+ x (* increment x-prime))
            (+ y (* increment y-prime))
            ]
            )}
          )
        )
      #{}
      intemediates
      )
    )
  )

(defn next-position
  [{:keys [orientation x y]} {:keys [rotate steps]}]
  (let [
    o-prime (apply-rotation orientation rotate)
    steps-factor (multiplier o-prime)
    x-prime (* steps-factor (if (even? o-prime) 0 steps))
    y-prime (* steps-factor (if (even? o-prime) steps 0))
    ] {
      :orientation o-prime
      :x (+ x x-prime)
      :y (+ y y-prime)
    })
  )

(defn distance
  [final-pos]
  (let [{:keys [x y]} final-pos]
    (+ (Math/abs x) (Math/abs y))
    )
  )

(defn final-position
  []
  (reduce
    (fn [current-pos direction]
      (next-position current-pos direction)
      )
    initial-position
    (map token->direction (file->tokens))
    )
  )

(defn first-visited-twice
  [visited-set {:keys [x y] :as current-pos} directions]
  (let [
    direction (first directions)
    rest-directions (rest directions)
    {x-prime :x y-prime :y :as position-prime} (next-position current-pos direction)
    intermediate-positions (intermediate-position-keys current-pos direction)
    new-set-key (seq [x-prime y-prime])
    ]
    (if (some #(contains? visited-set %) intermediate-positions)
      (set/intersection visited-set intermediate-positions)
      (recur
        (set/union
          visited-set
          intermediate-positions
          #{new-set-key}
          )
        position-prime
        rest-directions
        )
      )
    )
  )

(defn find-visited-twice
  []
  (first-visited-twice
    #{}
    initial-position
    (map token->direction (file->tokens))
    )
  )
