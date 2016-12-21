(ns advent-of-code.day3
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.java.io :as io]
    )
  )

(defn file->sides
  []
  (let [input-file (io/file "input/day3")]
    (-> (slurp input-file)
      (str/split #"\s+")
      (#(map (fn [e] (Integer/parseInt e)) %))
      (#(partition 3 %))
      )
    )
  )

(defn mapper
  [list]
  (map
    #(map
      (fn [str]
        (map
          (fn [e] (Integer/parseInt e))
          (str/split (str/trim str) #"\s+")
          )
        )
      %
      )
    list
    )
  )

; (566 477 376 5)
; (575 488 365 5)
; (50 18 156 5)

; (566 575 50)
; (477 488 18)
; (376 365 156)
; (5 5 5)
(defn group-columns
  [matrix]
  (reduce
    (fn [acc row]
      (reduce
        (fn [current-acc [idx elem]]
          (update current-acc idx #(conj % elem))
          )
        acc
        (concat (map-indexed (fn [idx elem] [idx elem]) row))
        )
      )
    (vec (repeat (count (first matrix)) []))
    matrix
    )
  )

(defn file->sides-by-column
  []
  (let [input-file (io/file "input/day3")]
    (-> (slurp input-file)
      (str/split #"\n")
      (#(partition 3 %))
      (mapper)
      (#(map group-columns %))
      (#(apply concat %))
      )
    )
  )

; 101 301 501
; 102 302 502
; 103 303 503
; 201 401 601
; 202 402 602
; 203 403 603
(defn partition-by-max-recur
  [current-max smaller [current-elem & rest-elems]]
  (if (not current-elem)
    [current-max smaller]
    (if (> current-elem current-max)
      (recur current-elem (cons current-max smaller) rest-elems)
      (recur current-max (cons current-elem smaller) rest-elems)
      )
    )
  )

(defn partition-by-max
  [[first-elem & rest-elems]]
  (partition-by-max-recur first-elem '() rest-elems)
  )

(defn count-valid-triangles
  [side-lengths]
  (reduce
    (fn [num-valid triplet]
      (let [
        [max smaller] (partition-by-max triplet)
        sum-smaller (apply + smaller)
        valid? (> sum-smaller max)
        ]
        (if valid?
          (inc num-valid)
          num-valid
          )
        )
      )
    0
    side-lengths
    )
  )
