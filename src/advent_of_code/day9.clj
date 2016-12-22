(ns advent-of-code.day9
  (:require
    [clojure.string :as str]
    [clojure.pprint :as pp]
    [clojure.set :as set]
    [clojure.java.io :as io]
    )
  )

(def re-marker #"\((\d+)x(\d+)\)(.*)")

(defn marker->triplet
  [[a b c]]
  [(Integer/parseInt a) (Integer/parseInt b) c]
  )

(defn prefix->triplet
  [s]
  (some->> (re-find re-marker s)
    (drop 1)
    (marker->triplet)
    )
  )

(defn parse-text-recur-2
  [decompressed compressed]
  (if (empty? compressed)
    decompressed
    (if-let [triplet (prefix->triplet compressed)]
      (let [
        [num-chars num-reps rest-compressed] triplet
        tail-compressed (subs rest-compressed num-chars)
        repeat-str (subs rest-compressed 0 num-chars)
        ]
        (recur
          (+
            decompressed
            (* num-reps (parse-text-recur-2 0 repeat-str))
            )
          tail-compressed
          )
        )
      (recur
        (inc decompressed)
        (subs compressed 1)
        )
      )
    )
  )

(def decompressed-length-2
  (partial parse-text-recur-2 0)
  )

(defn parse-text-recur-1
  [decompressed compressed]
  (if (empty? compressed)
    decompressed
    (if-let [triplet (prefix->triplet compressed)]
      (let [
        [num-chars num-reps rest-compressed] triplet
        tail-compressed (subs rest-compressed num-chars)
        ]
        (recur
          (+ decompressed (* num-reps num-chars))
          tail-compressed
          )
        )
      (recur
        (inc decompressed)
        (subs compressed 1)
        )
      )
    )
  )

(def decompressed-length-1
  (partial parse-text-recur-1 0)
  )

(defn file->output
  []
  (let [input-file (io/file "input/day9")]
    (->> (slurp input-file)
      (str/trim)
      (#(str/replace % #"\s+" ""))
      (decompressed-length-2)
      )
    )
  )
