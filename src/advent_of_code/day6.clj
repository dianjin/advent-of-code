(ns advent-of-code.day6
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.java.io :as io]
    )
  )

(defn file->lines
  []
  (let [input-file (io/file "input/day6")]
    (-> (slurp input-file)
      (str/split #"\n")
      )
    )
  )

(defn most-common-at-index
  [lines idx]
  (ffirst (sort-by #(second %) < (frequencies (map #(get % idx) lines))))
  )

(defn correct
  []
  (let [lines (file->lines)]
    (str/join
      (map
        (partial most-common-at-index lines)
        (range (count (first lines)))
        )
      )
    )
  )
