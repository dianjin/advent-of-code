(ns advent-of-code.day7
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.java.io :as io]
    )
  )

(def re-bracket #"\[[a-z]+\]")

(defn drop-first-last
  [s]
  (subs s 1 (dec (count s)))
  )

(defn all-brackets
  [s]
  (map
    drop-first-last
    (re-seq re-bracket s)
    )
  )

(defn is-abba?
  [[a b c d]]
  (if (= a d)
    (and (= b c) (not (= a b)))
    false
    )
  )

(defn has-abba
  [s]
  (cond
    (< (count s) 4) false
    (= (count s) 4) (is-abba? s)
    :otherwise (some is-abba? (partition 4 1 s))
    )
  )

(defn valid-brackets?
  "cannot have abbas"
  [brackets]
  (every? #(not (has-abba %)) brackets)
  )

(defn valid-rest?
  "must have one abba"
  [rest]
  (some has-abba rest)
  )

(defn valid-ip?
  [s]
  (and
    (valid-brackets? (all-brackets s))
    (valid-rest? (str/split s re-bracket))
    )
  )

(defn is-aba?
  [[a b c]]
  (and (= a c) (not (= a b)))
  )

(defn find-abas
  [input-strs]
  (reduce
    (fn [current-abas current-str]
      (concat
        current-abas
        (filter is-aba? (partition 3 1 current-str))
        )
      )
    '()
    input-strs
    )
  )

(defn aba->bab
  [[a b c]]
  [b a b]
  )

(defn valid-ip-ssl?
  [s]
  (let [
    rest-strs (str/split s re-bracket)
    brackets (all-brackets s)
    all-supernet-abas (set (find-abas rest-strs))
    all-hypernet-babs (set (find-abas brackets))
    corresponding-babs (set (map aba->bab all-supernet-abas))
    intersecting (set/intersection corresponding-babs all-hypernet-babs)
    ]
    (not (empty? intersecting))
    )
  )

(defn file->lines
  []
  (let [input-file (io/file "input/day7")]
    (-> (slurp input-file)
      (str/split #"\n")
      (#(map valid-ip-ssl? %))
      (#(filter identity %))
      (count)
      )
    )
  )
