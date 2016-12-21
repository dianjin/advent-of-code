(ns advent-of-code.day4
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.java.io :as io]
    )
  )

(defn get-top-recur
  [stop-at current-seq {:keys [freq list]} [current-top & rest-top-chars]]
  (cond
    (>= (count current-seq) stop-at)
    current-seq
    (not current-top)
    (concat current-seq (sort list))
    :otherwise
    (let [
      [this-char this-freq] current-top
      ]
      (if (< this-freq freq)
        ; Put all the current list in the current-seq
        (recur
          stop-at
          (concat current-seq (sort list))
          {:freq this-freq :list [this-char]}
          rest-top-chars
          )
        ; Put the current char in the list
        (recur
          stop-at
          current-seq
          {:freq freq :list (cons this-char list)}
          rest-top-chars
          )
        )
      )
    )
  )

(defn get-top-string
  [stop-at top-chars]
  (subs
    (str/join
      (get-top-recur stop-at '() {:freq (second (first top-chars)) :list []} top-chars)
      )
    0
    5
    )
  )

(defn shift-word
  [shift word]
  (str/join
    (map
      (fn [c]
        (let [char-int (- (int c) 97)]
          (char (+ 97 (mod (+ char-int shift) 26)))
          )
        )
      word
      )
    )
  )

(defn parse-room
  [stop-at room-str]
  (let [
    len (count room-str)
    split-idx (str/index-of room-str \[)
    name-str (subs room-str 0 split-idx)
    name-without-dashes (str/replace name-str #"-" "")
    name-str (first (str/split name-str #"\d+"))
    name (first (str/split name-without-dashes #"\d+"))
    sector (Integer/parseInt (last (str/split name-without-dashes #"[a-z]+")))
    checksum (subs room-str (inc split-idx) (dec len))
    counts (frequencies name)
    sorted-counts (sort-by (fn [[c count]] count) > counts)
    shift (mod sector 26)
    words (map (partial shift-word shift) (str/split name-str #"-"))
    ] {
      :top-chars (get-top-string stop-at sorted-counts)
      :checksum checksum
      :sector sector
      :name (str/join " " words)
    })
  )

(defn file->rooms
  []
  (let [input-file (io/file "input/day4")]
    (-> (slurp input-file)
      (str/split #"\n")
      (#(map (partial parse-room 5) %))
      )
    )
  )

(defn sum-valid-rooms
  [rooms]
  (reduce
    (fn [current-sum {:keys [top-chars checksum sector name]}]
      (println name sector)
      (if (= top-chars checksum)
        (+ current-sum sector)
        current-sum
        )
      )
    0
    rooms
    )
  )

(defn write-room-names!
  [rooms]
  (spit
    "output/day4"
    (str/join "\n"
      (map
        (fn [{:keys [name sector]}]
          (str "[" sector "] " name)
          )
        rooms
        )
      )
    )
  )
