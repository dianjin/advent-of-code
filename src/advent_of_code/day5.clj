(ns advent-of-code.day5
  (:require
    [clojure.string :as str]
    [clojure.set :as set]
    [clojure.java.io :as io]
    [digest]
    )
  )

(def input-str "reyedfim")

(defn interesting?
  [hashed]
  (= "00000" (subs hashed 0 5))
  )

(defn find-interesting-recur
  [prefix idx interesting-list]
  (if (= 8 (count interesting-list))
    interesting-list
    (let [
      current-str (str prefix idx)
      hashed (digest/md5 current-str)
      idx-prime (inc idx)
      ]
      (if (interesting? hashed)
        (recur
          prefix
          idx-prime
          (str interesting-list (subs hashed 5 6))
          )
        (recur
          prefix
          idx-prime
          interesting-list
          )
        )
      )
    )
  )

(defn parse-number
  "Reads a number from a string. Returns nil if not a number."
  [s]
  (if (re-find #"^-?\d+\.?\d*$" s)
    (read-string s)))

(defn find-better-recur
  [prefix idx password]
  (if (every? #(not (nil? %)) password)
    (str/join "" password)
    (let [
      current-str (str prefix idx)
      hashed (digest/md5 current-str)
      idx-prime (inc idx)
      idx-to-insert (parse-number (subs hashed 5 6))
      str-to-insert (subs hashed 6 7)
      ]
      (if (and (interesting? hashed) idx-to-insert (<= idx-to-insert 7))
        (recur
          prefix
          idx-prime
          (update password idx-to-insert
            (fn [current-char]
              (if current-char current-char str-to-insert)
              )
            )
          )
        (recur
          prefix
          idx-prime
          password
          )
        )
      )
    )
  )

(defn find-password-better
  []
  (find-better-recur input-str 0 (vec (repeat 8 nil)))
  )

(defn find-password
  []
  (find-interesting-recur input-str 0 "")
  )
