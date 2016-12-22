(ns advent-of-code.day10
  (:require
    [clojure.string :as str]
    [clojure.pprint :as pp]
    [clojure.set :as set]
    [clojure.java.io :as io]
    )
  )

(def re-assign #"value (\d+) goes to bot (\d+)")
(defn parse-assign
  [instr]
  (some->> (re-find re-assign instr)
    (drop 1)
    (#(merge
      {:value (Integer/parseInt (first %))}
      {:recipient [:bot (Integer/parseInt (second %))]}
      ))
    )
  )

(def re-recipient #"bot (\d+)|output (\d+)")
(defn parse-recipient
  [recipient]
  (->> (re-find re-recipient recipient)
    (drop 1)
    (#(if-let [bot (first %)]
      [:bot (Integer/parseInt bot)]
      [:output (Integer/parseInt (second %))]
      ))
    )
  )

(def re-give #"bot (\d+) gives low to (.*) and high to (.*)")
(defn parse-give
  [instr]
  (some->> (re-find re-give instr)
    (drop 1)
    (#(merge
      {:giver [:bot (Integer/parseInt (first %))]}
      {:lo-recipient (parse-recipient (second %))}
      {:hi-recipient (parse-recipient (last %))}
      ))
    )
  )

(defn assign-updater
  [{:keys [value recipient]}]
  #(update-in % recipient (partial set/union #{value}))
  )

(defn give-updater
  [{:keys [giver lo-recipient hi-recipient]}]
  (fn [current-map]
    (let [
      giver-set (get-in current-map giver)
      min-val (apply min giver-set)
      max-val (apply max giver-set)
      ]
      (-> current-map
        ; Giver has zero set
        (assoc-in giver #{})
        ; lo-recipient has minimum
        (update-in lo-recipient (partial set/union #{min-val}))
        ; hi-recipient has maximum
        (update-in hi-recipient (partial set/union #{max-val}))
        )
      )
    )
  )

(defn instructions->updaters
  [instructions]
  (reduce
    (fn [[bot-map updater-map] instr]
      (if-let [instr-map (parse-assign instr)]
        [((assign-updater instr-map) bot-map)
         updater-map]
        (let [{:keys [giver] :as instr-map} (parse-give instr)]
          [bot-map
           (assoc updater-map (second giver) (give-updater instr-map))]
          )
        )
      )
    [{} {}]
    instructions
    )
  )

(defn has-2-vals?
  [[id val-set]]
  (= 2 (count val-set))
  )

(defn give-values-recur
  [updater-map bot-map compare-sets]
  (let [bots-with-2 (filter has-2-vals? (:bot bot-map))]
    (if (empty? bots-with-2)
      compare-sets
      (recur
        updater-map
        (reduce
          (fn [m f] (f m))
          bot-map
          (map (fn [[id _]] (get updater-map id)) bots-with-2)
          )
        (set/union compare-sets (set bots-with-2))
        )
      )
    )
  )

(defn who-compared
  [set compare-sets]
  (some (fn [[id two-set]] (if (= set two-set) id)) compare-sets)
  )

(defn file->output
  []
  (let [input-file (io/file "input/day10")]
    (->> (slurp input-file)
      (#(str/split % #"\n"))
      (instructions->updaters)
      (#(give-values-recur (second %) (first %) #{}))
      (who-compared #{61 17})
      )
    )
  )
