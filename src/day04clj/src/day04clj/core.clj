(ns day04clj.core
  (:require clojure.set clojure.string))

(defn parse-array [s]
  (as-> s it
    (clojure.string/split it #" ")
    (filter #(not (clojure.string/blank? %)) it)
    (map #(Integer/parseInt %) it)
    (set it)))

(defn parse-card [s]
  (let [[id-part numbers-part] (clojure.string/split s #": ")
        id (-> id-part
               (clojure.string/split #" ")
               (last)
               Integer/parseInt)
        [winning-numbers numbers-have] (map parse-array (clojure.string/split numbers-part #" \| "))]
    {:id id
     :winning-numbers winning-numbers
     :numbers-have numbers-have}))

(defn calc-matches [cards]
  (->> cards
       ; intersect the winning numbers with the numbers on the card
       (map #(clojure.set/intersection (:winning-numbers %) (:numbers-have %)))
       (map count)))

(defn part-one [matches]
  (->> matches
       ; find cards with any winning numbers 
       (filter #(> % 0))
       ; calculate the points (2 to the power of the number of winning numbers minus 1) 
       (map #(->> % dec (Math/pow 2) int))
       ; sum
       (reduce +)))

(defn part-two [all-cards all-matches]
  (loop [acc 0
         cards all-cards
         matches all-matches]
    (if (empty? cards)
      acc
      (let [; add the number of current card to the accumulator
            acc' (+ acc (first cards))
            ; add won cards to the seq
            match (first matches)
            n-card (first cards)
            cards' (->> cards
                        ; trim the current card
                        rest
                        ; add one card to each won card for each current card
                        (map-indexed
                         (fn [i c] (if (< i match)
                                     (+ c n-card)
                                     c))))
            ; trim the current card
            matches' (rest matches)]
        (recur acc' cards' matches')))))

(defn -main [& args]
  (let [cards (as-> args it
                (first it)
                (slurp it)
                (clojure.string/split-lines it)
                (map parse-card it))
        matches (calc-matches cards)
        ones (for [c cards] 1)]
    (println (part-one matches))
    (println (part-two ones matches))))
