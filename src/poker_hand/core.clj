(ns poker-hand.core
  (:gen-class))

(defn poker-hand 
  [c1 c2 c3 c4 c5]
  (vec [c1 c2 c3 c4 c5]))

(def hands
  {:pair 1
   :two-pairs 2
   :three-of-kind 3
   :straight 4
   :flush 5
   :full-house 6
   :four-of-kind 7
   :straight-flush 8
   :royal-flush 9})

(def rank-values
  (hash-map \T 10 \J 11 \Q 12 \K 13 \A 14))

(defn rank
  "returns rank of the card"
  [card]
  (let [[x _] card]
    (if (Character/isDigit x)
      (Integer/valueOf (str x))
      (get rank-values x))))

(defn suit
  "returns suit of the card"
  [card]
  (let [[_ y] card]
    (str y)))
   
(defn number-of-cards?
  [number poker-hand]
  (if (some #(>= % number) (vals (frequencies (map rank poker-hand))))
    true
    false))

(defn pair?
  [poker-hand]
  (number-of-cards? 2 poker-hand))

(defn three-of-kind?
  [poker-hand]
    (number-of-cards? 3 poker-hand))

(defn four-of-kind?
  [poker-hand]
    (number-of-cards? 4 poker-hand))
 
(defn flush?
  "the same suit, znaczek "
  [poker-hand]
  (apply = (map suit poker-hand)))

(defn straight?
  "5 sequential cards"
  [poker-hand]
  (let [ranks (map rank poker-hand)]
    (or
     (and
      (= 5 (count (set ranks)))
      (let [maximum (apply max ranks)
            minimum (apply min ranks)]
        (= (sort ranks) (range minimum (inc maximum)))))
     (= '(2 3 4 5 14) (sort ranks)))))

(defn straight-flush?
  "5 sequential cards in the same suit"
  [poker-hand]
  (and
   (straight? poker-hand)
   (flush? poker-hand)))

(defn royal-flush? 
  [poker-hand]
  (and
   (flush? poker-hand)
   (= '(10 11 12 13 14) (sort (map rank poker-hand)))))

(defn two-pairs?
  [poker-hand]
  (or
   (= (count (filter #(>= % 2) (vals (frequencies (map rank poker-hand))))) 2)
   (four-of-kind? poker-hand)))

(defn full-house?
 "para i trojka" 
  [poker-hand] 
   (= '(2 3) (sort (vals (frequencies (map rank poker-hand))))))

(defn check-poker-hand
  [poker-hand]
  (cond
    (royal-flush? poker-hand) (:royal-flush hands)
    (straight-flush? poker-hand) (:straight-flush hands)
    (four-of-kind? poker-hand) (:four-of-kind hands)
    (full-house? poker-hand) (:full-house hands)
    (flush? poker-hand) (:flush hands)
    (straight? poker-hand) (:straight hands)
    (three-of-kind? poker-hand) (:three-of-kind hands)
    (two-pairs? poker-hand) (:two-pairs hands)
    (pair? poker-hand) (:pair hands)
    :else 0))

(defn compare-poker-hands
  [ph1 ph2]
  (if (> (check-poker-hand ph1) (check-poker-hand ph2))
    1
    0)
  )

(def normal                   ["2H" "3S" "4C" "5C" "7D"])
(def pair                     ["2H" "2S" "4C" "5C" "7D"])
(def two-pairs                ["2H" "2S" "4C" "4D" "7D"])
(def three-of-a-kind          ["2H" "2S" "2C" "4D" "7D"])
(def four-of-a-kind           ["2H" "2S" "2C" "2D" "7D"])
(def straight                 ["2H" "3S" "6C" "5D" "4D"])
(def straight2                ["2H" "3S" "4C" "5D" "AD"])
(def straight3                ["TH" "AS" "QC" "KD" "JD"])
(def flush_                   ["2H" "4H" "5H" "9H" "7H"])
(def full-house               ["2H" "5D" "2D" "2C" "5S"])
(def straight-flush           ["2H" "3H" "6H" "5H" "4H"])
(def low-ace-straight-flush   ["2D" "3D" "4D" "5D" "AD"])
(def high-ace-straight-flus   ["TS" "AS" "QS" "KS" "JS"])