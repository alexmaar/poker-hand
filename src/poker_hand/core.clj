(ns poker-hand.core
  (:gen-class))

(defn poker-hand 
  [c1 c2 c3 c4 c5]
  (vec [c1 c2 c3 c4 c5]))

(def hand-type
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
  "returns true if there is given number of cards of the same rank"
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
  "the same suit"
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
 "pair and three-of-cards" 
  [poker-hand] 
   (= '(2 3) (sort (vals (frequencies (map rank poker-hand))))))

(defn high-card?
  [poker-hand]
  (apply max (map rank poker-hand)))

(defn check-poker-hand
  [poker-hand]
  (cond
    (royal-flush? poker-hand) (:royal-flush hand-type)
    (straight-flush? poker-hand) (:straight-flush hand-type)
    (four-of-kind? poker-hand) (:four-of-kind hand-type)
    (full-house? poker-hand) (:full-house hand-type)
    (flush? poker-hand) (:flush hand-type)
    (straight? poker-hand) (:straight hand-type)
    (three-of-kind? poker-hand) (:three-of-kind hand-type)
    (two-pairs? poker-hand) (:two-pairs hand-type)
    (pair? poker-hand) (:pair hand-type)
    :else (high-card? poker-hand)))

;; (defn compare-poker-hands
;;   [ph1 ph2]
;;   (if (> (check-poker-hand ph1) (check-poker-hand ph2))
;;     1
;;     0)
;;   )

(defn ties?
  [ph1 ph2]
  (let [ranks1 (rank ph1)
        ranks2 (rank ph2)
        sum1 (apply + ranks1)
        sum2 (apply + ranks2)]
    (if (> sum1 sum2)
      1
      0)  
    ))

(defn compare-poker-hands
  [ph1 ph2]
  (let [type1 (check-poker-hand ph1)
        type2 (check-poker-hand ph2)]
    (cond
      (> type1 type2) :win 
      (< type1 type2) :loss
      (= type1 type2) (if 
                       (or 
                        (= (:pair hand-type) type)
                        (= (:three-of-kind hand-type) type)
                        (= (:four-of-kind hand-type) type)
                        (= (:straight hand-type) type)
                        (= (:straight-flush hand-type) type))
                        (ties? ph1 ph2)
                        (cond
                          (> (high-card? ph1) (high-card? ph2)) :win
                          (< (high-card? ph1) (high-card? ph2)) :loss
                          :else :ties)))))



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