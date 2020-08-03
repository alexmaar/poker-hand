(ns poker-hand.core-test
  (:require [clojure.test :refer :all]
            [poker-hand.core :refer :all]))

(deftest check-hand
  (testing "One-pair"
    (is (pair? ["2H" "2S" "4C" "5C" "7D"]))
    (is (not (pair? ["2H" "3S" "4C" "5C" "7D"]))))
  (testing "Two-pairs"
    (is (two-pairs? ["2H" "2S" "4C" "4D" "7D"]))
    (is (two-pairs? ["2H" "2S" "2C" "2D" "7D"]))
    (is (not (two-pairs? ["2H" "3S" "4C" "5C" "7D"]))))
  (testing "Three-of-kind"
    (is (three-of-kind? ["2H" "2S" "2C" "4D" "7D"]))
    (is (not (three-of-kind? ["2H" "2S" "4C" "4D" "7D"]))))
  (testing "Four-of-kind"
    (is (not (four-of-kind? ["2H" "2S" "4C" "4D" "7D"])))
    (is (four-of-kind? ["2H" "2S" "2C" "2D" "7D"])))
  (testing "Flush"
    (is (flush? ["2H" "4H" "5H" "9H" "7H"]))
    (is (not (flush? ["2H" "2S" "4C" "5C" "7D"]))))
  (testing "Full-house"
    (is (full-house? ["2H" "5D" "2D" "2C" "5S"]))
    (is (not (full-house? ["2H" "2S" "2C" "4D" "7D"]))))
  (testing "Straight"
    (is (straight? ["2H" "3H" "6H" "5H" "4H"]))
    (is (straight? ["2D" "3D" "4D" "5D" "AD"]))
    (is (not (straight? ["2H" "2D" "3H" "4H" "5H"]))))
  (testing "Straight-flush"
    (is (straight-flush? ["2H" "3H" "6H" "5H" "4H"]))
    (is (not (straight-flush? ["2H" "3S" "6C" "5D" "4D"])))
    (is = (straight-flush? ["2H" "4H" "5H" "9H" "7H"])))
  (testing "Royal-flush"
    (is (royal-flush? ["TS" "AS" "QS" "KS" "JS"]))
    (is (not (royal-flush? ["2H" "4H" "5H" "9H" "7H"])))))

(deftest check-poker-hands
  (testing "One-pair loses with Two-pairs"
    (let [one-pair ["2H" "2S" "4C" "5C" "7D"]
          two-pairs ["2H" "2S" "4C" "4D" "7D"]]
      (is (= :loss (compare-poker-hands one-pair two-pairs)))))
  (testing "One-pair loses with Three-of-kind"
    (let [two-pairs ["2H" "2S" "4C" "4D" "7D"]
          three-of-kind ["2H" "2S" "2C" "4D" "7D"]]
      (is (= :win (compare-poker-hands three-of-kind two-pairs)))))
  (testing "Three-of-kind loses with Four-of-kind"
    (let [three-of-kind ["2H" "2S" "2C" "4D" "7D"]
          four-of-kind ["2H" "2S" "2C" "2D" "7D"]]
      (is (= :loss (compare-poker-hands three-of-kind four-of-kind)))))
  (testing "Straight wins with One-pair"
    (let [one-pair ["2H" "2S" "4C" "5C" "7D"]
          straight ["2H" "3S" "6C" "5D" "4D"]]
      (is (= :win (compare-poker-hands straight one-pair)))))
  (testing "Stronger Straight wins with another Straight"
    (let [straight1 ["2H" "3S" "6C" "5D" "4D"]
          straight2 ["TH" "AS" "QC" "KD" "JD"]]
      (is (= :win (compare-poker-hands straight2 straight1)))))
  (testing "Flush wins with Two-pairs"
    (let [flush ["2H" "4H" "5H" "9H" "7H"]
          two-pairs ["2H" "2S" "4C" "4D" "7D"]]
      (is (= :win (compare-poker-hands flush two-pairs)))))
  (testing "Full-house loses with Four-of-kind"
    (let [full-house ["2H" "5D" "2D" "2C" "5S"]
          four-of-kind ["2H" "2S" "2C" "2D" "7D"]]
      (is (= :loss (compare-poker-hands full-house four-of-kind)))))
  (testing "Straight-flush wins with Four-of-kind"
    (let [straight-flush ["2H" "3H" "6H" "5H" "4H"]
          four-of-kind ["2H" "2S" "2C" "2D" "7D"]]
      (is (= :win (compare-poker-hands straight-flush four-of-kind)))))
  (testing "Royal-flush wins with Flush"
    (let [royal-flush ["TS" "AS" "QS" "KS" "JS"]
          flush ["2H" "4H" "5H" "9H" "7H"]]
      (is (= :win (compare-poker-hands royal-flush flush)))))
  (testing "Four-of-kind wins with Two-pairs"
    (let [four-of-kind ["2H" "2S" "2C" "2D" "7D"]
          two-pairs ["2H" "2S" "4C" "4D" "7D"]]
      (is (= :win (compare-poker-hands four-of-kind two-pairs)))))
  (testing "High card"
    (let [ph1 ["2H" "3S" "4C" "5C" "7D"]
          ph2 ["2H" "3S" "4C" "5C" "KD"]]
    (is (= :loss (compare-poker-hands ph1 ph2))))))


