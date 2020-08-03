(ns poker-hand.core-test
  (:require [clojure.test :refer :all]
            [poker-hand.core :refer :all]))

(deftest check-hand
  (testing "One-pair"
    (is (= true (pair? ["2H" "2S" "4C" "5C" "7D"])))
    (is (= false (pair? ["2H" "3S" "4C" "5C" "7D"]))))
  (testing "Two-pairs"
    (is (= true (two-pairs? ["2H" "2S" "4C" "4D" "7D"])))
    (is (= true (two-pairs? ["2H" "2S" "2C" "2D" "7D"])))
    (is (= false (two-pairs? ["2H" "3S" "4C" "5C" "7D"]))))
  (testing "Three-of-kind"
    (is (= true (three-of-kind? ["2H" "2S" "2C" "4D" "7D"])))
    (is (= false (three-of-kind? ["2H" "2S" "4C" "4D" "7D"]))))
  (testing "Four-of-kind"
    (is (= false (four-of-kind? ["2H" "2S" "4C" "4D" "7D"])))
    (is (= true (four-of-kind? ["2H" "2S" "2C" "2D" "7D"]))))
  (testing "Flush"
    (is (= true (flush? ["2H" "4H" "5H" "9H" "7H"])))
    (is (= false (flush? ["2H" "2S" "4C" "5C" "7D"]))))
  (testing "Full-house"
    (is (= true (full-house? ["2H" "5D" "2D" "2C" "5S"])))
    (is (= false (full-house? ["2H" "2S" "2C" "4D" "7D"]))))
  (testing "Straight"
    (is (= true (straight? ["2H" "3H" "6H" "5H" "4H"])))
    (is (= true (straight? ["2D" "3D" "4D" "5D" "AD"])))
    (is (= false (straight? ["2H" "2D" "3H" "4H" "5H"]))))
  (testing "Straight-flush"
    (is (= true (straight-flush? ["2H" "3H" "6H" "5H" "4H"])))
    (is (= false (straight-flush? ["2H" "3S" "6C" "5D" "4D"])))
    (is = (= false (straight-flush? ["2H" "4H" "5H" "9H" "7H"]))))
  (testing "Royal-flush"
    (is (= true (royal-flush? ["TS" "AS" "QS" "KS" "JS"])))
    (is (= false (royal-flush? ["2H" "4H" "5H" "9H" "7H"])))))

(deftest check-poker-hands
  (testing "One-pair loses with Two-pairs"
    (let [one-pair ["2H" "2S" "4C" "5C" "7D"]
          two-pairs ["2H" "2S" "4C" "4D" "7D"]]
      (is (= "loss" (compare-poker-hands one-pair two-pairs)))))
  (testing "One-pair loses with Three-of-kind"
    (let [two-pairs ["2H" "2S" "4C" "4D" "7D"]
          three-of-kind ["2H" "2S" "2C" "4D" "7D"]]
      (is (= "win" (compare-poker-hands three-of-kind two-pairs)))))
  (testing "Three-of-kind loses with Four-of-kind"
    (let [three-of-kind ["2H" "2S" "2C" "4D" "7D"]
          four-of-kind ["2H" "2S" "2C" "2D" "7D"]]
      (is (= "loss" (compare-poker-hands three-of-kind four-of-kind)))))
  (testing "Straight wins with One-pair"
    (let [one-pair ["2H" "2S" "4C" "5C" "7D"]
          straight ["2H" "3S" "6C" "5D" "4D"]]
      (is (= "win" (compare-poker-hands straight one-pair)))))
  (testing "Stronger Straight wins with another Straight"
    (let [straight1 ["2H" "3S" "6C" "5D" "4D"]
          straight2 ["TH" "AS" "QC" "KD" "JD"]]
      (is (= "win" (compare-poker-hands straight2 straight1)))))
  (testing "Flush wins with Two-pairs"
    (let [flush ["2H" "4H" "5H" "9H" "7H"]
          two-pairs ["2H" "2S" "4C" "4D" "7D"]]
      (is (= "win" (compare-poker-hands flush two-pairs)))))
  (testing "Full-house loses with Four-of-kind"
    (let [full-house ["2H" "5D" "2D" "2C" "5S"]
          four-of-kind ["2H" "2S" "2C" "2D" "7D"]]
      (is (= "loss" (compare-poker-hands full-house four-of-kind)))))
  (testing "Straight-flush wins with Four-of-kind"
    (let [straight-flush ["2H" "3H" "6H" "5H" "4H"]
          four-of-kind ["2H" "2S" "2C" "2D" "7D"]]
      (is (= "win" (compare-poker-hands straight-flush four-of-kind)))))
  (testing "Royal-flush wins with Flush"
    (let [royal-flush ["TS" "AS" "QS" "KS" "JS"]
          flush ["2H" "4H" "5H" "9H" "7H"]]
      (is (= "win" (compare-poker-hands royal-flush flush)))))
  (testing "Four-of-kind wins with Two-pairs"
    (let [four-of-kind ["2H" "2S" "2C" "2D" "7D"]
          two-pairs ["2H" "2S" "4C" "4D" "7D"]]
      (is (= "win" (compare-poker-hands four-of-kind two-pairs)))))
  (testing "High card"
    (let [ph1 ["2H" "3S" "4C" "5C" "7D"]
          ph2 ["2H" "3S" "4C" "5C" "KD"]]
    (is (= "loss" (compare-poker-hands ph1 ph2))))))


