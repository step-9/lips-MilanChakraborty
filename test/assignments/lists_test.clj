(ns assignments.lists-test
  (:require [clojure.test :refer :all]
            [assignments.lists :refer :all :as l]))

(deftest ^:kaocha/pending map-test
  (testing "identity with single coll"
    (is (= [1 2 3] (map' identity [1 2 3])))))

(deftest ^:kaocha/pending filter-test
  (testing "even? predicate"
    (is (= [2 4 6] (filter' even? [1 2 3 4 5 6 9])))))

(deftest ^:kaocha/pending reduce-test
  (testing "arity 2"
    (is (= 15 (reduce' + [1 2 3 4 5]))))
  (testing "arity 3"
    (is (= 15 (reduce' + 1 [2 3 4 5])))))

(deftest ^:kaocha/pending count-test
  (testing "sequence length"
    (is (= 5 (count' [1 2 3 4 5]))))
  (testing "nil"
    (is (= 0 (count' nil))))
  (testing "[]"
    (is (= 0 (count' []))))
  (testing "map"
    (is (= 2 (count' {:one 1 :two 2}))))
  (testing "string"
    (is (= 6 (count' "abcdef")))))

(deftest ^:kaocha/pending reverse-test
  (testing "seqable collection"
    (is (= [5 4 3 2 1] (reverse' [1 2 3 4 5]))))
  (testing "non-seqable collection"
    (is (nil? (reverse' 1)))))

(deftest ^:kaocha/pending every-test
  (testing "true condition"
    (is (true? (every?' even? [2 4 6 8]))))
  (testing "false condition"
    (is (false? (every?' even? [2 4 9 6 8])))))

(deftest ^:kaocha/pending some-test
  (testing "true condition"
    (is (true? (some?' even? [1 5 3 4]))))
  (testing "false condition"
    (is (false? (some?' even? [1 5 7 9])))))

(deftest ^:kaocha/pending ascending-test
  (testing "true condition"
    (is (true? (ascending? [1 2 3 4 5 6]))))
  (testing "false condition"
    (is (false? (ascending? [1 2 3 2 1 0])))))

(deftest ^:kaocha/pending index-of-test
  (testing "element present in collection"
    (is (= 4 (index-of [1 2 3 4 5 6] 5))))
  (testing "element not-present in collection"
    (is (= -1 (index-of [1 2 3 4 5 6] 9)))))

(deftest ^:kaocha/pending palindrome?-test
  (testing "true condition"
    (is (true? (palindrome? ["n" "a" "m" "a" "n"]))))
  (testing "false condition"
    (is (false? (palindrome? ["k" "a" "n" "n" "u"])))))

(deftest ^:kaocha/pending sqr-of-the-first-test
  (testing "non-empty collection"
    (is (= [16 16 16] (sqr-of-the-first [4 5 6])))))

(deftest ^:kaocha/pending double-up-test
  (testing "single dimensional collection"
    (is (= [1 1 2 2 3 3] (double-up [1 2 3]))))
  (testing "two dimensional collection"
    (is (= [[1 2] [1 2] [3 4] [3 4]] (double-up [[1 2] [3 4]])))))

(deftest ^:kaocha/pending distinct-test
  (testing "non-empty collection"
    (is (= [1 2 4 5 3] (distinct' [1 2 1 2 4 5 3 5])))))

(deftest ^:kaocha/pending dedupe'-test
  (testing "non-empty collection"
    (is (= [1 2 1 4 5 3 5] (dedupe' [1 1 2 2 1 4 5 3 5])))))

(deftest ^:kaocha/pending sum-of-adjacent-digits-test
  (testing "sum of adjacent digits of [1 2 3]"
    (is (= [3 5] (sum-of-adjacent-digits [1 2 3])))))

(deftest ^:kaocha/pending difference-test
  (testing "non-empty collection"
    (is (= [2 3] (difference [1 2 3] [1])))))

(deftest ^:kaocha/pending union-test
  (testing "non-empty collection"
    (is (= [1 2 3] (union [1 2] [2 3])))))

(deftest ^:kaocha/pending transpose-test
  (testing "non-empty collection"
    (is (= [[:a :d] [:b :e] [:c :f]] (transpose [[:a :b :c] [:d :e :f]])))))

(deftest ^:kaocha/pending cross-product-test
  (testing "non-empty collection"
    (is (= [[1 4] [1 3] [1 5] [2 4] [2 3] [2 5] [3 4]] (cross-product [1 2 3] [4 3 5])))))

(deftest ^:kaocha/pending third-or-fifth-test
  (testing "non-empty collection"
    (is (= [1 4] (third-or-fifth [1 2 3 4])))))

(deftest ^:kaocha/pending russian-dolls-test
  (testing "[1 2 3] 3 => [[[1]] [[2]] [[3]]]"
    (is (= [[[1]] [[2]] [[3]]]  (russian-dolls [1 2 3] 3)))))

(deftest ^:kaocha/pending muted-thirds-test
  (testing "[1 2 8 4 15 2 7] => [1 2 0 4 15 0 7]"
    (is (= [1 2 0 4 15 0 7] (muted-thirds [1 2 8 4 15 2 7])))))

(deftest ^:kaocha/pending split-comb-test
  (testing "odd length collection"
    (is (= '(1 3 2 4 5) (split-comb [1 2 3 4 5]))))
  (testing "even length collection"
    (is (= '(1 4 2 5 3 6) (split-comb [1 2 3 4 5 6])))))

(deftest ^:kaocha/pending max-three-digit-sequence-test
  (testing "less than 3 elements"
    (is (= [1 2] (max-three-digit-sequence [1 2]))))
  (testing "three elements or more"
    (are [x y] (= x y)
      [4 5 6] (max-three-digit-sequence [1 2 4 5 6])
      [5 2 4] (max-three-digit-sequence [1 3 5 2 4 1]))))

(deftest ^:kaocha/pending max-of-pairs-test
  (testing "max of pairs with 0 elements"
    (is (= [] (max-of-pairs []))))
  (testing "max of pairs with 1 element"
    (is (= [1] (max-of-pairs [1]))))
  (testing "max of pairs with 2 elements"
    (is (= [2] (max-of-pairs [1 2]))))
  (testing "max of pairs with more than 2 elements"
    (are [x y] (= x (max-of-pairs y))
      [2 3] [1 2 3]
      [2 3 4] [1 2 3 4]
      [2 3 4 5] [1 2 3 4 5]
      [3 3] [3 3 3]
      [2 2 3] [1 2 1 3])))

(deftest ^:kaocha/pending filter-by-index-test
  (testing "filter-by-index returns elements at odd indices"
    (are [result pred coll] (= result (filter-by-index pred coll))
      [] odd? []
      [] odd? [1]
      [2] odd? [1 2 3]
      [2 4] odd? [1 2 3 4]
      [2 4] odd? [1 2 3 4 5]))
  (testing "filter-by-index returns elements at even indices"
    (are [result pred coll] (= result (filter-by-index pred coll))
      [] even? []
      [1] even? [1]
      [1 3] even? [1 2 3]
      [1 3] even? [1 2 3 4]
      [1 3 5] even? [1 2 3 4 5]))
  (testing "filter-by-index returns elements at custom indices"
    (are [result pred coll] (= result (filter-by-index pred coll))
      [] #{0 1 4 5} []
      [1] #{0 1 4 5} [1]
      [1 2] #{0 1 4 5} [1 2]
      [1 2] #{0 1 4 5} [1 2 3]
      [1 2] #{0 1 4 5} [1 2 3 4]
      [1 2 5] #{0 1 4 5} [1 2 3 4 5]
      [1 2 5 6] #{0 1 4 5} [1 2 3 4 5 6])))

(deftest ^:kaocha/pending collatz-sequence-test
  (testing "collatz sequences"
    (are [x y] (= (collatz-sequence x) y)
      1 [1]
      2 [2 1]
      3 [3 10 5 16 8 4 2 1]
      20 [20 10 5 16 8 4 2 1]
      23 [23 70 35 106 53 160 80 40 20 10 5 16 8 4 2 1])))