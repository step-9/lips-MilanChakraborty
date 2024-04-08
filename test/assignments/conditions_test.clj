(ns assignments.conditions-test
  (:require [clojure.test :refer [deftest testing is are]]
            [assignments.conditions :as c]))

;; When done with this, mark ^:implementing as ^:implemented
(deftest ^:implemented safe-division-test
  (testing "non zero denominator"
    (is (= 2 (c/safe-divide 4 2))))
  (testing "zero denominator"
    (is (nil? (c/safe-divide 3 0)))))

;; when ready to implement mark ^:kaocha/pending as ^:implementing
(deftest ^:implemented informative-division-test
  (testing "non zero denominator calculates result"
    (is (= 2 (c/informative-divide 4 2))))
  (testing "zero denominator yields :infinite"
    (is (= :infinite (c/informative-divide 3 0)))))

(deftest ^:implemented harishchandra-test
  (testing "falsy values"
    (is (nil? (c/harishchandra false)))
    (is (nil? (c/harishchandra nil))))
  (testing "truthy values"
    (is (= 2 (c/harishchandra 2)))
    (is (= "" (c/harishchandra "")))
    (is (= [] (c/harishchandra [])))
    (is (zero? (c/harishchandra 0)))
    (is (true? (c/harishchandra true)))))

(deftest ^:implemented yudhishtira-test
  (testing "falsy values"
    (is (= :ashwathama (c/yudishtira false)))
    (is (= :ashwathama (c/yudishtira nil))))
  (testing "truthy values"
    (is (= 2 (c/yudishtira 2)))
    (is (= "" (c/yudishtira "")))
    (is (= [] (c/yudishtira [])))
    (is (zero? (c/yudishtira 0)))
    (is (true? (c/yudishtira true)))))

(deftest ^:implemented duplicate-first-test
  (testing "empty coll"
    (is (nil? (c/duplicate-first [])))
    (is (nil? (c/duplicate-first '())))
    (is (nil? (c/duplicate-first {})))
    (is (nil? (c/duplicate-first "")))
    (is (nil? (c/duplicate-first #{}))))
  (testing "duplicates first element of non empty coll"
    (is (= [0 0] (c/duplicate-first [0])))
    (is (= [0 1 0] (c/duplicate-first [0 1])))))

(deftest ^:implemented five-point-someone-test
  (testing ":chetan-bhagat when y is 5"
    (are [x y] (= :chetan-bhagat (c/five-point-someone x y))
      0  5
      :a 5
      [] 5
      5  5))
  (testing ":satan-bhagat when x is 5"
    (are [x y] (= :satan-bhagat (c/five-point-someone x y))
      5  0
      5 :a
      5 []
      5  6))
  (testing ":greece when x is greater than y"
    (are [x y] (= :greece (c/five-point-someone x y))
      2 0
      3 1
      4 2))
  (testing ":universe when x is lesser than y"
    (are [x y] (= :universe (c/five-point-someone x y))
      0 2
      1 3
      2 4)))

(deftest ^:implemented conditions-apply-test
  (testing ":wonder-woman when 1 and 3 in order"
    (is (= :wonder-woman (c/conditions-apply [1 3])))
    (is (= :wonder-woman (c/conditions-apply [1 2 3])))
    (is (= :wonder-woman (c/conditions-apply [1 0 2 3])))
    (is (= :wonder-woman (c/conditions-apply [1 0 2 0 3])))
    (is (= :wonder-woman (c/conditions-apply [0 1 2 0 3])))
    (is (= :wonder-woman (c/conditions-apply [0 1 2 3 0]))))
  (testing ":durga when :a :b :c in order"
    (is (= :durga (c/conditions-apply [:a :b :c])))
    (is (= :durga (c/conditions-apply [:a 0 :b 0 :c])))
    (is (= :durga (c/conditions-apply [0 :a :b :c 0]))))
  (testing ":cleopatra when [2 3] and [4 5] are in order"
    (is (= :cleopatra (c/conditions-apply [[2 3] [4 5]])))
    (is (= :cleopatra (c/conditions-apply [[2 3] [3 4] [4 5]])))
    (is (= :cleopatra (c/conditions-apply [[1 2] [2 3] [3 4] [4 5] 0])))
    (is (= :cleopatra (c/conditions-apply [[1 2] [2 3] [3 4] [4 5] 0]))))
  (testing ":tuntun when none of the above apply"
    (are [x y] (= x (c/conditions-apply y))
      :tuntun [1 1]
      :tuntun [1 1 3]
      :tuntun [3 3]
      :tuntun [3 1]
      :tuntun [3 1 3]
      :tuntun [:c :a :b]
      :tuntun [:c :b :a]
      :tuntun [:b :c :a]
      :tuntun [:b :a :c]
      :tuntun [:a :c :b]
      :tuntun [:a :a :b :c]
      :tuntun [:a :b :b :c]
      :tuntun [:a :b :c :c]
      :tuntun [[2 3] [2 3] [4 5]]
      :tuntun [[2 3] [4 5] [2 3]]
      :tuntun [[4 5] [2 3]]
      :tuntun [[2 3] [2 3] [4 5]]
      :tuntun [[4 5] [2 3] [4 5]])))

(deftest ^:implemented repeat-and-truncate-test
  (testing "Returns the coll when no repeat/truncate present"
    (is (= [0 1 2 3] (c/repeat-and-truncate (range 4) false false 4))))
  (testing "Only truncate option"
    (are [x y] (= x (c/repeat-and-truncate (range 4) false true y))
      []        0
      [0]       1
      [0 1]     2
      [0 1 2]   3
      [0 1 2 3] 4
      [0 1 2 3] 5))
  (testing "Only repeat option"
    (are [x y] (= x (c/repeat-and-truncate y true false 0))
      []        []
      [0 0]     [0]
      [0 1 0 1] [0 1]))
  (testing "Truncate and repeat options"
    (are [x y] (= x (c/repeat-and-truncate (range 4) true true y))
      [] 0
      [0] 1
      [0 1] 2
      [0 1 2] 3
      [0 1 2 3] 4
      [0 1 2 3 0] 5
      [0 1 2 3 0 1] 6
      [0 1 2 3 0 1 2] 7
      [0 1 2 3 0 1 2 3] 8
      [0 1 2 3 0 1 2 3] 9)))

(deftest ^:implemented order-in-words-test
  (testing "order in words"
    (are [x y] (= x (apply c/order-in-words y))
      [:x-greater-than-y] [4 2 3]
      [:y-greater-than-z] [3 4 2]
      [:z-greater-than-x] [2 3 4]
      [:x-greater-than-y :y-greater-than-z] [4 3 2]
      [:y-greater-than-z :z-greater-than-x] [2 4 3]
      [:x-greater-than-y :z-greater-than-x] [3 2 4])))

(deftest ^:implemented zero-aliases-test
  (testing "zero like values return keywords"
    (are [x y] (= x (c/zero-aliases y))
      :zero         0
      :empty        []
      :empty        '()
      :empty-set    #{}
      :empty-map    {}
      :empty-string "")))
