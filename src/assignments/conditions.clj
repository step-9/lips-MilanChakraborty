(ns assignments.conditions)

(defn safe-divide
  "Returns the result of x/y unless y is 0. Returns nil when y is 0"
  {:level :easy
   :use   '[when-not zero?]}
  [x y])

(defn informative-divide
  "Returns the result of x/y unless y is 0. Returns :infinite when y is 0"
  {:level :easy
   :use   '[if-not zero?]}
  [x y])

(defn harishchandra
  "Only returns truthy values as themselves.
  Falsy values(false and nil) return nil"
  {:level :easy
   :use   '[when-let]}
  [x])

(defn yudishtira
  "Only returns truthy values as themselves.
  Falsy values(false and nil) return :ashwathama"
  {:level :easy
   :use   '[if-let]}
  [x])

(defn duplicate-first
  "Returns coll with the first element duplicated.
  Returns nil if col is empty"
  {:level      :easy
   :use        '[when-first concat]
   :alternates '[empty? seq? conj into]}
  [coll])

(defn five-point-someone
  "Returns :chetan-bhagat if y is 5.
  If x is 5 is returns :satan-bhagat.
  If x is greater than y it returns :greece
  Otherwise it returns :universe"
  {:level :easy
   :use   '[cond]}
  [x y])

(defn conditions-apply
  "Given a collection of any length, returns:
  :wonder-woman if collection has a single occurrence of 1 and 3 in that order
  :durga        if collection has a single occurrence of :a :b and :c in that order
  :cleopatra    if collection has a single occurrence of [2 3] and [4 5] in that order
  :tuntun if none of the conditions apply"
  {:level      :medium
   :use        '[condp filter]
   :alternates '[if cond]}
  [coll])

(defn repeat-and-truncate
  "Given coll and options to repeat and truncate
  returns a collection that optionally repeats itself
  and is optionally truncated to the first n elements.
  (repeat-and-truncate (range 4) true true 6) => '(0 1 2 3 0 1)"
  {:level :medium
   :use   '[cond->> concat take]}
  [coll rep? truncate? n])

(defn order-in-words
  "Given x, y and z, returns a vector consisting of
  sentences that explicitly mention if x > y or y > z or z > x.
  (order-in-words 4 3 2) => [:x-greater-than-y :y-greater-than-z]
  (order-in-words 4 3 5) => [:x-greater-than-y :z-greater-than-x]
  (order-in-words 2 3 4) => [:z-greater-than-x]"
  {:level :easy
   :use   '[cond-> conj]}
  [x y z])

(defn zero-aliases
  "Given a zero-like value(0,[],(),#{},{}) should
  give back an alias for each type of zero like value
  or a default of :not-zero
  0 -> :zero
  [] -> :empty
  '() -> :empty
  #{} -> :empty-set
  {}  -> :empty-map
  \"\"  -> :empty-string"
  {:level :easy
   :use   '[case]}
  [zero-like-value])
