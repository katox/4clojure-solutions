;;
;; foreclojure-solutions, 2012 Kamil Toman
;; Distributed under the Eclipse Public License, the same as 4Clojure.
;;
(ns foreclojure-solutions.core)

; imports
(use 'clojure.set)

;; 4clojure solutions
;; This only contains problems that require writing some real code

;; 15. Double Down
;; Write a function which doubles a number.
* 2

;; 16. Hello World
;; Write a function which returns a personalized greeting.
format "Hello, %s!"

;; 19. Last Element
;; Write a function which returns the last element in a sequence.
(fn[coll] (nth coll (dec (count coll))))

;; 20. Penultimate Element
;; Write a function which returns the second to last element from
;; a sequence.
#(first (take-last 2 %))

;; 21. Nth Element
;; Write a function which returns the Nth element from a sequence.
#(get (vec %1) %2)

;; 22. Count a Sequence
;; Write a function which returns the total number of elements in a sequence.
#(reduce (fn[x y] (inc x)) 0 %)

;; 23. Reverse a Sequence
;; Write a function which reverses a sequence.
#(reduce (fn [x y] (conj x y)) '() %)

;; 24. Sum It All Up
;; Write a function which returns the sum of a sequence of numbers.
reduce + 0

;; 25. Find the odd numbers
;; Write a function which returns only the odd numbers from a sequence.
filter odd?

;; 26. Fibonacci Sequence
;; Write a function which returns the first X fibonacci numbers.
(fn [x]
  (take x
    ((fn fib [a b]
        (cons a (lazy-seq (fib b (+ a b)))))
     1 1)))

;; 27. Palindrome Detector
;; Write a function which returns true if the given sequence is a palindrome.
;; Hint: "racecar" does not equal '(\r \a \c \e \c \a \r)
#(= (vec %) (rseq (vec %)))

;; 28. Flatten a Sequence
;; Write a function which flattens a sequence.
(fn flt[coll]
          (let [l (first coll) r (next coll)]
               (concat
                (if (sequential? l)
                    (flt l)
                    [l])
                (when (sequential? r)
                  (flt r)))))

;; 29. Get the Caps
;; Write a function which takes a string and returns a new string
;; containing only the capital letters.
(fn only-lower [s] (apply str (re-seq #"[A-Z]+" s)))

;; 30. Compress a Sequence
;; Write a function which removes consecutive duplicates from a
;; sequence.
(fn [coll] (map first (partition-by identity coll)))

;; 31. Pack a Sequence
;; Write a function which packs consecutive duplicates into
;; sub-lists.
(fn pack-seq [s] (partition-by identity (vec s)))

;; 32. Duplicate a Sequence
;; Write a function which duplicates each element of a sequence.
mapcat #(vector % %)

;; 33. Replicate a Sequence
;; Write a function which replicates each element of a sequence a
;; variable number of times.
(fn [s n] (mapcat (fn [i] (repeatedly n #(identity i))) s))

;; 34. Implement range
;; Write a function which creates a list of all integers in a
;; given range.
(fn [x y] (take (- y x) (iterate inc x)))

;; 38. Maximum value
;; Write a function which takes a variable number of parameters
;; and returns the maximum value.
(fn [& rest] (reduce #(if (> %1 %2) %1 %2) rest))

;; 39. Interleave Two Seqs
;; Write a function which takes two sequences and returns the
;; first item from each, then the second item from each, then the
;; third, etc.
mapcat (fn [& x] x)

;; 40. Interpose a Seq
;; Write a function which separates the items of a sequence by an
;; arbitrary value.
(fn [sep s] (rest (mapcat #(list sep %) s)))

;; 41. Drop Every Nth Item
;; Write a function which drops every Nth item from a sequence.
(fn [s n] (mapcat (partial take (dec n)) (partition-all n s)))

;; 42. Factorial Fun
;; Write a function which calculates factorials.
#(apply * (range 1 (inc %)))

;; 43. Reverse Interleave
;; Write a function which reverses the interleave process into x
;; number of subsequences.
(fn [s num] (let [ivec (map-indexed #(vector (mod % num) %2) s)]
                            (vals (sort (reduce
                                    (fn [ret x]
                                      (let [k (first x), v (last x)]
                                        (assoc ret k (conj (get ret k []) v))))
                                    {}
                                    ivec)))))

;; 44. Rotate Sequence
;; Write a function which can rotate a sequence in either direction.
(fn [dir s]
  (let [n (mod dir (count s))]
    (concat (drop n s) (take n s))))

;; 46. Flipping out
;; Write a higher-order function which flips the order of the
;; arguments of an input function.
(fn [f] (fn [arg1 arg2 & args] (apply f arg2 arg1 args)))

;; 49. Split a sequence
;; Write a function which will split a sequence into two parts.
#(vector (take % %2) (drop % %2))

;; 50. Split by Type
;; Write a function which takes a sequence consisting of items with
;; different types and splits them up into a set of homogeneous
;; sub-sequences. The internal order of each sub-sequence should be
;; maintained, but the sub-sequences themselves can be returned in any
;; order (this is why 'set' is used in thetest cases).
#(vals (group-by type %))

;; 53. Longest Increasing Sub-Seq
;; Given a vector of integers, find the longest consecutive
;; sub-sequence of increasing numbers. If two sub-sequences have the
;; same length, use the one that occurs first. An increasing
;; sub-sequence must have a length of 2 or greater to qualify.
(fn sub[s]
  (let [subb (fn [s curs maxs]
             (let [x (first s)
                    r (rest s)
                    ncurs (if (= ((fnil dec 0) x) (peek curs)) (conj curs x) [x])
                    nmax (max-key count ncurs maxs)]
                 (if (seq r)
                    (recur r ncurs nmax)
                     nmax)))
        longest (subb s [(first s)] [])]
    (if (> (count longest) 1) longest [])))

;; 54. Partition a Sequence
;; Write a function which returns a sequence of lists of x items each.
;; Lists of less than x items should not be returned.
(fn my-partition [num s]
  (let [[x y] (split-at num s)]
    (if (<= num (count x))
      (cons x (my-partition num y)))))

;; 55. Count Occurrences
;; Write a function which returns a map containing the number of
;; occurences of each distinct item in a sequence.
#(reduce (fn [res x] (update-in res [x] (fnil inc 0))) {} %)

;; 56. Find Distinct Items
;; Write a function which removes the duplicates from a sequence.
;; Order of the items must be maintained.
#(let [dst (fn [sq rs st]
              (let [x (first sq)
                    r (rest sq)
                    nrs (if (get st x) rs (conj rs x))]
                   (if (seq r)
                     (recur r nrs (conj st x))
                     nrs)))]
   (dst % [] #{}))

;; 58. Function Composition
;; Write a function which allows you to create function compositions.
;; The parameter list should take a variable number of functions, and
;; create a function applies them from right-to-left.
(fn [& funcs]
  (fn [& args] (first (reduce #(list (apply %2 %)) args (reverse funcs)))))

;; 59. Juxtaposition
;; Take a set of functions and return a new function that takes a
;; variable number of arguments and returns a sequence containing the
;; result of applying each function left-to-right to the argument
;; list.
(fn [& funcs] (fn [& args] (map #(apply % args) funcs)))

;; 60. Sequence Reductions
;; Write a function which behaves like reduce, but returns each
;; intermediate value of the reduction. Your function must accept
;; either two or three arguments, and the return sequence must be
;; lazy.
(fn myreductions
  ([f coll] (myreductions f (first coll) (rest coll)))
  ([f val coll]
     (cons val
           (lazy-seq
             (when-let [s (seq coll)]
               (myreductions f (f val (first s)) (rest s)))))))

;; 61. Map Construction
;; Write a function which takes a vector of keys and a vector of
;; values and constructs a map from them.
#(apply merge (map hash-map % %2))

;; 62. Re-implement Iterate
;; Given a side-effect free function f and an initial value x write a
;; function which returns an infinite lazy sequence of x, (f x),
;; (f (f x)), (f (f (f x))), etc.
(fn it [f x] (cons x (lazy-seq (it f (f x)))))

;; 63. Group a Sequence
;; Given a function f and a sequence s, write a function which returns
;; a map. The keys should be the values of f applied to each item in
;; s. The value at each key should be a vector of corresponding items
;; in the order they appear in s.
(fn g [f s]
  (reduce (fn [r v] (let [add #((fnil conj []) % %2)]
                     (update-in r [(f v)] add v))) {} s))

;; 65. Black Box Testing
;; Clojure has many sequence types, which act in subtly different
;; ways. The core functions typically convert them into a uniform
;; "sequence" type and work with them that way, but it can be
;; important to understand the behavioral and performance differences
;; so that you know which kind is appropriate for yourapplication.
;;
;; Write a function which takes a collection and returns one of :map,
;; :set, :list, or :vector - describing the type of collection it was
;; given. You won't be allowed to inspect their class or use the
;; built-in predicates like list? - the point is to poke at them and
;; understand their behavior.
(fn[x]
  (let [t (conj (empty x) [:a :b] [:c :d])]
    (cond
      (:a t) :map
      (get t 0) :vector
      (get t [:a :b]) :set
      :else :list)))

;; 66. Greatest Common Divisor
;; Given two integers, write a function which returns the greatest
;; common divisor.
(fn[a b]
  (let [dv (fn[x] (set (filter #(zero? (rem x %)) (range 1 (inc x)) )))]
    (apply max (clojure.set/intersection (dv a) (dv b)))))

;; 67. Prime Numbers
;; Write a function which returns the first x number of prime numbers.
(fn [x]
  (let [prime? (fn [n pvec] (every? #(pos? (rem n %)) pvec))
        gen-primes (fn [x n pvec]
          (if (pos? x)
            (if (prime? n pvec)
              (recur (dec x) (+ n 2) (conj pvec n))
              (recur x (+ n 2) pvec))
            pvec))]
    (gen-primes (dec x) 3 [2])))

;; 69. Merge with a Function
;; Write a function which takes a function f and a variable number of
;; maps. Your function should return a map that consists of the rest
;; of the maps conj-ed onto the first. If a key occurs in more than
;; one map, the mapping(s) from the latter (left-to-right) should be
;; combined with the mapping in the result by calling (f val-in-result
;; val-in-latter)
(fn [f & maps]
  (reduce #(conj %
    (reduce (fn [r [k v]]
              (let [rv (get r k)] (assoc r k (if rv (f rv v) v)))) % %2))
          maps))

;; 70. Word Sorting
;; Write a function that splits a sentence up into a sorted list of
;; words. Capitalization should not affect sort order and punctuation
;; should be ignored.
(fn [s] (sort-by #(.toLowerCase %) (re-seq #"\w+" s)))

;; 73. Analyze a Tic-Tac-Toe Board
;; A tic-tac-toe board is represented by a two dimensional vector. X
;; is represented by :x, O is represented by :o, and empty is
;; represented by :e. A player wins by placing three Xs or three Os in
;; a horizontal, vertical, or diagonal row. Write a function which
;; analyzes a tic-tac-toe board and returns :x if X has won, :o if O
;; has won, and nil if neitherplayer has won.
(fn [[a b c]]
  (let [q (into [a b c
                 [(first a)(second b)(last c)]
                 [(first c)(second b)(last a)]] (map vector a b c))
        won? (fn [[x & r]] (when (and (apply = x r) (not= :e x)) x))]
    (first (remove nil? (map won? q)))))

;; 74. Filter Perfect Squares
;; Given a string of comma separated integers, write a function which
;; returns a new comma separated string that only contains the numbers
;; which are perfect squares.
(fn [s]
  (letfn [(i[s] (map #(Integer/parseInt %) (re-seq #"\d+" s)))]
    (apply str (interpose \,
                          (keep #(let [sq (Math/sqrt %)]
                                      (when (== (* sq sq) %) %))
                                (i s))))))

;; 75. Euler's Totient Function
;; Two numbers are coprime if their greatest common divisor equals 1.
;; Euler's totient function f(x) is defined as the number of positive
;; integers less than x which are coprime to x. The special case f(1)
;; equals 1. Write a function which calculates Euler's totient
;; function.
(fn [x]
  (let [gcd (fn [a b]
             (cond
              (= b 0) a
              (= a 0) b
              (> a b) (recur b (mod a b))
              :else (recur a (mod b a))))
        coprime? #(= (gcd % x) 1)]
       (if (= 1 x) 1
           (count (filter coprime? (range 1 x))))))

;; 77. Anagram Finder
;; Write a function which finds all the anagrams in a vector of words.
;; A word x is an anagram of word y if all the letters in x can be
;; rearranged in a different order to form y. Your function should
;; return a set of sets, where each sub-set is a group of words which
;; are anagrams of each other. Each sub-set should have at least two
;; words. Words without any anagrams should not be included in the
;; result.
(fn [words]
  (let [hash-key #(apply str (sort (.toLowerCase %)))
        groups (vals (group-by hash-key words))]
    (set (map set (filter #(> (count %) 1) groups)))))

;; 78. Reimplement Trampoline
;; Reimplement the function described in "Intro to Trampoline".
(fn t
  ([f] (let [r (f)]
            (if (fn? r)
              (recur r)
              r)))
  ([f & a] (t #(apply f a))))

;; 79. Triangle Minimal Path
;; Write a function which calculates the sum of the minimal path
;; through a triangle. The triangle is represented as a collection of
;; vectors. The path should start at the top of the triangle and move
;; to an adjacent number on the next row until the bottom of the
;; triangle is reached.
(fn [t]
  (let [min-step (fn [s] (map (partial apply min) (partition 2 1 s)))]
    (first
      (reduce
        #(map + (min-step %1) %2)
        (reverse t)))))

;; 80. Perfect Numbers
;; A number is "perfect" if the sum of its divisors equal the number
;; itself. 6 is a perfect number because 1+2+3=6. Write a function
;; which returns true for perfect numbers and false otherwise.
(fn [n]
  (let [dv (fn[x] (filter #(zero? (rem x %)) (range 1 x) ))]
    (= n (reduce + (dv n)))))

;; 81. Set Intersection
;; Write a function which returns the intersection of two sets. The
;; intersection is the sub-set of items that each set has in common.
(fn [r s] (reduce #(if (contains? s %2) (conj % %2) %) #{} r))

;; 82. Word Chains
;; A word chain consists of a set of words ordered so that each word
;; differs by only one letter from the words directly before and after
;; it. The one letter difference can be either an insertion, a
;; deletion, or a substitution. Here is an example word chain:
;;
;; cat -> cot -> coat -> oat -> hat -> hot -> hog -> dog
;;
;; Write a function which takes a sequence of words, and returns true
;; if they can be arranged into one continous word chain, and false if
;; they cannot.
(fn [s]
  (letfn [(permutations [r s]
            (if (seq s)
              (mapcat #(permutations (conj r %) (disj s %)) (seq s))
              [r]))
          (chainable? [x y]
            (let [diffs (fn [x y] (<= (reduce + (map #(if (= %1 %2) 0 1) x y)) 1))
                  drop-char (fn [n s] (str (subs s 0 n) (subs s (inc n) (count s))))
                  drop-variants (fn [s] (map #(drop-char % s) (range (count s))))
                  deletions (fn [l s] (some #(= s %) (drop-variants l)))
                  cx (count x)
                  cy (count y)]
              (cond
                (= cx cy) (diffs x y)
                (= cx (inc cy)) (deletions x y)
                (= (inc cx) cy) (deletions y x))))
          (chain? [s]
            (reduce #(when (chainable? %1 %2) %2) s))]
    (boolean
      (some chain? (permutations [] s)))))

;; 83. A Half-Truth
;; Write a function which takes a variable number of booleans. Your
;; function should return true if some of the parameters are true, but
;; not all of the parameters are true. Otherwise your function should
;; return false.
#(and (not= nil (some true? %&)) (not-every? true? %&))

;; 84. Transitive Closure
;; Write a function which generates the transitive closure of a binary
;; relation. The relation will be represented as a set of 2 item
;; vectors.
(fn [e]
  (let [kp (fn [x] (keep #(if (= (first %) x) (second %)) e))
        pair (fn [x s] (for [y s] [x y]))
        n (remove empty? (mapcat #(pair (first %) (kp (second %))) e))
        ne (into e n)]
    (if (= e ne) e (recur ne))))

;; 85. Power Set
;; Write a function which generates the power set of a given set. The
;; power set of a set x is the set of all subsets of x, including the
;; empty set and x itself.
(fn [s]
  (let [p (fn [c n s]
               (cond (zero? n) (assoc c 0 #{})
                     (= n 1) (assoc c 1 (set (map hash-set s)))
                     :else (assoc c n
                                  (reduce into #{}
                                          (for [i s]
                                            (map #(conj % i) (c (dec n))))))))]
    (reduce into (set [#{} s]) (vals (reduce #(p % %2 s) {} (range (count s)))))))

;; 86. Happy numbers
;; Happy numbers are positive integers that follow a particular
;; formula: take each individual digit, square it, and then sum the
;; squares to get a new number. Repeat with the new number and
;; eventually, you might get to a number whose squared sum is 1. This
;; is a happy number. An unhappy number (or sad number) is one that
;; loops endlessly. Write a function that determines if a number is
;; happy or not.
(fn [n]
  (= 1 ((fn [s n]
     (let [i (fn [n] (reduce #(+ % (* %2 %2)) 0 (map #(- (int %) (int \0)) (str n))))]
          (if (contains? s n)
            n
            (recur (conj s n) (i n))))) #{} n)))

;; 88. Symmetric Difference
;; Write a function which returns the symmetric difference of two
;; sets. The symmetric difference is the set of items belonging to one
;; but not both of the two sets.
#(clojure.set/difference (clojure.set/union % %2) (clojure.set/intersection % %2))

;; 89. Graph Tour
;; Starting with a graph you must write a function that returns true
;; if it is possible to make a tour of the graph in which every edge
;; is visited exactly once. The graph is represented by a vector of
;; tuples, where each tuple represents a single edge.
;; The rules are:
;; - You can start at any node.
;; - You must visit each edge exactly once.
;; - All edges are undirected.
(fn [edges]
  (let [v (zipmap (distinct (flatten edges)) (repeat 0))
        degrees (vals (reduce
                        (fn [r [a b]]
                          (if (not= a b)
                            (-> r
                              (update-in [a] inc)
                              (update-in [b] inc))
                            r))
                        v edges))
        odd (count (filter odd? degrees))
        connected? (fn [edges]
                     (let [find (fn [union k] (or (some #(if (contains? % k) %) union) #{k}))]
                       (= 1 (count
                              (reduce (fn [r [a b]]
                                        (let [ua (find r a)
                                              ub (find r b)]
                                          (-> r
                                            (disj ua ub)
                                            (conj (clojure.set/union ua ub)))))
                                #{} edges)))))]
    (and
      (or (= odd 2) (= odd 0))
      (connected? edges))))

;; 90. Cartesian Product
;; Write a function which calculates the Cartesian product of two sets.
#(set (for [x % y %2] [x y]))

;; 91. Graph Connectivity
;; Given a graph, determine whether the graph is connected. A
;; connected graph is such that a path exists between any two given
;; nodes.
;; -Your function must return true if the graph is connected and false otherwise.
;; -You will be given a set of tuples representing the edges of a
;;  graph. Each member of a tuple being a vertex/node in the graph.
;; -Each edge is undirected (can be traversed either direction). 
(fn [edges]
  (let [find (fn [union k] (or (some #(if (contains? % k) %) union) #{k}))]
    (= 1 (count
           (reduce (fn [r [a b]]
                     (let [ua (find r a)
                           ub (find r b)]
                       (-> r
                         (disj ua ub)
                         (conj (clojure.set/union ua ub)))))
             #{} edges)))))

;; 92. Read Roman numerals
;; Roman numerals are easy to recognize, but not everyone knows all
;; the rules necessary to work with them. Write a function to parse a
;; Roman-numeral string and return the number it represents.
;; You can assume that the input will be well-formed, in upper-case,
;; and follow the subtractive principle. You don't need to handle any
;; numbers greater than MMMCMXCIX (3999), the largest number
;; representable with ordinary letters.
(fn [s]
  (let [rom {\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000}]
       (apply + (remove nil? (reduce (fn [[r l] x]
                          (let [n (rom x)]
                               (cond (nil? l) [r n]
                                     (> n l) [(+ r (- n l)) nil]
                                     :else [(+ r l) n])))
                                     [0 nil] s)))))

;; 93. Partially Flatten a Sequence
;; Write a function which flattens any nested combination of
;; sequential things (lists, vectors, etc.), but maintains the lowest
;; level sequential items. The result should be a sequence of
;; sequences with only one level of nesting.
(fn exp [acc s]
  (if (some coll? s)
    (exp (exp acc (first s)) (rest s))
    (if (seq s)
      (conj acc s)
      acc))) []

;; 94. Game of Life
;; The game of life is a cellular automaton devised by mathematician
;; John Conway. 
;; The 'board' consists of both live (#) and dead ( ) cells. Each cell
;; interacts with its eight neighbours (horizontal, vertical,
;; diagonal), and its next state is dependent on the followingrules:
;; 1) Any live cell with fewer than two live neighbours dies, as if
;; caused by under-population.
;; 2) Any live cell with two or three live neighbours lives on to the
;; next generation.
;; 3) Any live cell with more than three live neighbours dies, as if
;; by overcrowding.
;; 4) Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
;; Write a function that accepts a board, and returns a board
;; representing the next generation of cells.
(fn [board]
  (let [size (count board)
        alive? (fn [x y] (= (get-in board [x y]) \#))
        neighbours (fn [x y]
                     (reduce #(if %2 (inc %) %) 0
                             (for [i [-1 0 1]
                                   j [-1 0 1]
                                   :when (not= i j 0)]
                               (alive? (+ x i) (+ y j)))))
        cell (fn [x y n] (if (or (= n 3) (and (= n 2) (alive? x y))) \# \space))]
    (for [x (range size)]
      (apply str (for [y (range size)]
                   (cell x y (neighbours x y)))))))

;; 95. To Tree, or not to Tree
;; Write a predicate which checks whether or not a given sequence
;; represents a binary tree. Each node in the tree must have a value,
;; a left child, and a rightchild.
(fn tree? [t]
    (cond (nil? t) true
          (coll? t) (and (= (count t) 3)
                         ((complement coll?) (first t))
                         (tree? (second t))
                         (tree? (last t)))
          :else false))

;; 96. Beauty is Symmetry
;; Let us define a binary tree as "symmetric" if the left half of the
;; tree is the mirror image of the right half of the tree. Write a
;; predicate to determine whether or not a given binary tree is
;; symmetric. (see To Tree, or not to Tree for a reminder on the tree
;; representation we're using).
(fn sym?
  ([t] (sym? (second t) (last t)))
  ([a b] (or
          (and (nil? a) (nil? b))
          (if (and (coll? a) (coll? b))
            (and (= (count a) (count b) 3)
                 (= (first a) (first b))
                 (sym? (second a) (last b))
                 (sym? (last a) (second b)))
            false))))

;; 97. Pascal's Triangle
;; Pascal's triangle is a triangle of numbers computed using the
;; following rules:
;; - The first row is 1.
;; - Each successive row is computed by adding together adjacent
;; numbers in the row above, and adding a 1 to the beginning and end
;; of the row.
;; Write a function which returns the nth row of Pascal's Triangle.
(fn pascal[n]
  (cond (= n 1) [1]
        (= n 2) [1 1]
        :else (conj (first
                     (reduce (fn[[r l] x] [(conj r (+ l x)) x]) [[] 0] (pascal(dec n))))
                    1)))

;; 98. Equivalence Classes
;; A function f defined on a domain D induces an equivalence relation
;; on D, as follows: a is equivalent to b with respect to f if and
;; only if (f a) is equal to (f b). Write a function with arguments f
;; and D that computes the equivalence classes of D with respect to f.
(fn [f d]
  (set (map set (vals (group-by f d)))))

;; 99. Product Digits
;; Write a function which multiplies two numbers and returns the
;; result as a sequence of its digits.
(fn [a b] (map #(- (int %) (int \0)) (seq (str (* a b)))))

;; 100 Least Common Multiple
;; Write a function which calculates the least common multiple. Your
;; function should accept a variable number of positive integers or
;; ratios.
(fn [& nums]
  (let [gcd (fn [a b]
             (cond
              (= b 0) a
              (= a 0) b
              (> a b) (recur b (mod a b))
              :else (recur a (mod b a))))]
    (reduce (fn [a b] (* (/ a (gcd a b)) b)) nums)))

;; 101. Levenshtein Distance
;; Given two sequences x and y, calculate the Levenshtein distance of
;; x and y, i. e. the minimum number of edits needed to transform x
;; into y. The allowed edits are:
;; - insert a single item
;; - delete a single item
;; - replace a single item with another item
;; WARNING: Some of the test cases may timeout if you write an
;; inefficient solution!
(comment placeholder)

;; 102. intoCamelCase
;; When working with java, you often need to create an object with
;; fieldsLikeThis, but you'd rather work with a hashmap that has
;; :keys-like-this until it's time to convert. Write a function which
;; takes lower-case hyphen-separated strings and converts them to
;; camel-case strings.
(fn [s]
  (let [parts (re-seq #"\w+" s)]
    (apply str (first parts) (map clojure.string/capitalize (rest parts)))))

;; 103. Generating k-combinations
;; Given a sequence S consisting of n elements generate all
;; k-combinations of S, i. e. generate all possible sets consisting of
;; k distinct elements taken from S. The number of k-combinations for
;; a sequence is equal to the binomial coefficient.
(fn [n s]
  (let [p (fn [c n s]
               (cond (zero? n) (assoc c 0 #{})
                     (= n 1) (assoc c 1 (set (map hash-set s)))
                     :else (assoc c n
                                  (reduce into #{}
                                          (for [i s]
                                            (map #(conj % i) (c (dec n))))))))]
       (cond
        (< (count s) n) #{}
        (= (count s) n) (hash-set (set s))
        :else (set (filter #(= (count %) n)
                           ((reduce #(p % %2 s) {} (range (count s))) n s))))))

;; 104. Write Roman Numerals
;; This is the inverse of Problem 92, but much easier. Given an
;; integer smaller than 4000, return the corresponding roman numeral
;; in uppercase, adhering to the subtractive principle.
(fn wnum [n]
  (let [r (sorted-map-by > 1000 "M" 900 "CM" 500 "D" 400 "CD" 100 "C"
                      90 "XC" 50 "L" 40 "XL" 10 "X" 9 "IX" 5 "V" 4 "IV" 1 "I")
        m (some #(when (>= (- n %) 0) %) (keys r))]
  (when-not (nil? m)
    (str (r m) (wnum (- n m))))))

;; 105. Identify keys and values
;; Given an input sequence of keywords and numbers, create a map such
;; that each key in the map is a keyword, and the value is a sequence
;; of all the numbers (if any) between it and the next keyword in the
;; sequence.
(fn conv [s]
  (reduce (fn [r [k v]]
          (assoc (into r (zipmap k (repeat []))) (last k) v)) {}
          (partition 2 (partition-by keyword? s))))

;; 106. Number Maze
;; Given a pair of numbers, the start and end point, find a path
;; between the two using only three possible operations:
;; - double
;; - halve (odd numbers cannot be halved)
;; - add 2
;; Find the shortest path through the "maze". Because there are
;; multiple shortest paths, you must return the length of the shortest
;; path, not the path itself.
(comment placeholder)

;; 107. Simple closures
;; Lexical scope and first-class functions are two of the most basic
;; building blocks of a functional language like Clojure. When you
;; combine the two together, you get something very powerful called
;; lexical closures. With these, you can exercise a great deal of
;; control over the lifetime of your local bindings, saving their
;; values for use later, long after the code you're running now has
;; finished.
;; It can be hard to follow in the abstract, so let's build a simple
;; closure. Given a positive integer n, return a function (f x) which
;; computes xn. Observe that the effect of this is to preserve the
;; value of n for use outside the scope in which it is defined.
(fn [x] (fn [n] (reduce * (repeat x n))))

;; 108. Lazy Searching
;; Given any number of sequences, each sorted from smallest to
;; largest, find the smallest number which appears in each sequence.
;; The sequences may be infinite, so be careful to search lazily.
(fn [& s]
  ((fn [c]
      (let [f (map first c) m (apply min f)]
           (if (apply = f)
             (ffirst c)
             (recur (map #(if (= m (first %)) (rest %) %) c))))) s))

;; 110. Sequence of pronunciations
;; Write a function that returns a lazy sequence of "pronunciations"
;; of a sequence of numbers. A pronunciation of each element in the
;; sequence consists of the number of repeating identical numbers and
;; the number itself. For example, [1 1] is pronounced as [2 1] ("two
;; ones"), which in turn is pronounced as [1 2 1 1] ("one two, one one").
;; Your function should accept an initial sequence of numbers, and
;; return an infinite lazy sequence of pronunciations, each element
;; being a pronunciation of the previous element.
(fn [s]
  (drop 1 (iterate (fn [s]
                     (mapcat (juxt count first)
                             (partition-by identity s))) s)))

;; 111. Crossword puzzle
;; Write a function that takes a string and a partially-filled
;; crossword puzzle board, and determines if the input string can be
;; legally placed onto theboard.
;; The crossword puzzle board consists of a collection of
;; partially-filled rows. Empty spaces are denoted with an underscore
;; (_), unusable spaces are denoted with a hash symbol (#), and
;; pre-filled spaces have a character in place; the whitespace
;; characters are for legibility and should be ignored.
;; For a word to be legally placed on the board:
;; - It may use empty spaces (underscores)
;; - It may use but must not conflict with any pre-filled characters.
;; - It must not use any unusable spaces (hashes).
;; - There must be no empty spaces (underscores) or extra characters
;; before or after the word (the word may be bound by unusable spaces
;; though). 
;; - Characters are not case-sensitive.
;; - Words may be placed vertically (proceeding top-down only), or
;; horizontally (proceeding left-right only).
(defn words [word iv]
  (let [horiz (map #(.replace % " " "") iv)
        vert (map (partial apply str) (apply map vector (map seq horiz)))
        words (mapcat #(clojure.string/split % #"#") (concat horiz vert))
        pwords (filter #(>= (count %) (count word)) words)
        patterns (map #(re-pattern (.replace % "_" ".")) pwords)]
    (boolean (some #(re-find % word) patterns))))

;; 112. Sequs Horribilis
;; Create a function which takes an integer and a nested collection of
;; integers as arguments. Analyze the elements of the input collection
;; and return a sequence which maintains the nested structure, and
;; which includes all elements starting from the head whose sum is
;; less than or equal to the input integer.
(fn [n root]
   (let [upto (fn upto [n s]
                (lazy-seq
                 (when-let [[x & r] (seq s)]
                   (when (>= (- n x) 0)
                     (cons x (upto (- n x) r))))))
         nums (upto n (flatten root))
         walk (fn walk [s]
              (lazy-seq
               (when-let [[x & r] (seq s)]
                 (cons (if (sequential? x)
                         (remove #(or (nil? %) (= '() %))(walk x))
                         (when (some #(= x %) nums) x))
                       (walk r)))))]
     (take-while #(not (nil? %)) (walk root))))

;; 113. Making Data Dance
;; Write a function that takes a variable number of integer arguments.
;; If the output is coerced into a string, it should return a comma
;; (and space) separated list of the inputs sorted smallest to
;; largest. If the output is coerced into a sequence, it should return
;; a seq of unique input elements in the same order as they were
;; entered.
(comment placeholder)

;; 114. Global take-while
;; take-while is great for filtering sequences, but it limited: you
;; can only examine a single item of the sequence at a time. What if
;; you need to keep track of some state as you go over the sequence?
;; Write a function which accepts an integer n, a predicate p, and a
;; sequence. It should return a lazy sequence of items in the list up
;; to, but not including, the nth item that satisfies the predicate.
(fn mytake [n f s]
  (lazy-seq
   (when-let [[c & r] (seq s)]
     (let [m (if (f c) (dec n) n)]
          (when (pos? m)
            (cons c (mytake m f r)))))))

;; 115. The Balance of N
;; A balanced number is one whose component digits have the same sum
;; on the left and right halves of the number. Write a function which
;; accepts an integer n, and returns true iff n is balanced.
(fn [n]
  (let [l (count (str n))
        h (quot l 2)
        [l r] (partition (+ h (rem l 2)) h (map #(- (int %) (int \0)) (str n)))]
    (= (apply + l) (apply + r))))

;; 116. Prime Sandwich
;; A balanced prime is a prime number which is also the mean of the
;; primes directly before and after it in the sequence of valid
;; primes. Create a function which takes an integer n, and returns
;; true iff it is a balanced prime.
(fn sand [n]
  (letfn [(prime? [n p] (every? #(pos? (rem n %)) p))
          (next-prime
             ([n p] (if (prime? n p) (conj p n) (recur (+ n 2) p)))
             ([p] (next-prime (+ (peek p) 2) p)))
          (gen-primes [p] (cons (last p) (lazy-seq (gen-primes (next-prime p)))))]
    (and (> n 3)
      (let [p (into [2] (take-while #(<= % n) (gen-primes [2 3])))
            [r s] (subvec p (- (count p) 2))]
        (and (= s n) (= s (/ (+ r (peek (next-prime p))) 2)))))))

;; 117. For Science!
;; A mad scientist with tenure has created an experiment tracking mice
;; in a maze. Several mazes have been randomly generated, and you've
;; been tasked with writing a program to determine the mazes in which
;; it's possible for the mouse to reach the cheesy endpoint. Write a
;; function which accepts a maze in the form of a collection of rows,
;; each row isa string where:
;; - spaces represent areas where the mouse can walk freely
;; - hashes (#) represent walls where the mouse can not walk
;; - M represents the mouse's starting point
;; - C represents the cheese which the mouse must reach
;; - The mouse is not allowed to travel diagonally in the maze (only
;; up/down/left/right), nor can he escape the edge of the maze. Your
;; function must return true iff the maze is solvable by themouse.
(comment placeholder)

;; 118. Re-implement Map
;; Map is one of the core elements of a functional programming
;; language. Given a function f and an input sequence s, return a lazy
;; sequence of (f x) for each element x in s.
(fn mp [f s]
  (lazy-seq
   (when (seq s)
     (cons (f (first s)) (mp f (rest s))))))

;; 119. Win at Tic-Tac-Toe
;; As in Problem 73, a tic-tac-toe board is represented by a two
;; dimensional vector. X is represented by :x, O is represented by :o,
;; and empty is represented by :e. Create a function that accepts a
;; game piece and board as arguments, and returns a set (possibly
;; empty) of all valid board placements of the game piece which would
;; result in an immediate win.
;; Board coordinates should be as in calls to get-in. For example,
;; [0 1] is the topmost row, center position.
(fn [piece board]
  (let [won? (fn [[x & r]] (and (apply = x r) (not= :e x)))
        winning? (fn [[a b c]]
                   (let [q (into [a b c
                                  [(first a)(second b)(last c)]
                                  [(first c)(second b)(last a)]]
                                 (map vector a b c))]
                     (some won? q)))]
    (set (for [x (range 3)
               y (range 3)
               :when (and (= (get-in board [x y]) :e)
                          (winning? (assoc-in board [x y] piece)))]
           [x y]))))

;; 120. Sum of square of digits
;; Write a function which takes a collection of integers as an
;; argument. Return the count of how many elements are smaller than
;; the sum of their squared component digits. For example: 10 is
;; larger than 1 squared plus 0 squared; whereas 15 is smaller than 1
;; squared plus 5 squared.
(fn check[col]
  (letfn [(sq[n] (reduce + (map #(let [x (- (int %) (int \0))] (* x x)) (str n))))]
    (count (filter #(< % (sq %)) col))))

;; 121. Universal Computation Engine
;; Given a mathematical formula in prefix notation, return a function
;; that calculates the value of the formula. The formula can contain
;; nested calculations using the four basic mathematical operators,
;; numeric constants, and symbols representing variables. The returned
;; function has to accept a single parameter containing the map of
;; variable names to their values.
(fn [form]
  (fn [bmap]
    (let [calc (fn calc
                    ([x] (if (sequential? x) (apply calc x) x))
                    ([op & x] (let [ops {'* *, '/ /, '+ +, '- -}]
                                   (apply (ops op) (map calc x)))))
          formula (clojure.walk/prewalk-replace bmap form)]
      (calc formula))))

;; 122. Read a binary number
;; Convert a binary number, provided in the form of a string, to its
;; numerical value.
#(Integer/parseInt % 2)

;; 124. Analyze Reversi
;; Reversi is normally played on an 8 by 8 board. In this problem, a 4
;; by 4 board is represented as a two-dimensional vector with black,
;; white, and empty pieces represented by 'b, 'w, and 'e,
;; respectively. Create a function that accepts a game board and color
;; as arguments, and returns a map of legal moves for that color. Each
;; key should be the coordinates of a legal move, and its value a set
;; of the coordinates of the pieces flipped by that move.
;; Board coordinates should be as in calls to get-in. For example,
;; [0 1] is the topmost row, second column from the left.
(comment placeholder)

;; 125. Gus' Quinundrum
;; Create a function of no arguments which returns a string that is an
;; exact copy of the function itself. 
(comment placeholder)

;; 127. Love Triangle
;; Everyone loves triangles, and it's easy to understand why—they're
;; so wonderfully symmetric (except scalenes, they suck).
;; Your passion for triangles has led you to become a miner (and
;; part-time Clojure programmer) where you work all day to chip out
;; isosceles-shaped minerals from rocks gathered in a nearby open-pit
;; mine. There are too many rocks coming from the mine to harvest them
;; all so you've been tasked with writing a program to analyze the
;; mineral patterns of each rock, and determine which rocks have the
;; biggest minerals.
;; Someone has already written a computer-vision system for the mine.
;; It images each rock as it comes into the processing centre and
;; creates a cross-sectional bitmap of mineral (1) and rock (0)
;; concentrations for each one. 
;;
;; You must now create a function which accepts a collection of
;; integers, each integer when read in base-2 gives the
;; bit-representation of the rock (again, 1s are mineral and 0s are
;; worthless scalene-like rock). You must return the cross-sectional
;; area of the largest harvestable mineral from the input rock,as
;; follows: 
;; - The minerals only have smooth faces when sheared vertically or
;; horizontally from the rock's cross-section
;; - The mine is only concerned with harvesting isosceles triangles
;; (such that one or two sides can be sheared)
;; - If only one face of the mineral is sheared, its opposing vertex
;; must be a point (ie. the smooth face must be of odd length), and
;; its two equal-length sides must intersect the shear face at 45°
;; (ie. those sides must cut even-diagonally)
;; - The harvested mineral may not contain any traces of rock
;; - The mineral may lie in any orientation in the plane
;; - Area should be calculated as the sum of 1s that comprise the
;; mineral
;; - Minerals must have a minimum of three measures of area to be
;; harvested
;; - If no minerals can be harvested from the rock, your function
;; should returnnil
(comment placeholder)

;; 128. Recognize Playing Cards
;; A standard American deck of playing cards has four suits - spades,
;; hearts, diamonds, and clubs - and thirteen cards in each suit. Two
;; is the lowest rank, followed by other integers up to ten; then the
;; jack, queen, king, and ace.
;; It's convenient for humans to represent these cards as suit/rank
;; pairs, such as H5 or DQ: the heart five and diamond queen
;; respectively. But these forms are not convenient for programmers,
;; so to write a card game you need some way to parse an input string
;; into meaningful components. For purposes of determining rank, we
;; will define the cards to be valued from 0 (the two)to 12 (the ace)
;; Write a function which converts (for example) the string "SJ" into
;; a map of {:suit :spade, :rank 9}. A ten will always be represented
;; with the single character "T", rather than the two characters "10".
(fn [card] (let [ranks (zipmap "23456789TJQKA" (range 13))
                 suits {\S :spade, \H :heart, \D :diamond, \C :club}
                 [s r] (seq card)]
            {:suit (suits s), :rank (ranks r)}))

;; 130. Tree reparenting
;; Every node of a tree is connected to each of its children as well
;; as its parent. One can imagine grabbing one node of a tree and
;; dragging it up to the root position, leaving all connections
;; intact. For example, below on the left is a binary tree. By pulling
;; the "c" node up to the root, we obtain the tree on the right.
;; http://i.imgur.com/UtD2T.png
;; Note it is no longer binary as "c" had three connections total --
;; two children and one parent. Each node is represented as a vector,
;; which always has at least one element giving the name of the node
;; as a symbol. Subsequent items in the vector represent the children
;; of the node. Because the children are ordered it's important that
;; the tree you return keeps the children of each node in order and
;; that the old parent node, if any, is appended on the right. Your
;; function will be given two args -- the name of the node that should
;; become the new root, and the tree to transform. 
(comment placeholder)

;; 131. Sum Some Set Subsets
;; Given a variable number of sets of integers, create a function
;; which returns true iff all of the sets have a non-empty subset with
;; an equivalent summation.
(fn [& s]
  (let [p (fn [s]
              (disj (reduce (fn [s a] (clojure.set/union s (map #(conj % a) s))) #{#{}} s) #{}))
        r (fn [s] (set (map #(apply + %) (p s))))]
    (not (empty? (apply clojure.set/intersection (map r s))))))

;; 132. Insert between two items
;; Write a function that takes a two-argument predicate, a value, and
;; a collection; and returns a new collection where the value is
;; inserted between every two items that satisfy the predicate.
(fn [p v s]
  ((fn ins [p v s1 s2]
    (lazy-seq
     (let [a (seq s1) b (seq s2)]
          (if (and a b)
            (cons (first a)
                  (if (p (first a) (first b))
                    (cons v (ins p v (rest a) (rest b)))
                    (ins p v (rest a) (rest b))))
            a)))) p v s (rest s)))

;; 134. A nil key
;; Write a function which, given a key and map, returns true iff the
;; map contains an entry with that key and its value is nil.
#(= (%2 %1 :not-found) nil)

;; 135. Infix Calculator
;; Your friend Joe is always whining about Lisps using the prefix
;; notation for math. Show him how you could easily write a function
;; that does math using the infix notation. Is your favorite language
;; that flexible, Joe? Write a function that accepts a variable length
;; mathematical expression consisting of numbers and the operations +,
;; -, *, and /. Assume a simple calculator that does not do precedence
;; and instead just calculates left to right.
(fn infix
  ([a] a)
  ([a op b & r] (apply infix (op a b) r)))

;; 137. Digits and bases
;; Write a function which returns a sequence of digits of a
;; non-negative number (first argument) in numerical system with an
;; arbitrary base (second argument). Digits should be represented with
;; their integer values, e.g. 15 would be [1 5] in base 10, [1 1 1 1]
;; in base 2 and [15]in base 16. 
(fn [x b]
  (let [n (if (zero? x) 0 (int (/ (Math/log x) (Math/log b))))]
       (first (reduce (fn [[v r] i]
            (let [bi (apply * (repeat i b))]
                 [(conj v (quot r bi)) (rem r bi)]))
                      [[] x] (range n -1 -1)))))

;; 138. Squares Squared
;; Create a function of two integer arguments: the start and end,
;; respectively. You must create a vector of strings which renders a
;; 45° rotated square of integers which are successive squares from
;; the start point up to and including the end point. If a number
;; comprises multiple digits, wrap them around the shape individually.
;; If there are not enough digits to complete the shape, fill in the
;; rest with asterisk characters. The direction of the drawing should
;; be clockwise, starting from the center of the shape and working
;; outwards, with the initial direction being down and to the right.
(comment placeholder)

;; 140. Veitch, Please!
;; Create a function which accepts as input a boolean algebra function
;; in the form of a set of sets, where the inner sets are collections
;; of symbols corresponding to the input boolean variables which
;; satisfy the function (the inputs of the inner sets are conjoint,
;; and the sets themselves are disjoint... also known as canonical
;; minterms). Note: capitalized symbols represent truth, and
;; lower-case symbols represent negation of the inputs. Your function
;; must return the minimal function which is logically equivalent to
;; the input.
(comment placeholder)

;; 141. Tricky card games
;; In trick-taking card games such as bridge, spades, or hearts, cards
;; are played in groups known as "tricks" - each player plays a single
;; card, in order; the first player is said to "lead" to the trick.
;; After all players have played, one card is said to have "won" the
;; trick. How the winner is determined will vary by game, but
;; generally the winner is the highest card played in the suit that
;; was led. Sometimes (again varying by game), a particular suit will
;; be designated "trump", meaning that its cards are more powerful
;; than any others: if there is a trump suit, and any trumps are
;; played, then the highest trump winsregardless of what was led.
;;
;; Your goal is to devise a function that can determine which of a
;; number of cards has won a trick. You should accept a trump suit,
;; and return a function winner. Winner will be called on a sequence
;; of cards, and should return the one which wins the trick. Cards
;; will be represented in the format returned by Problem 128,
;; Recognize Playing Cards: a hash-map of :suit and a numeric :rank.
;; Cards with a largerrank are stronger.
(fn [trump]
  (fn [c]
    (let [lead (:suit (first c))
          cmp (fn [{s1 :suit r1 :rank}{s2 :suit r2 :rank}]
                  (let [sorder (assoc {lead 1} trump 2)
                        sc (compare (sorder s1 0) (sorder s2 0))]
                       (if (zero? sc) (compare r1 r2) sc)))]
      (last (sort cmp c)))))

;; 143. dot product
;; Create a function that computes the dot product of two sequences.
;; You may assume that the vectors will have the same length.
#(reduce + (map * % %2))

;; 144. Oscilrate
;; Write an oscillating iterate: a function that takes an initial
;; value and a variable number of functions. It should return a lazy
;; sequence of the functions applied to the value in order, restarting
;; from the first function after it hits the end.
(fn [i & fs]
  ((fn os [i cf fs]
    (let [f (first cf)
          rf (rest cf)]
      (cons i (lazy-seq (os (f i) (if (seq rf) rf fs) fs))))) i fs fs))

;; 146. Trees into tables
;; Because Clojure's for macro allows you to "walk" over multiple
;; sequences in a nested fashion, it is excellent for transforming all
;; sorts of sequences. If you don't want a sequence as your final
;; output (say you want a map), you are often still best-off using
;; for, because you can produce a sequence and feed it into a map,for
;; example.
;; For this problem, your goal is to "flatten" a map of hashmaps. Each
;; key in your output map should be the "path"1 that you would have to
;; take in the original map to get to a value, so for example
;; {1 {2 3}} should result in {[1 2] 3}. You only need to flatten one level
;; of maps: if one of the values is a map, just leave it alone.
;; 
;; (1) That is, (get-in original [k1 k2]) should be the same as (get result [k1 k2])
(fn [m]
  (into {}
        (for [[k r] m
              [l v] r]
          [[k l] v])))

;; 147. Pascal's Trapezoid
;; Write a function that, for any given input vector of numbers,
;; returns an infinite lazy sequence of vectors, where each next one
;; is constructed from the previous following the rules used in
;; Pascal's Triangle. For example, for [3 1 2], the next row is [3 4 3 2].
(fn [row]
  (let [f (fn [row] (conj (first (reduce (fn[[r l] x] [(conj r (+ l x)) x]) [[] 0N] row))
                          (last row)))]
    (iterate f row)))

;; 148. The Big Divide
;; Write a function which calculates the sum of all natural numbers
;; under n (first argument) which are evenly divisible by at least one
;; of a and b (second and third argument). Numbers a and b are
;; guaranteed to becoprimes.
;; Note: Some test cases have a very large n, so the most obvious
;; solution will exceed the time limit.
(fn [n a b]
  (let [quotc (fn [n x] (quot (dec n) x))
        sum (fn [n a] (*' n (inc n) a 1/2))
        x (sum (quotc n a) a)
        y (sum (quotc n b) b)
        z (sum (quotc n (* a b)) (* a b))]
    (+ (- x z) y)))

;; 150. Palindromic Numbers
;; A palindromic number is a number that is the same when written
;; forwards or backwards (e.g., 3, 99, 14341).
;; Write a function which takes an integer n, as its only argument,
;; and returns an increasing lazy sequence of all palindromic numbers
;; that are not less than n.
;; The most simple solution will exceed the time limit
(comment placeholder)

;; 152. Latin Square Slicing
;; Our aim is to implement a function which accepts a vector of
;; vectors V as an argument, and returns a map which keys and values
;; are integers. Each key should be the order of a Latin square
;; included in V, and its value a count of different Latin squares of
;; that order included in V. If V does not include any Latin squares
;; an empty map should be returned. In the previous example the
;; correct output of such a function is {3 1, 2 1} and not {3 1, 2 3}.
;; http://www.4clojure.com/problem/152
(comment placeholder)

;; 153. Pairwise Disjoint Sets
;; Given a set of sets, create a function which returns true if no two
;; of those sets have any elements in common1 and false otherwise.
;; Some of the test cases are a bit tricky, so pay a little more
;; attention to them.
;; (1) Such sets are usually called pairwise disjoint or mutually
;; disjoint.
(fn [s]
  (empty? (for [x s, y s
                :let [i (clojure.set/intersection x y)]
                :while (not= x y)
          	:when (not-empty i)]
            i)))

;; 156. Map Defaults
;; When retrieving values from a map, you can specify default values
;; in case the key is not found:
;; (= 2 (:foo {:bar 0, :baz 1} 2))
;; However, what if you want the map itself to contain the default
;; values? Write a function which takes a default value and a sequence
;; of keys and constructsa map.
(fn [v m] (reduce #(assoc % %2 v) {} m))

;; 157. Indexing Sequences
;; Transform a sequence into a sequence of pairs containing the
;; original elements along with their index.
(fn [c] (map-indexed #(vector %2 %) c))

;; 158. Decurry
;; Write a function that accepts a curried function of unknown arity
;; n. Return an equivalent function of n arguments. 
(fn [f]
  (partial
   (fn [f & args]
     (let [g (f (first args))]
          (if (fn? g)
            (recur g (rest args))
            g)))
   f))

;; 164. Language of a DFA
;; A deterministic finite automaton (DFA) is an abstract machine that
;; recognizes a regular language. Usually a DFA is defined by a
;; 5-tuple, but instead we'll use a map with 5 keys:
;; - :states is the set of states for the DFA.
;; - :alphabet is the set of symbols included in the language recognized
;; by the DFA.
;; - :start is the start state of the DFA.
;; - :accepts is the set of accept states in the DFA.
;; - :transitions is the transition function for the DFA, mapping
;; :states ⨯ :alphabet onto :states.
;; - Write a function that takes as input a DFA definition (as
;; described above) and returns a sequence enumerating all strings in
;; the language recognized by the DFA. Note: Although the DFA itself
;; is finite and only recognizes finite-length strings it can still
;; recognize an infinite set of finite-length strings. And because
;; stack space is finite, make sure you don't get stuck in an infinite
;; loop that's not producing results every so often!
(comment placeholder)

;; 166. Comparisons
;; For any orderable data type it's possible to derive all of the
;; basic comparison operations (<, ≤, =, ≠, ≥, and >) from a single
;; operation (any operator but = or ≠ will work). Write a function
;; that takes three arguments, a less than operator for the data and
;; two items to compare. The function should return a keyword
;; describing the relationship between the two items. The keywords for
;; the relationship between x and y are as follows:
;; - x = y → :eq
;; - x > y → :gt
;; - x < y → :lt
#(cond (% %2 %3) :lt
        (% %3 %2) :gt
        :else :eq)

;; 168. Infinite Matrix
;; In what follows, m, n, s, t denote nonnegative integers, f denotes
;; a function that accepts two arguments and is defined for all
;; nonnegative integers in both arguments.
;; In mathematics, the function f can be interpreted as an infinite
;; matrix with infinitely many rows and columns that, when written,
;; looks like an ordinary matrix but its rows and columns cannot be
;; written down completely, so are terminated with ellipses. In
;; Clojure, such infinite matrix can be represented as an infinite
;; lazy sequence of infinite lazy sequences, where the inner sequences
;; represent rows.
;; Write a function that accepts 1, 3 and 5 arguments
;; - with the argument f, it returns the infinite matrix A that has
;; the entry in the i-th row and the j-th column equal to f(i,j) for
;; i,j = 0,1,2,...;
;; - with the arguments f, m, n, it returns the infinite matrix B that
;; equals the remainder of the matrix A after the removal of the first
;; m rows and the first n columns;
;; - with the arguments f, m, n, s, t, it returns the finite s-by-t
;; matrix that consists of the first t entries of each of the first s
;; rows of the matrix B or, equivalently, that consists of the first s
;; entries of each of the first t columns of thematrix B.
(comment placeholder)

;; 171. Intervals
;; Write a function that takes a sequence of integers and returns a
;; sequence of "intervals". Each interval is a a vector of two
;; integers, start and end, such that all integers between start
;; and end (inclusive) are contained in the input sequence.
#(->> %
      sort
      ((fn i [[xf & xr :as s]]
         (let [maxi (fn [n s]
                      (if-let [[c & r] (seq s)]
                        (if (>= (inc n) c) (recur c r) [n s])
                        [n s]))
               [x r] (when-let [[s1 r1] (maxi xf xr)]
                       [[xf s1] r1])]
           (if (seq r)
             (conj (i r) x)
            (if (nil? (first x)) [] (list x)))))))

;; 173. Intro to Destructuring 2
;;Sequential destructuring allows you to bind symbols to parts
;; of sequential things (vectors, lists, seqs, etc.):
;; (let [bindings* ] exprs*) Complete the bindings so all let-parts
;; evaluate to 3.
(= 3
  (let [[op v] [+ (range 3)]] (apply op v))
  (let [[[op v] b] [[+ 1] 2]] (op v b))
  (let [[op v] [inc 2]] (op v)))

;; 177. Balancing Brackets
;; When parsing a snippet of code it's often a good idea to do a sanity
;; check to see if all the brackets match up. Write a function that takes
;; in a string and returns truthy if all square [ ] round ( ) and curly { }
;; brackets are properly paired and legally nested, or returns falsey otherwise.
(fn [s]
  (let [special {\) \( \] \[ \} \{}
        is-close? #(special %)
        is-open? #((set (vals special)) %)
        matching? (fn[c o] (= (special o) c))
        bm (fn[s stack]
             (if-let [c (first s)]
               (cond
                 (is-open? c) (recur (rest s) (conj stack c))
                 (is-close? c)
                   (when (matching? (first stack) c)
                     (recur (rest s) (rest stack)))
                 :else (recur (rest s) stack))
               (empty? stack)))]
    (bm s '())))

;; 178. Best Hand
;; Following on from Recognize Playing Cards, determine the best poker hand
;; that can be made with five cards. The hand rankings are listed below for
;; your convenience.
;;
;; Straight flush: All cards in the same suit, and in sequence
;; Four of a kind: Four of the cards have the same rank
;; Full House: Three cards of one rank, the other two of another rank
;; Flush: All cards in the same suit
;; Straight: All cards in sequence (aces can be high or low, but not both at once)
;; Three of a kind: Three of the cards have the same rank
;; Two pair: Two pairs of cards have the same rank
;; Pair: Two cards have the same rank
;; High card: None of the above conditions are met
(fn [cards]
  (let [ranks (sort (map #((zipmap "23456789TJQKA" (range 13)) (second %)) cards))
        np (fn [n o] (= o (count (filter #(= n %) (vals (frequencies ranks))))))
        straight (or (= ranks (range (first ranks) (+ (first ranks) 5)))
                     (= ranks [0 1 2 3 12]))
        flush (apply = (map first cards))]
    (cond
     (and straight flush) :straight-flush
     (np 4 1) :four-of-a-kind
     (and (np 3 1) (np 2 1)) :full-house
     flush :flush
     straight :straight
     (np 3 1) :three-of-a-kind
     (np 2 2) :two-pair
     (np 2 1) :pair
     :else :high-card)))
