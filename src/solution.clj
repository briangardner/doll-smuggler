;Solution to Doll Smuggling Problem
;Forked from: https://github.com/micahalles/doll-smuggler
;Started: 21-MAY-2012
;Finished: ??

;structure of the doll input
(ns dollsmuggler
  (:use clojure.test))
(defstruct Doll :name :weight :value)

;doing a quick declare here so I can reference it in my pick-dolls function
;this is memoized later
(declare in-memory-pick-dolls)

;Knapsack algorithm
;Recursively calls itself.  If the 
(defn pick-dolls
  "Knapsack algorithm"
 [dolls index max-weight]
  (
    if (or (= 0 max-weight) (< index 0))
    [[] 0]
    (let [doll (get dolls index)
          weight (get doll :weight)
          value (get doll :value)]
      (if (> weight max-weight)
        (in-memory-pick-dolls dolls (- index 1) max-weight)
        (let [[dolls_if_skipping value_if_skipping]
              (in-memory-pick-dolls dolls (- index 1) max-weight)
              [dolls_if_keeping value_if_keeping]
              (in-memory-pick-dolls dolls (- index 1) (- max-weight weight))]
          ; note that dolls_if_keeping and value_if_keeping do not yet include
          ; the current doll
          (if (> (+ value_if_keeping value) value_if_skipping)
            ; keep the doll
            [(conj dolls_if_keeping doll) (+ value_if_keeping value)]
            ; skip the doll
            [dolls_if_skipping value_if_skipping]))))))

;cache the previous runs of pick-dolls in memory
;takes up more RAM, but saves processing.
;http://clojuredocs.org/clojure_core/clojure.core/memoize
(def in-memory-pick-dolls (memoize pick-dolls)) 

(defn get-input 
  "Generic function to get user input from the console"
  [prompt]
  (println prompt ":")
  (try
    (read-string (read-line))
    (catch Exception e nil)))

(defn get-max-weight
  "Get maximum weight the mule can carry"
  []
  (get-input "Input Max Weight the mule can carry (integer values only please)"))

(defn get-dealer-inventory 
  "Get dealer inventory of dolls in the format of [Name Weight Value Name2 Weight2 Value2...]"
  []
  (get-input "Input Dealer Inventory"))

;Helper method to return a vector in the format of the struct Doll
(defn get-vector
  "Returns a vector of dolls"
  [dolls]
  (vec (map #(apply struct Doll %) (partition 3 dolls))))

;Kicks off the process
(defn fill-knapsack
  "Fill knapsack based on dolls from dealer and maximum weight"
  [dolls max-weight]
  (if(vector? dolls)
    (let [dolls (get-vector dolls)
          num_of_dolls (count dolls)]
        (in-memory-pick-dolls dolls (- num_of_dolls 1) max-weight))
    (do (println "Input doll data is not a vector or max weight is not integer "))
    )
)

;Run this to input your own data.
(defn run-solution[]
   (
     ;let [good-test-weight 50
     ;     good-test-dolls ["Brian" 20 10]
     ;     results (fill-knapsack good-test-dolls good-test-weight)]
     let [old-woman (get-max-weight)
         inventory (get-dealer-inventory)
         results (fill-knapsack inventory old-woman)]
     (println "Results:")
     (println "Dolls selected: ")
     (doseq [result (first results)] 
       (println result))
     (println "Total value: " (last results))
  ))

;Test Suite
(deftest test-suite[]
    
    (let [good-test-weight 50
          good-test-dolls ['Brian 20 10]
          large-test-data [ 'luke 9 150 
                            'anthony 13 35
                            'candice   153   200
														'dorothy    50   160
														'puppy      15    60
														'thomas     68    45
														'randal     27    60
														'april      39    40
														'nancy      23    30
														'bonnie     52    10
														'marc       11    70
														'kate       32    30
														'tbone      24    15
														'tranny     48    10
														'uma        73    40
														'grumpkin   42    70
														'dusty      43    75
														'grumpy     22    80
														'eddie       7    20
														'tory       18    12
														'sally       4    50
														'babe       30    10]
          expected-good-value 10
      ]
      (testing "Test suite"
        ;testing to make sure true is true
        ;silly test, but just making sure that I can call testing
        (testing "Testing"
             (is (= 1 1)))
        ;simple good data test
        (testing "Test with small set of good data"
               (is (= expected-good-value (last (fill-knapsack good-test-dolls good-test-weight))))
               (is (= 1 (count(first(fill-knapsack good-test-dolls good-test-weight)))))
                    )
        (testing "Test with large amount of data"
                 (is(= 1030 (last(fill-knapsack large-test-data 400))))
                 (is(= 12 (count(first(fill-knapsack large-test-data 400))))))
        (testing "Test with negative weight"
                 (is(= 0 (last(fill-knapsack good-test-dolls -4)))))
        (testing "Test with non-vector input"
                 (is(= nil (fill-knapsack -1 123))))
        (testing "Test with non-number input"
                 (is(= 123 (fill-knapsack good-test-dolls "abc"))))
      )
      
    ))



;(test-suite)
;(run-solution) ;Run the solution

