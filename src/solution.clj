;Solution to Doll Smuggling Problem
;Written By: Brian Gardner
;Forked from: https://github.com/micahalles/doll-smuggler
;Started: 21-MAY-2012
;Finished: 23-MAY-2012

(ns dollsmuggler
  (:use clojure.test))

;structure of the doll input
(defstruct Doll :name :weight :value)

;doing a quick declare here so I can reference it in my pick-dolls function
;this is memoized later
(declare in-memory-pick-dolls)

;Knapsack algorithm
;derived from http://en.wikipedia.org/wiki/Knapsack_problem#Dynamic_Programming_Algorithm
(defn pick-dolls
  "Knapsack algorithm"
 [dolls max-weight]
  (if (or (>= 0 max-weight) (= 0 (count dolls))) ;if we're at our weight is at or below zero, or we have no elements
    [[] 0] ; return an empty set and 0
    (let [doll (first dolls)
          weight (get doll :weight)
          value (get doll :value)]
      (if (> weight max-weight)
        (in-memory-pick-dolls (rest dolls) max-weight) ;skip, doll too heavy
        (let [[dolls_skip value_skip]
              (in-memory-pick-dolls (rest dolls) max-weight) ;lookup with current weight
              [dolls_keep value_keep]
              (in-memory-pick-dolls (rest dolls) (- max-weight weight))] ;lookup without current doll's weight
          (if (> (+ value_keep value) value_skip)
            [(conj dolls_keep doll) (+ value_keep value)] ;add current doll
            [dolls_skip value_skip])))))) ;return previous best doll

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
  (get-input "Input Dealer Inventory in the format of [Name Weight Value Name2 Weight2 Value2...]"))

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
    (if(number? max-weight)
      (let [dolls (get-vector dolls)]
        (in-memory-pick-dolls dolls max-weight))
      (do (println "Input max weight is not numeric")))
    (do (println "Input doll data is not a vector or max weight is not integer "))))

;Run this to input your own data.
(defn run-solution[]
   (
     let [grandma (get-max-weight)
         inventory (get-dealer-inventory)
         results (fill-knapsack inventory grandma)]
     (println "Results:")
     (println "Dolls selected: ")
     (println "Name \t\t Weight \t\t Value")
     (doseq [result (first results)]
       (println result))
     (println "\nTotal value: " (last results))
  ))

;Test Suite
(deftest test-suite[]
    
    (let [good-test-weight 50
          good-test-dolls ['Brian 20 10]
          expected-good-value 10
          large-test-data ['luke        9   150
                           'anthony    13    35
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
          large-test-weight 400
          large-expected-data ['sally       4    50
                               'eddie       7    20
                               'grumpy     22    80
                               'dusty      43    75
                               'grumpkin   42    70
                               'marc       11    70
                               'randal     27    60
                               'puppy      15    60
                               'dorothy    50   160
                               'candice   153   200
                               'anthony    13    35
                               'luke        9   150]
          large-expected-value 1030
          zero-expected-count 0
          empty-results-set [[] 0]
          
      ]
      (testing "Test suite"
        (testing "Testing"
             (is (= 1 1))) ;silly test, but a sanity check and  making sure that I can call testing
        (testing "Test with small set of good data"
                 (is (= [(get-vector good-test-dolls) expected-good-value] (fill-knapsack good-test-dolls good-test-weight))))
        (testing "Test with large amount of data"
                 (is(= [(get-vector large-expected-data) large-expected-value]
                       (fill-knapsack large-test-data large-test-weight))))
        (testing "Test with negative weight"
                 (is(= [[] 0] (fill-knapsack good-test-dolls -4))))
        (testing "Test with non-vector input"
                 (is(= nil (fill-knapsack -1 123))))
        (testing "Test with empty set and 0 weight"
                 (is(= empty-results-set (fill-knapsack [] 0)))) 
        (testing "Test with non-number input"
                 (is(= nil (fill-knapsack good-test-dolls "abc")))))))

(run-solution)