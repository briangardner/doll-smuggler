;Solution to Doll Smuggling Problem
;Forked from: https://github.com/micahalles/doll-smuggler
;Started: 21-MAY-2012
;Finished: ??

;structure of the doll input
(defstruct Doll :name :weight :value)

;doing a quick declare here so I can reference it in my pick-dolls function
;this is memoized later
(declare in-memory-pick-dolls)

;Knapsack algorithm
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
            [dolls_if_skipping value_if_skipping]
            )
          )
        )
      )
    )
  )

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
  (get-input "Input Max Weight the mule can carry"))

(defn get-dealer-inventory 
  "Get dealer inventory of dolls in the format of [Name Weight Value Name2 Weight2 Value2...]"
  []
  (get-input "Input Dealer Inventory")
  )

(defn fill-knapsack
  "Fill knapsack based on dolls from dealer"
  [dolls max-weight]
  (let [num_of_dolls (count dolls)
        results (in-memory-pick-dolls dolls (- num_of_dolls 1) max-weight)]
    (println "Results:")
    (doseq [result results] (prn result))
    )
)

(defn run-solution[]
   (let [old-woman (get-max-weight)
         inventory (get-dealer-inventory)
         dealer-inventory (vec (map #(apply struct Doll %)  (partition 3 inventory )))
         ]
     (fill-knapsack dealer-inventory old-woman)
  ))
  
(run-solution) ;Run the solution

