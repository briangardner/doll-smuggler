;Solution to Doll Smuggling Problem
;Forked from: https://github.com/micahalles/doll-smuggler
;Started: 21-MAY-2012
;Finished: ??
;Knapsack algorithm came from: http://pparkkin.info/2010/02/20/knapsack-in-clojure-old-school-version/

(defrecord Doll [Name Weight Value])
(defrecord Mule [MaxWeight])
(defprotocol Fill
  "Fill container"
  (fill [this] "Container Filled!"))
(extend-type Mule
               Fill
               (fill [this] (str "Purse Filled")))

(defn get-input 
  "Generic function to get user input from the console"
  [prompt]
  (println prompt)
  (try
    (read-string (read-line))
    (catch Exception e nil)))

(defn get-max-weight
  "Get maximum weight the mule can carry"
  []
  (get-input "Input Max Weight the Mule can carry"))

(defn run-solution[]
  (let [old-woman (Mule. (get-max-weight))]
  (println "Max Weight: " (:MaxWeight old-woman))))
  

(run-solution)