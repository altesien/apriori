; imports
(ns test-0.core
  (:gen-class))
(use '[clojure.string :only (join split)])
(use '[clojure.set :only (union intersection)])
(use 'clojure.walk)

(defn hash-vec
  "splits strings into vector of ints" [#^String bucket]
  (map read-string (split bucket #"\s")))

;Apriori Exclusive
(defn zip-line
  "hashes strings into into map" [input line-num]
  (#(apply assoc{}(interleave %2(repeat #{%}))) line-num input))

;PCY Exclusive
(defn freq-filter
  "filters each input line by the most frequent items"
  [current freq]
  (keep (into #{} current) (into #{}freq)))

;PCY Exclusive
(defn create-bitmap
  "creates a bitmap where the hashed value is the key"
  [coll]
  (reduce #(bit-set %1 (- (first %2) 1)) 0 coll))

;PCY Exclusive
(defn count-of
  "count of each item"
  [input]
  (#(apply assoc{}(interleave % (repeat 1))) input))

;Apriori Exclusive
(defn pass-2-apriori
  "Second pass of Apriori
   compares the locations of the frequent items
   and counts the amount of overlap
   printing out the values"
  [coll thresh]
    (if (empty? coll)
      (println "Done")
      (let [fir (second (first coll))
            re (walk second #(into [] %) (rest coll))
            current (first (first coll))]
              (dotimes [n (count re)]
                (let [filtered-val (count (intersection fir (nth re n)))]
                  (if (>= filtered-val thresh)
                      (println current "and"
                        (first (nth (rest coll) n))
                        "intersect"
                        filtered-val
                        "times"))))
              (recur (rest coll) thresh))))

;Apriori
(defn apriori
  "Takes in the list and the threshold
   counts all the frequent items and locations and
   passes them to the second pass function"
  [coll thresh]
  (let [x (filter #(>= (count (second %)) thresh)
        (reduce
          #(merge-with union %1 %2){}
            (map zip-line (map hash-vec coll) (range))))]
                (pass-2-apriori x thresh)))

;PCY Exclusive
(defn pair-up
  "pairing function for PCY
   pairs and hashes each pair to a bucket
   the hash becomes a key
   returns a map of keys and the number of times the key was generated"
  [input pair-bucket]
  (if (empty? input)
    pair-bucket
    (let [first-element (first input)
          rest-elements (rest input)
          merge-bucket pair-bucket]
        (recur rest-elements
          (reduce #(merge-with + %1 %2) merge-bucket
            (map #(assoc {} (mod (bit-xor first-element %) 1000)1) rest-elements)))
  )))

;PCY Exclusive
(defn pass-two-2
  "Pass two of PCY
   Takes in the bitmap an empty map and a list of
   filtered values based on threshold
   compares the hash of each value to the bitmap
   to determine if it is a correct pair
   if so it pairs them in a map with a count and returns the map"
  [bit-map build coll]
  (if (empty? coll)
      build
      (let [current (first coll)
            the-rest (rest coll)]
            (let [filterd (filter #(bit-test bit-map (- (mod (bit-xor current %) 1000)1)) the-rest)]
                 (recur
                   bit-map
                   (reduce #(merge-with + %2 %1) build (map #(assoc {} (list current %) 1) filterd))
                   the-rest)))))


(defn pcy
  "Takes in the list and the threshold
   completes the first pass to find frequent items
   creates the bitmap and the runs the second pass"
  [coll thresh]
  (let [collect (pmap hash-vec coll)]
       (let [x (filter #(>= (second %) thresh)
            (reduce #(merge-with + %1 %2){}
              (map count-of collect)))]
            (let [have (create-bitmap (filter #(>= (second %) thresh)
                 (reduce #(merge-with + %1 %2) {} (map pair-up collect (repeat {})))))
                   list-ok (walk first #(into () %)x)]
            (println (filter #(>= (second %) thresh)
              (reduce #(merge-with + %1 %2) {}
                (map #(pass-two-2 have {} (freq-filter % list-ok)) collect))))))))



(defn -read-file
  "read the input file" [file thresh]
  (with-open [rdr (clojure.java.io/reader file)]
            (doall (apriori (line-seq rdr) thresh))))

(defn -main "soon"
  [& args]
  (time (-read-file (first args) (read-string (second args))))
  )
