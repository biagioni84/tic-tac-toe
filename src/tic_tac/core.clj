(ns tic-tac.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; TODO: use https://github.com/mikera/core.matrix
(defn columns [matrix]
   (apply (partial map list) matrix))

(defn is-square [matrix]
  (let [col-num (map count matrix)
        complete (apply = col-num)
        is-square (if complete (= (count col-num) (first col-num)) false)
        ]
    is-square)
  )

(defn diagonals [matrix]
  (let [is-square (is-square matrix)]
    (if is-square
      [(reverse (reduce
                  (fn [val col]
                    (conj val (nth col (count val)))
                    )
                  nil matrix)
                )
       (reduce
         (fn [val col]
           (conj val (nth col (count val)))
           )
         nil (reverse matrix))]
      )
    ))
(defn line-winner [line]
  (if (apply = line) (first line) \?)
  )
(defn winner [board]
  (let [h (apply list board)
        v  (into () (columns board))
        [d1 d2] (diagonals board)
        all (conj (concat v h) d1 d2)
        ]
    (first (filter #(not= % \?)
            (map line-winner all)
                   ))
    ))

(comment
  ;(println (type h))
  ;(println (type v))
  ;(println (type d1))
  ;(println (type d2))
  ;(println h)
  ;(println v)
  ;(println d1)
  ;(println d2)
  ;(println all)

  (rseq matrix)
  (filter #(not= % \?)
          (\x \? \? \? \? \? \? \?))
  (#(not= % \?) \x)
  (winner matrix2)
  (winner [[\o \o \x]
           [\x \o \?]
           [\x \? \o]])
  (apply conj (diagonals matrix2))
  (type (diagonals matrix2))
  (columns matrix2)
  (is-square matrix)
  (cons '(\x \o) \o)
  (is-square [[1 2][3 4][5 6]])

  (count (map count matrix))
  (map count [[1 2][3 4][5 6]])
  (def matrix [[:x :o :?]
               [:x :o :?]
               [:x :? :?]])
  (def matrix [[\x \o \x]
               [\x \x \?]
               [\x \? \?]])
  (def matrix2 [[\o \o \x]
           [\x \x \?]
           [\x \? \?]])
  (apply #(map list %1 %2 %3) matrix)
  (apply #(map list %1 %2 %3) [[1 2][3 4][5 6]])
  (map + [1 2][3 4])
  (apply (fn [a b c] (list (first a) (first b) (first c))) matrix)

  (def row [:x :o :?])
  row
  matrix

  (line-winner [\x  \x  \?])
  (line-winner [\? \? \?])
  (line-winner [\x \x \x])
  (line-winner [\? \x  \?])
  )
