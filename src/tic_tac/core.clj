(ns tic-tac.core
  (:refer-clojure :exclude [* - + == / < <= > >= not= = min max])
  (:require [clojure.string :as str]
            [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :refer :all]
            )
  (:gen-class))


;; TODO: use https://github.com/mikera/core.matrix
(defn cols [matrix]
  "get list of columns in matrix"
   (apply (partial map list) matrix))

(defn is-square [matrix]
  "check if matrix is square"
  (let [col-num (map count matrix)
        complete (apply = col-num)
        is-square (if complete (= (count col-num) (first col-num)) false)
        ]
    is-square)
  )
;; jp
;(defn is-square [matrix]
;  (apply = (conj (map count matrix) (count matrix))))

(defn diagonals [matrix]
  "extract diagonals from matrix"
  (let [index-range (range (count matrix))
        f (fn [i v] (get v i))]
    [(map f index-range matrix)
     (map f (reverse index-range) matrix)]))

(defn line-winner [line]
  "check if someone wins in this line"
  (if (apply = line) (first line) \?)
  )


(defn line-winable [line]
  "check winning opportunities in this line"
  (let [freqs (frequencies line)]
    (if (= (get freqs \?) 1)
      (first (first (filter #(= 2 (val %)) freqs)))
      )
    )
  )

(let [freqs (frequencies [\x \o \x])]
  (if (= (get freqs \?) 1)
    (first (first (filter #(= 2 (val %)) freqs)))
    )
  )
(get (frequencies [\x \? \x]) \?)
(defn get-lines [board]
  "extracts ascending diagonal, desc diag, columns right to left, rows top to bottom
  (we dont need order for playing tic tac toe)"
  (let [h (apply list board)
        v  (into () (cols board))
        [d1 d2] (diagonals board)
        all (conj (concat v h) d1 d2)
        ]
    all (conj (concat v h) d1 d2)
    ))
;(defn winner [plays]
;  (first (filter #(not= % \?)
;                 (map line-winner plays)
;                 ))
;  )
(defn winables [board]
          (filter #(and (not= % \?) (not= % nil) (not= % '()))
                   (map line-winable (get-lines board))
                 )
  )

(defn winner [board]
  (filter #(not= % \?) (map line-winner (get-lines board)))
  )
(defn find-with-val [board val]
  "finds coords of elements with value val on the board"
  (let [indexed-rows (map-indexed (fn [i v] [i v])  board)]
    (reduce concat (filter not-empty (map (fn [[row-index row-val]]
                             (filter not-empty (map-indexed (fn [i v] (if (= v val) [ row-index i ])) row-val))
                             ) indexed-rows))
    ))
  )
;(defn find-with-val [board val]
;  "create all posible scenarios for next move"
;  (let [indexed-rows (map-indexed (fn [i v] [i v])  matrix)]
;    (map (fn [[row-index row-val]]
;           (map-indexed (fn [i v] [(+ (* row-index (count row-val)) i) (= v val)]) row-val)
;           ) indexed-rows)
;    )
;  )
(defn apply-action [action board [r c]]
  (assoc board r (assoc (nth board r) c action))
  )

(defn future-boards [action board condition]
  (let [available-coords (find-with-val board condition)]
    (map (fn [n] [n (apply-action action board n)]) available-coords)
    ))
(defn map-row [row-index row]
  (map-indexed
    (fn [i v]
      [[row-index i] v])
    row)
  )

(defn index-board [board]
  (apply concat (map-indexed
                  map-row
                  board))
  )

;(rand-nth (find-with-val @board \?))
(defn first-moves [board player opponent]
  "center/opposite corner/empty corner/empty side"
  (let [indexed (index-board board)
        first-move (= (count (find-with-val board \?)) 9)
        size (count board)
        max (- size 1)
        med (/ max 2)
        center (first (filter #(and (= (first %) [med med]) (= (second %) \?) ) indexed))
        empty-corners (map #(first %) (filter #(and (or (= (first %) [0 0])
                                                        (= (first %) [0 max])
                                                        (= (first %) [max 0])
                                                        (= (first %) [max max])
                                                        )
                                                    (= \? (second %)))
                                              indexed))
        [row col :as opponent-corner] (some not-empty (map #(first %) (filter #(and (or (= (first %) [0 0])
                                                                               (= (first %) [0 max])
                                                                               (= (first %) [max 0])
                                                                               (= (first %) [max max])
                                                                               )
                                                                           (= opponent (second %)))
                                                                     indexed)))
        opposite-corner (if opponent-corner (remove #(or (= % [(if (zero? row) max 0) (if (zero? col) max 0)])
                                     (= % [row col]))
                                empty-corners))
        ;[(if (zero? row) max 0) (if (zero? col) max 0)]
        empty-sides (map #(first %) (filter #(and (or (= (first %) [0 med])
                                 (= (first %) [med 0])
                                 (= (first %) [med max])
                                 (= (first %) [max med])
                                 )
                                  (= \? (second %)))
                            indexed))
        ]
    ;; if first move use a corner to increase opponent error prob
    (println center)
    (if first-move
      (rand-nth [[0 0] [0 max] [max 0] [max max]])
      (if center
        ;(println "center")
        (first center)
        ;empty-corners
        (if (not-empty opposite-corner)
          (rand-nth opposite-corner)
          (if (not-empty empty-corners)
            (rand-nth empty-corners)
            (rand-nth empty-sides)
            )
          )
        )
      )
    )
  )

(def current-player (atom nil))
(def one (atom true))
(defn utility [state]
  ;(println state)
  (let [winner (first (winner (second state)))
        end-of-game (= (count (find-with-val (second state) \?)) 0)]
    (if winner
      (if (= winner @current-player)
        10
        -10)
      (if end-of-game
        0 )
      )
    )
  )
(defn min-value-pmap [state])
(defn max-value-pmap [state]
  (let [util (utility state)
        my-move (first state)]
    ;(println "max")
    ;(if @one (println state))
    (reset! one false)

    (if util
      [(first state) util]
      (let [vals (remove nil? (pmap min-value-pmap (future-boards @current-player (second state) \?)))
            k (if (not-empty vals) (apply max-key second vals))
            ]
        ;(println "max-vals")
        ;(println vals) ;; mayor q 1 o falla el max-key
        ;(println k)
        (if k
          (if (first state) [(first state) (second k)] k)
          (println "---------- NO K ----------");(into () [[my-move -100]])
          )
        )
      )
    )
  ;; check if terminal and return score
  ;; calculate succesor states
  ;; find max between -inf and min-value (s)
  ;; return max value
  )


(defn min-value-pmap [state]
  (let [util (utility state)
        my-move (first state)]
    ;(println "min")
    ;(println state)
    (if util
      [(first state) util]
      (let [vals (remove nil? (pmap max-value-pmap (future-boards (if (= @current-player \x) \o \x) (second state) \?)))
            k (if (not-empty vals) (apply min-key second vals))
            ]
        ;(println "min-vals")
        ;(println vals) ;; mayor q 1 o falla el max-key
        ;(println k)
        (if k
          (if (first state) [(first state) (second k)] k)
          (println "---------- NO K ----------");(into () [[my-move -100]])
          )
        )
      )
    )
  ;; check if terminal and return score
  ;; calculate succesor states
  ;; find min between +inf and max-value (s)
  ;; return min value
  )

(defn min-value-ideep [state level])
(defn max-value-ideep [state level]
  (let [util (utility state)
        my-move (first state)]
    (if util
      [(first state) util]
      (if (= level 0)
        [(first state) -1]
        (let [vals (remove nil?  (map #(min-value-ideep % (dec level)) (future-boards @current-player (second state) \?)))
              k (if (not-empty vals) (apply max-key second vals))
              ]
          ;(println "max-vals")
          ;(println vals) ;; mayor q 1 o falla el max-key
          ;(println k)
          (if k
            (if (first state) [(first state) (second k)] k)
            (println "---------- NO K ----------");(into () [[my-move -100]])
            )
          )
        )
      )
    )
  )


(defn min-value-ideep [state level]
  (let [util (utility state)
        my-move (first state)]
    (if util
      [(first state) util]
      (if (= level 0)
        [(first state) -1]
        (let [vals (remove nil? (map #(max-value-ideep % (dec level)) (future-boards (if (= @current-player \x) \o \x) (second state) \?)))
              k (if (not-empty vals) (apply min-key second vals))
              ]
          ;(println "min-vals")
          ;(println vals) ;; mayor q 1 o falla el max-key
          ;(println k)
          (if k
            (if (first state) [(first state) (second k)] k)
            (println "---------- NO K ----------") ;(into () [[my-move -100]])
            )
          )
        )
      )
    )
  )

(defn min-value [])
(defn max-value [state]
  (let [util (utility state)
        my-move (first state)]
    ;(println "max")
    ;(if @one (println state))
    (reset! one false)

    (if util
      [(first state) util]
      (let [vals (remove nil?  (map min-value (future-boards @current-player (second state) \?)))
            k (if (not-empty vals) (apply max-key second vals))
            ]
        ;(println "max-vals")
        ;(println vals) ;; mayor q 1 o falla el max-key
        ;(println k)
        (if k
          (if (first state) [(first state) (second k)] k)
          (println "---------- NO K ----------");(into () [[my-move -100]])
          )
        )
      )
    )
  ;; check if terminal and return score
  ;; calculate succesor states
  ;; find max between -inf and min-value (s)
  ;; return max value
  )


(defn min-value [state]
  (let [util (utility state)
        my-move (first state)]
    ;(println "min")
    ;(println state)
    (if util
      [(first state) util]
      (let [vals (remove nil? (map max-value (future-boards (if (= @current-player \x) \o \x) (second state) \?)))
            k (if (not-empty vals) (apply min-key second vals))
            ]
        ;(println "min-vals")
        ;(println vals) ;; mayor q 1 o falla el max-key
        ;(println k)
        (if k
          (if (first state) [(first state) (second k)] k)
          (println "---------- NO K ----------");(into () [[my-move -100]])
          )
        )
      )
    )
  ;; check if terminal and return score
  ;; calculate succesor states
  ;; find min between +inf and max-value (s)
  ;; return min value
  )

(defn max-value-ab [state alpha beta])
(defn min-value-ab [state alpha beta]
  (let [util (utility state)
        my-move (first state)]
    (if util
      [my-move util]
      (let [r (reduce (fn [val col]
                        (let [m (max-value-ab col alpha (second val))
                              ret (if (< (second m) (second val))
                                    m
                                    val
                                    )
                              ]
                          (if (and (not= beta 1000) (<=  (second ret) alpha) )
                            (reduced ret)
                            )
                          ret
                          )
                        )
                      [[100 100] 1000]
                      (future-boards (if (= @current-player \x) \o \x) (second state) \?))
            ]
        (if my-move
          [my-move (second r)]
          r
          )
        )
      )
    )
  )

(defn max-value-ab [state alpha beta]
  (let [util (utility state)
        my-move (first state)]
    (if util
      [(first state) util]
      (let [r (reduce (fn [val col]
                        (let [m (min-value-ab col (second val) beta)
                              ret (if (> (second m) (second val))
                                    m
                                    val
                                    )
                             ]
                          (if (and (not= alpha -1000) (<= beta (second ret)))
                              (reduced ret)
                            )
                          ret
                          )
                        )
                      [[100 100] -1000]
                      (future-boards @current-player (second state) \?))
            ]
        (if my-move
          [my-move (second r)]
          r
          )
        )
      )
    )
  )
(defn max-value-ab-rand [state alpha beta])
(defn min-value-ab-rand [state alpha beta]
  (let [util (utility state)
        my-move (first state)]
    (if util
      [my-move util]
      (let [r (reduce (fn [val col]
                        (let [m (max-value-ab col alpha (second val))
                              ret (if (< (second m) (second val))
                                    m
                                    val
                                    )
                              ]
                          (if (and (not= beta 1000) (<= (second ret) alpha))
                            (reduced ret)
                            )
                          ret
                          )
                        )
                      [[100 100] 1000]
                      (shuffle (future-boards (if (= @current-player \x) \o \x) (second state) \?)))
            ]
        (if my-move
          [my-move (second r)]
          r
          )
        )
      )
    )
  )

(defn max-value-ab-rand [state alpha beta]
  (let [util (utility state)
        my-move (first state)]
    (if util
      [(first state) util]
      (let [r (reduce (fn [val col]
                        (let [m (min-value-ab col (second val) beta)
                              ret (if (> (second m) (second val))
                                    m
                                    val
                                    )
                              ]
                          (if (and (not= alpha -1000) (<= beta (second ret)))
                            (reduced ret)
                            )
                          ret
                          )
                        )
                      [[100 100] -1000]
                      (shuffle (future-boards @current-player (second state) \?)))
            ]
        (if my-move
          [my-move (second r)]
          r
          )
        )
      )
    )
  )
(defn forks [winnable player]
  "find if player makes a fork deducing from winnables [[0 0] {o 1}]"
  (if (= (get (second winnable) player) 2)
    (first winnable)
    )
  )

(defn best-next-move [board player opponent]
  (let [
        my-options  (future-boards player board \?)
        his-options (future-boards opponent board \?)
        winners  (remove nil? (map (fn [n]
                                     (let [coords (first n)
                                           winner (first (winner (second n)))]
                                       (if winner coords))
                                     )
                                   my-options))
        losers  (remove nil? (map (fn [n]
                                    (let [coords (first n)
                                          winner (first (winner (second n)))]
                                      (if winner coords))
                                    )
                                  his-options))
        old-forks (frequencies (remove nil? (winables  board)))
        potential-wins  (remove nil? (map (fn [n]
                                    (let [coords (first n)
                                          winables (winables (second n))]
                                      (if (not-empty winables) [coords  (frequencies winables)]))
                                    )
                                  my-options))
        my-forks (remove nil? (map #(forks % player) potential-wins))
        potential-loses  (remove nil? (map (fn [n]
                                      (let [coords (first n)
                                            winables (winables (second n))]
                                        (if (not-empty winables) [coords (frequencies winables)]))
                                      )
                                    his-options))
        his-forks (remove nil? (map #(forks % opponent) potential-loses))
        ]
    (if (not-empty winners)
      (rand-nth winners)
      (if (not-empty losers)
        (rand-nth losers)
        (if (not-empty my-forks)
          (first my-forks)
          (if (not-empty his-forks)
            (first his-forks)
            (first-moves board player opponent)
            ;(println "first m,oves")
            )
          )
        )
      )
    )
  )
(def empty-board [[\? \? \?]
                  [\? \? \?]
                  [\? \? \?]])

(def board (atom nil))

(def game-on (atom true))
(defn make-a-move [player board coords]
  (let [indexed (index-board board)
        selected (first (filter #(= (first %) coords) indexed))
        ]
    (if (= (second selected) \?)
      (apply-action player board coords)
      )
    )
  )


;(-main)
;@board
;;@current-player
;matrix
;(best-next-move matrix \o \x)
;(best-next-move @board \o \x)
;(future-boards \o @board \?)
(def start-time (atom nil))
(def play-time-x (atom 0))
(def play-time-o (atom 0))
;(-main)
;(repeatedly 10 #(-main))
;; DONE: timekeeping
;; DONE: make a pmap version and compare with ab
;; DONE: implement randomness on minimax (rand order of future boards)
;; DONE: depth level limiting
;; DONE: find optimal depth level ==> 2 !!

;; TODO:
;; progressive deepening?
;; symmetry recogn to reduce tree options?
;; predictive modelling?
;; NegaScout?
;; TODO: 8-puzzle
(comment
  "x = bestmove, o = max-value-ab, x's first, 10 plays"
  "x:24.413481 o:11253.817577"
  "x = bestmove, o = max-value-ab, o's first, 10 plays"
  "x:18.38725 o:100551.096312"

  "x = max-value, o = max-value-ab, x's first, 10 plays"
  "x:103639.180915 o:11060.427525"
  "x = max-value, o = max-value-ab, o's first, 10 plays"
  "x:11654.861062 o:102315.418369"
  "-----> ab pruning not doing too much??"

  "x = max-value-pmap, o = max-value-ab, x's first, 10 plays"
  "OutOfMemoryError unable to create new native thread  java.lang.Thread.start0 (Thread.java:-2)\n"


  "x = max-value-rand, o = max-value-ab, x's first, 10 plays"
  "x:101491.200252 o:11497.97137"
  "x = max-value-rand, o = max-value-ab, o's first, 10 plays"
  "x:11004.388851 o:101704.630212"

  "x = max-value-ideep, o = max-value-ab, x's first, 10 plays depth=4"
  "x:919.472522 o:10872.075267"
  "x = max-value-ideep, o = max-value-ab, x's first, 10 plays depth=3"
  "x:166.63179 o:11409.982519"
  "x = max-value-ideep, o = max-value-ab, x's first, 10 plays depth=2"
  "x:33.211772 o:11259.547563"
  "====> on depth=1 we start losing"
  "x = max-value-progressive, o = max-value-ab, x's first, 10 plays max-depth=2 start=0"
"x:47.886064\no:11179.915575"
  )
(defn max-value-progressive [max-depth level]
  (let [[coord util] (max-value-ideep [nil @board] level)
        ]
    (if (and (= util -1) (not= level max-depth))
      (max-value-progressive max-depth (inc level))
      coord
      )
    )
  )
;(max-value-ideep [nil @board] 1)
;(max-value-progressive 3 0)


(def activation-fn (fn [x] (Math/tanh x)))
(def dactivation-fn (fn [y] (- 1.0 (* y y))))

(defn layer-activation [inputs strengths]
  "forward propagate the input of a layer"
  (mapv activation-fn
        (mapv #(reduce + %)
              (* inputs (transpose strengths)))))


(defn output-deltas [targets outputs]
  "measures the delta errors for the output layer (Desired value – actual value) and multiplying it by the gradient of the activation function"
  (* (mapv dactivation-fn outputs)
     (- targets outputs)))


(defn hlayer-deltas [odeltas neurons strengths]
  (* (mapv dactivation-fn neurons)
     (mapv #(reduce + %)
           (* odeltas strengths))))

(defn update-strengths [deltas neurons strengths lrate]
  (+ strengths (* lrate
                  (mapv #(* deltas %) neurons))))



(defn feed-forward [input network]
  (let [[in i-h-strengths h h-o-strengths out] network
        new-h (layer-activation input i-h-strengths)
        new-o (layer-activation new-h h-o-strengths)]
    [input i-h-strengths new-h h-o-strengths new-o]))

(defn update-weights [network target learning-rate]
  (let [[ in i-h-strengths h h-o-strengths out] network
        o-deltas (output-deltas target out)
        h-deltas (hlayer-deltas o-deltas h h-o-strengths)
        n-h-o-strengths (update-strengths
                          o-deltas
                          h
                          h-o-strengths
                          learning-rate)
        n-i-h-strengths (update-strengths
                          h-deltas
                          in
                          i-h-strengths
                          learning-rate)]
    [in n-i-h-strengths h n-h-o-strengths out]))
(defn train-network [network input target learning-rate]
  (update-weights (feed-forward input network) target learning-rate))


(defn ff [input network]
  (last (feed-forward input network)))

(defn train-data [network data learning-rate]
  (if-let [[input target] (first data)]
    (recur
      (train-network network input target learning-rate)
      (rest data)
      learning-rate)
    network))

(defn inverse-data []
  (let [n (rand 1)
        m 0
        ]
    [[n m] [m n]]))

(defn gen-strengths [to from]
  (let [l (* to from)]
    (map vec (partition from (repeatedly l #(rand (/ 1 l)))))))

(defn construct-network [num-in num-hidden num-out]
  (vec (map vec [(repeat num-in 0)
                 (gen-strengths num-in num-hidden)
                 (repeat num-hidden 0)
                 (gen-strengths num-hidden num-out)
                 (repeat num-out 0)])))
(def tnn (atom (construct-network 9 45 9)))
(defn r [c] [(quot c 3) (rem c 3)])
(defn predict-move[b]
  (let [next-board (ff b @tnn)
        new-vals   (map-indexed (fn [i v]
                                  (if (not= 0 (get b i))
                                    0
                                    v
                                    )
                                  ) next-board)
        ret (if (= @current-player \x)
               (apply max-key second (map-indexed vector new-vals))
               (apply min-key second (map-indexed vector new-vals)))
        ]
    (println ret)
    (if (= (second ret) 0)
      (rand-nth (find-with-val @board \?))
      (r (first ret))
      )
    )
  )



(r 4)
(-main)
@tnn
;(predict-move (prepare-board @board))
;(rand-nth (find-with-val @board \?))
;(reset! tnn (read-string (slurp "180tnn.dat")))
(reset! tnn (read-string (slurp "45neurons.dat")))
(spit "45neurons.dat" (pr-str @tnn))
;(spit "36tnn.dat" (pr-str @tnn))
(repeatedly  100 #(-main))
(defn prepare-board [board]
  (into []
        (map #(if (= % \?)
          0
          (if (= % \x)
            1
            -1)) (apply concat board)))
  )

@wins-o
@wins-x
(def wins-o (atom 0))
(def wins-x (atom 0))
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (reset! board empty-board)
  (reset! current-player \o)
  (reset! game-on true)
  ;(println "")
  ;(println "introduce coord [r c] USER (x) first")
  (while @game-on
    ;(println (str "Current player: " @current-player))
    ;(doall (map println @board))
    ;(Thread/sleep 1000)
    (reset! start-time (System/nanoTime))
    (let [
          ;input (if (= @current-player \x)
          ;        (str/split (read-line) #" "))
          move (if (= @current-player \x)
                 ;[(Integer. (re-find #"\d+" (first input))) (Integer. (re-find #"\d+" (second input)))]
                 ;(best-next-move @board \x \o)
                 (predict-move (prepare-board @board))
                 (rand-nth (find-with-val @board \?))
                 ;(best-next-move @board \o \x)
                 ;(first (max-value-pmap [nil @board]))
                 ;(first (max-value-ab-rand [nil @board] -1000 1000))
                 ;(first (max-value-ideep [nil @board] 2))
                 ;(max-value-progressive 2 1)
                 ;(first (max-value-ab [nil @board] -1000 1000))
                 ;(first (max-value-ab [nil @board] -1000 1000))
                 )
          next-board (make-a-move @current-player @board move)
          ]
      (if (= @current-player \x)
        (swap! play-time-x #(+ % (- (System/nanoTime) @start-time)))
        (swap! play-time-o #(+ % (- (System/nanoTime) @start-time)))
        )

      ;(doall (map println next-board))
      ;(println move)

      (if next-board
        (do
          (if (= @current-player \x)
            (repeatedly 100  (swap! tnn #(train-data % [[(prepare-board @board)
                                        (prepare-board
                                          (make-a-move \x @board
                                                       (first (max-value-ab [nil @board] -1000 1000)
                                                              )
                                                       ;(best-next-move @board \x \o)
                                                       ))]]
                                    0.1)))
            ;(swap! tnn #(train-data % [[(prepare-board-inv @board) (prepare-board-inv next-board)]] 0.2))
            )
          (reset! board next-board)
               (let [winner (winner @board)]
                 (if (not-empty winner)
                   (do
                     (println "winner!!!!!")
                     (println winner)
                     (if (= (first winner) \x)
                       (swap! wins-x inc)
                       (swap! wins-o inc)
                       )
                     (reset! game-on false)
                     )
                   (if (not-empty (find-with-val @board \?))
                     (reset! current-player (if (= @current-player \x) \o \x))
                     (reset! game-on false)
                     )
                   )
                 ))
        (do
          (println "wrong move!")
        (println move))
        )

      )
    )
  (doall (map println @board))
  ;(println "Stats:")
  ;(println (str "x:" (/ @play-time-x 1e6)))
  ;(println (str "o:" (/ @play-time-o 1e6)))
  )

(def m
  [[\? \? \?]
   [\? \? \?]
   [\x \? \o]])
(def empty-board [[\? \? \?]
                  [\? \? \?]
                  [\? \? \?]])
(future-boards @current-player empty-board \?)
(find-with-val empty-board \?)
(reset! current-player \x)
(max-value-ideep [nil m] 1)

(comment
  (def m
    [[\? \? \?]
     [\? \o \?]
     [\x \? \o]])

  (reset! current-player \x)
  (max-value-ab [nil m] -1000 1000)
  (def m
    [
     [\o \x \?]
     [\x \o \o]
     [\x \o \x]])
  (def m
    [[\x \? \?]
     [\o \o \x]
     [\x \? \o]])
  (best-next-move m \x \o)
  (def l (make-a-move \x m [0 2]))
  (not-empty (find-with-val l \?))

  (def matrix2 [[\x \? \?]
                [\o \o \x]
                [\x \? \o]])
  (reset! current-player \x)
  (max-value [nil matrix2])

  (def matrix2 [[\x \? \?]
                [\o \o \?]
                [\x \? \?]])
  @current-player
  (reset! current-player \x)
  (first (winner matrix2))
  (utility matrix2)

  (max-value [[10 10] matrix2])

  (utility [[10 10] matrix2])
  (future-boards \x matrix2 \?)
  (winner matrix2)
  (remove #(or (= % '[0 0]) (= % '[max max])) '[[0 0] [0 max] [max 0] [max max]])
  (first-moves forking-matrix \x \o)
  (first-moves empty-matrix \x \o)
  (first-moves sides-matrix \x \o)
  (first-moves start-matrix \o \x)
  (def empty-matrix [[\o \? \?]
                     [\? \? \?]
                     [\? \? \?]])
  (def forking-matrix [[\? \? \o]
                       [\x \? \o]
                       [\? \? \x]])
  (def sides-matrix [[\o \x \o]
                     [\? \x \x]
                     [\x \o \o]])
  (def corner-matrix [[\? \? \x]
                      [\x \o \o]
                      [\o \? \x]])
  (best-next-move start-matrix \x \o)

  (best-next-move sides-matrix \x \o)
  (best-next-move forking-matrix \o \x)
  (def matrix [[\? \? \?]
               [\? \x \?]
               [\? \? \?]])
  (def matrix2 [[\x \o \o]
                [\x \x \?]
                [\x \? \?]])
  (winner matrix2)

  (forks matrix)
  (best-next-move matrix \x \o)

  (best-next-move matrix2 \x \o)
  matrix
  (future-boards \x matrix \?)
  (apply-action \? matrix [0 1])
  (concat '(([1 0] [1 2]) ([2 1] [2 2])) )
(type matrix)
(assoc matrix 0 (assoc (first matrix) 2 \?))
(find-with-val matrix \?)
(map #(apply-action \x matrix %) (find-with-val matrix \?))
(map #(println %) (find-with-val matrix \?))

  ;; using a solving algorithm



  (map #(= % \?) (second matrix))
  (map-indexed (fn [i v] [i (= v \?)]) (second matrix))
  (map-indexed (fn [i v] [i v])  matrix)
  (map-indexed (fn [i v] [i v])  matrix)
  ((fn [[row-index row-val]] (map-indexed (fn [i v] [(+ (* row-index (count row-val)) i) v]) row-val)) [2 [\x \o \x]])
  (let [indexed-rows (map-indexed (fn [i v] [i v])  matrix)]
    (map (fn [[row-index row-val]]
           (map-indexed (fn [i v] [(+ (* row-index (count row-val)) i)  (= v \?)]) row-val)
           ) indexed-rows)
    )
  (let [indexed-rows (map-indexed (fn [i v] [i v])  matrix)]
    (map (fn [[row-index row-val]]
            (map-indexed (fn [i v] [(+ (* row-index (count row-val)) i)  (= v \?)]) row-val)
           ) indexed-rows)
    )

  (let [indexed-rows (map-indexed (fn [i v] [i v])  matrix)]
    (filter not-empty (map (fn [[row-index row-val]]
           (filter second (map-indexed (fn [i v] [(+ (* row-index (count row-val)) i)  (= v \?)]) row-val))
           ) indexed-rows))
    )
  (map (fn [[row-index row-val]] (map-indexed (fn [i v] [(+ (* row-index (count row-val)) i) v]) row-val)) (map-indexed (fn [i v] [i v])  matrix))
  ;(map (fn [n] (map #(= % \?) n))  matrix)
  (keep second (find-with-val matrix \?))

  )
