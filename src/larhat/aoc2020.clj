(ns larhat.aoc2020
  (:require
   [larhat.prelude :refer :all]
   [clojure.string :as str]
   [clojure.set :as cl-set]))

(defn day-1-1 [data]
  (as-> (map parse-int data) i
    (for [x     i
          y     i
          :let  [sum (+ x y)]
          :when (= sum 2020)]
      (* x y))
    (first i)))
(defn run-day-1-1 []
  (day-1-1 (inp-lines 1)))

(defn day-1-2 [data]
  (as-> (map parse-int data) i
    (for [x     i
          y     i
          z     i
          :let  [sum (+ x y z)]
          :when (= sum 2020)]
      (* x y z))
    (first i)))
(defn run-day-1-2 []
  (day-1-2 (inp-lines 1)))

(defn parse-pass [p]
  (let [[_ min max char pass] (re-find #"(\d+)-(\d+) (\w): (\w+)" p)]
    {:min (parse-int min)
     :max (parse-int max)
     :char (first (char-array char))
     :pass pass}))

(defn valid-pass [{:keys [min max char pass]}]
  (as-> (filter #(= % char) pass) x
    (count x)
    (<= min x max)))

(defn day-2-1 [data]
  (->> data
    (map parse-pass)
    (filter valid-pass)
    (count)))
(defn run-day-2-1 []
  (day-2-1 (inp-lines 2)))

(defn valid-pass-2 [{:keys [min max char pass]}]
  (not=
    (= char (.charAt pass (dec min)))
    (= char (.charAt pass (dec max)))))

(defn day-2-2 [data]
  (->> data
    (map parse-pass)
    (filter valid-pass-2)
    (count)))
(defn run-day-2-2 []
  (day-2-2 (inp-lines 2)))

(defn mod+ [a b m]
  (mod (+ a b) m))

(defn sliding-down [map discard? step]
  (let [width (count (first map))]
    (reduce
      (fn [{:keys [pos trees coord]} line]
        (let [pos'   (mod+ pos step width)
              ch     (.charAt line pos')
              trees' (if (= ch \#) (inc trees) trees)]
          {:pos   pos', :trees trees'}))
      {:pos 0, :trees 0}
      (keep-indexed (fn [index elem]
                      (if (or (= 0 index) (discard? index))
                        nil
                        elem))
        map))))

(defn find-trees [map discard? step]
  (:trees (sliding-down map discard? step)))

(defn day-3-1 [data]
  (find-trees data #(= 0 %) 3))
(defn run-day-3-1 []
  (day-3-1 (inp-lines 3)))

(defn day-3-2 [slope]
  (*
    (find-trees slope (constantly false) 1)
    (find-trees slope (constantly false) 3)
    (find-trees slope (constantly false) 5)
    (find-trees slope (constantly false) 7)
    (find-trees slope #(odd? %) 1)))
(defn run-day-3-2 []
  (day-3-2 (inp-lines 3)))

(def fields #{:byr :iyr :eyr :hgt :hcl :ecl :pid :cid})
(defn get-field [s]
  (-> s
    (subs 0 3)
    (keyword)))

(defn get-only-fields [passport]
  (into #{} (map get-field) passport))

(defn valid-passport [passport]
  (let [passport-fields (get-only-fields passport)
        diff (cl-set/difference fields passport-fields)]
    (or
      (empty? diff)
      (= #{:cid} diff))))

(defn day-4-1 [data]
  (->> data
    (map words)
    (filter valid-passport)
    (count)))
(defn run-day-4-1 []
  (day-4-1 (inp-phrases 4)))

(defn between [as min max]
  (<= min (parse-int as) max))

(defn valid-height [s]
  (let [l (count s)
        l' (- l 2)
        d (subs s 0 l')
        m (subs s l')]
    (cond
        (= "cm" m) (between d 150 193)
        (= "in" m) (between d 59 76))))

(defn required [nextp]
  (fn [s] (when s (nextp s))))

(def passport-rules
  {:byr (required #(between % 1920 2002))
   :iyr (required #(between % 2010 2020))
   :eyr (required #(between % 2020 2030))
   :hgt (required valid-height)
   :hcl (required #(re-seq #"^#[0-9a-f]{6}$" %))
   :ecl (required #(#{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} %))
   :pid (required #(re-seq #"^\d{9}$" %))
   :cid (constantly true)})

(def check-passport
  (->> passport-rules
    (cl-set/map-invert)
    (map #(apply comp %))
    (apply every-pred)))

(defn get-field-value [s]
  (let [[f v] (str/split s #":")]
    [(keyword f) v]))

(defn get-fields-2 [passport]
  (into {} (map get-field-value) passport))

(defn valid-passport-2 [passport]
  (-> passport
    get-fields-2
    check-passport))

(defn day-4-2 [data]
  (->> data
    (map words)
    (filter valid-passport-2)
    (count)))
(defn run-day-4-2 []
  (day-4-2 (inp-phrases 4)))

(defn find-seat-id [barcode]
  (-> barcode
    (str/escape {\F \0 \B \1 \R \1 \L \0})
    (parse-int 2)))

(defn day-5-1 [data]
  (->> data
    (map find-seat-id)
    (apply max)))
(defn run-day-5-1 []
  (day-5-1 (inp-lines 5)))

(defn find-missing [seats]
  (reduce
    (fn [prev-el el]
      (if (< 1 (- el prev-el))
        (reduced (dec el))
        el))
    seats))

(defn day-5-2 [data]
  (->> data
    (map find-seat-id)
    sort
    find-missing))
(defn run-day-5-2 []
  (day-5-2 (inp-lines 5)))

(defn num-anwsers [s]
  (->> s
    (remove #{\space \newline})
    (into #{})
    count))

(defn day-6-1 [data]
  (->> data
    (map num-anwsers)
    (reduce +)))
(defn run-day-6-1 []
  (day-6-1 (inp-phrases 6)))

(defn num-answers-2 [s]
  (->> s
    lines
    (map set)
    (reduce clojure.set/intersection)
    count))

(defn day-6-2 [data]
  (->> data
    (map num-answers-2)
    (reduce +)))
(defn run-day-6-2 []
  (day-6-2 (inp-phrases 6)))

(defn no-spaces [x]
  (str/replace x " " "-"))

(defn parse-bag [l]
  (let [[left right] (str/split l #"contain")
         outer (-> left
                 (str/split #"bag")
                 first
                 str/trim
                 no-spaces
                 keyword)
        inners (as-> right x
                 (str/trim x)
                 (str/split x #"(bags|bag|\d+|,|\.)")
                 (remove str/blank? x)
                 (map str/trim x)
                 (map no-spaces x)
                 (map keyword x))]
    (map #(vector % outer) inners)))

(defn parse-bags [l]
  (->> l
    (mapcat parse-bag)
    (reduce (fn [m [f v]]
              (update m f
                #(conj % v))) {})))

(defn find-all-bags [start path g]
  (let [links (start g)]
    do
    (if links
      (mapcat
        #(find-all-bags % (conj path %) g)
        links)
                                        ; else
      path)))

(defn day-7-1 [data]
  (->> data
    parse-bags
    (find-all-bags :shiny-gold [])
    distinct
    count))
(defn run-day-7-1 []
  (day-7-1 (inp-lines 7)))

(defn count-and-bag [l]
  (let [[[_ n bag]] (re-seq #"(\d+) ([a-z ]+)" l)]
    (when n
      [(parse-int n) (keyword (no-spaces bag))])))

(defn parse-bag-2 [l]
  (let [[left right] (str/split l #"contain")
        outer        (-> left
                       (str/split #"bag")
                       first
                       str/trim
                       no-spaces
                       keyword)
        inners       (as-> right x
                       (str/trim x)
                       (str/split x #"(bags|bag|,|\.)")
                       (map str/trim x)
                       (remove str/blank? x)
                       (map count-and-bag x)
                       (remove empty? x))]
    [outer inners]))

(defn parse-bags-2 [l]
  (->> l
    (map parse-bag-2)
    (into {})))

(defn count-inner-bags [start g]
  (let [links (start g)]
    (if links
      (reduce +
        (map (fn [[n s]]
               (+ n (* n
                      (count-inner-bags s g))))
          links))
      0)))

(defn day-7-2 [data]
  (->> data
    parse-bags-2
    (count-inner-bags :shiny-gold)))
(defn run-day-7-2 []
  (day-7-2 (inp-lines 7)))

(defn parse-cmd [l]
  (let [[left right] (str/split l #" ")]
    [(keyword left)
     (parse-int right)]))

(defn execute-no-loops-aux [cmds acc pos visited]
  (if (visited pos)
    acc
    (let [[cmd arg] (nth cmds pos)
          visited' (conj visited pos)]
      (cond
        (= :acc cmd) (recur cmds (+ acc arg) (inc pos) visited')
        (= :nop cmd) (recur cmds acc (inc pos) visited')
        (= :jmp cmd) (recur cmds acc (+ pos arg) visited')))))

(defn execute-no-loops [cmds]
  (execute-no-loops-aux cmds 0 0 #{}))

(defn swap-jmp-nop [cmd]
  (cond
    (= cmd :nop) :jmp
    (= cmd :jmp) :nop
    :else cmd))

(defn replace-if [i req-i cmd]
  (if (= i req-i)
    (swap-jmp-nop cmd)
    cmd))

(defn day-8-1 [data]
  (->> data
    (mapv parse-cmd)
    execute-no-loops))
(defn run-day-8-1 []
  (day-8-1 (inp-lines 8)))

(defn execute-no-loops-with-replace [cmds acc pos visited replace]
  (cond
    (= (count cmds) pos) acc
    (visited pos) (recur cmds 0 0 #{} (inc replace))
    :else
    (let [[pre-cmd arg] (nth cmds pos)
          cmd (replace-if pos replace pre-cmd)
          visited' (conj visited pos)]
      (cond
        (= :acc cmd) (recur cmds (+ acc arg) (inc pos) visited' replace)
        (= :nop cmd) (recur cmds acc (inc pos) visited' replace)
        (= :jmp cmd) (recur cmds acc (+ pos arg) visited' replace)))))

(defn execute-no-loops-2 [cmds]
  (execute-no-loops-with-replace cmds 0 0 #{} 0))

(defn day-8-2 [data]
  (->> data
    (map parse-cmd)
    vec
    execute-no-loops-2))
(defn run-day-8-2 []
  (day-8-2 (inp-lines 8)))

(defn xmas-valid [batch]
  (let [preamble (butlast batch)
        inp (last batch)]
    (when (nil? (first (filter #(= % inp)
                         (for [x preamble y preamble]
                           (+ x y)))))
      inp)))

(defn pre-aoc-9 [data]
  (->> data
    (map parse-long)
    vec))

(defn mid-aoc-9 [i]
  (->> i
    (partition 26 1)
    (keep xmas-valid)
    first))

(defn day-9-1 [data]
  (mid-aoc-9 (pre-aoc-9 data)))
(defn run-day-9-1 []
  (day-9-1 (inp-lines 9)))

(defn subseq-sum-eq [coll el]
  (->>
    (iterate rest coll)
    (take-while seq)
    (mapcat #(reductions conj [] %))
    (filter #(= el (reduce + %)))
    first))

(defn day-9-2 [data]
  (let [inp (pre-aoc-9 data)
        inv (mid-aoc-9 inp)
        subs (subseq-sum-eq inp inv)]
    (+ (apply min subs) (apply max subs))))
(defn run-day-9-2 []
  (day-9-2 (inp-lines 9)))

(defn with-device [chain]
  (concat chain (list (+ 3 (last chain)))))
(defn with-port [chain]
  (cons 0 chain))

(defn chain-diffs [chain]
  (->> chain
    (partition 2 1)
    (map #(apply - %))
    (map #(* -1 %))))

(defn mk-chain [data]
  (->> data
    (map parse-int)
    sort
    with-device
    with-port))

(defn day-10-1 [data]
  (->> data
    mk-chain
    chain-diffs
    frequencies
    (#(* (% 1) (% 3)))))
(defn run-day-10-1 []
  (day-10-1 (inp-lines 10)))

(def chain-counts-rec
  (memoize
    (fn [adapter chain-set]
      (cond
        (not (chain-set adapter)) 0
        (= adapter 0)             1
        :else
        (+
          (chain-counts-rec (- adapter 1) chain-set)
          (chain-counts-rec (- adapter 2) chain-set)
          (chain-counts-rec (- adapter 3) chain-set))))))

(defn chain-counts-rec* [chain]
  (let [device (last chain)
        chain-set (set chain)]
    (chain-counts-rec device chain-set)))

(defn chain-counts-dp [chain]
  (reduce
    (fn [routes adapter]
      (assoc routes adapter
        (apply + (map #(get routes % 0)
                   (range (- adapter 3) adapter)))))
    {0 1}
    (rest chain)))

(defn chain-counts-dp* [chain]
  ((chain-counts-dp chain) (last chain)))

(defn tribonacci [x]
  (last
    (reduce
      (fn [[a b c] n]
        (conj [b c] (+ a b c)))
      [0 0 1]
      (range x))))

(defn chain-counts-formula [chain]
  (let [xf
        (comp
          (partition-by identity) ; find subsequent deltas of the same length
          (filter #(some #{1} %)) ; in practice there only 1 or 3,
                                  ; and only diffs of 1 add variability
          (map count)             ; count number of subsequent 1s
          (map tribonacci))       ; compute number of choices as tribonnaci of number of subsequent 1s
        ]
    (transduce xf * (chain-diffs chain))))

(defn day-10-2-dp [data]
  (->> data
    mk-chain
    chain-counts-dp*))
(defn day-10-2-rec [data]
  (->> data
    mk-chain
    chain-counts-rec*))
(defn day-10-2-formula [data]
  (->> data
    mk-chain
    chain-counts-formula))
(defn run-day-10-2 []
  (let [i (inp-lines 10)]
    [(day-10-2-dp i)
     (day-10-2-rec i)
     (day-10-2-formula i)]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
