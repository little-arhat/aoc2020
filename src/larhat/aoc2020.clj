(ns larhat.aoc2020
  (:gen-class))

(defn input [n]
  (slurp (format "resources/larhat/inp%d.txt" n)))

(defn inp-words [n]
  (-> (input n)
    (clojure.string/split #"\s")))

(defn inp-lines [n]
  (-> (input n)
    (clojure.string/split #"\n")))

(defn parse-int [n]
  (Integer. n))

(defn aoc-1 []
  (let [i (map parse-int (inp-lines 1))]
    (first (for [x i
                 y i
                 :let  [sum (+ x y)]
                 :when (= sum 2020)]
             (* x y)))))

(defn aoc-1-2 []
  (let [i (map parse-int (inp-lines 1))]
    (first (for [x i
                 y i
                 z i
                 :let  [sum (+ x y z)]
                 :when (= sum 2020)]
             (* x y z)))))

(def pass-r #"(\d+)-(\d+) (\w): (\w+)")
(defn parse-pass [p]
  (let [[_ min max char pass] (re-find pass-r p)]
    {:min (parse-int min)
     :max (parse-int max)
     :char (first (char-array char))
     :pass pass}))

(defn valid-pass [{:keys [min max char pass]}]
  (let [filtered (filter #(= % char) pass)
        c (count filtered)]
    (and
      (>= c min)
      (<= c max))))

(defn valid-pass-2 [{:keys [min max char pass]}]
  (not=
    (= char (.charAt pass (dec min)))
    (= char (.charAt pass (dec max)))))

(defn aoc-2 []
  (->> (inp-lines 2)
    (map parse-pass)
    (filter valid-pass)
    (count)))

(defn aoc-2-2 []
  (->> (inp-lines 2)
    (map parse-pass)
    (filter valid-pass-2)
    (count)))

(defn mod+ [a b m]
  (mod (+ a b) m))

(defn find-path [map discard? step]
  (let [width (count (first map))]
    (reduce
      (fn [{:keys [pos trees coord]} line]
        (let [pos'   (mod+ pos step width)
              ch     (.charAt line pos')
              coord' [pos' ch line]
              trees' (if (= ch \#) (inc trees) trees)]
          {:pos   pos', :trees trees',
           :coord (conj coord coord')}))
      {:pos 0, :trees 0, :coord [] }
      (keep-indexed (fn [index elem]
                      (if (or (= 0 index) (discard? index))
                        nil
                        elem))
        map))))

(defn find-trees [map discard? step]
  (:trees (find-path map discard? step)))

(defn aoc-3 []
  (let [map (inp-lines 3)]
    (find-trees map #(= 0 %) 3)))

(defn aoc-3-2 []
  (let [slope (inp-lines 3)]
    (*
      (find-trees slope (constantly false) 1)
      (find-trees slope (constantly false) 3)
      (find-trees slope (constantly false) 5)
      (find-trees slope (constantly false) 7)
      (find-trees slope #(odd? %) 1))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
