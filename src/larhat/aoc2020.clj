(ns larhat.aoc2020
  (:gen-class))

(defn input [n]
  (slurp (format "resources/larhat/inp%d.txt" n)))

(defn inp-seq [n]
  (-> (input n)
    (clojure.string/split #"\n")))

(defn parse-int [n]
  (Integer. n))

(defn aoc-1 []
  (let [i (map parse-int (inp-seq 1))]
    (first (for [x i
                 y i
                 :let  [sum (+ x y)]
                 :when (= sum 2020)]
             (* x y)))))

(defn aoc-1-2 []
  (let [i (map parse-int (inp-seq 1))]
    (first (for [x i
                 y i
                 z i
                 :let  [sum (+ x y z)]
                 :when (= sum 2020)]
             (* x y z)))))

(def pass-r #"(\d+)-(\d+) (\w): (\w+)")
(defn parse-password [p]
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

(defn valid-pass-2 [:keys [pos-1 pos-2 char pass]]
  (or
    (= char (.charAt pass (+1 pos-1)))
    (= char (.charAt pass (+1 pos-2)))))

(defn aoc-2 []
  (->> (inp-seq 2)
    (map parse-password)
    (filter valid-pass)
    (count)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
