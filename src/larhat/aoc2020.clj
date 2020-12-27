(ns larhat.aoc2020
  (:gen-class))

(defn input [n]
  (slurp (format "resources/larhat/inp%d.txt" n)))

(defn words [s]
  (clojure.string/split s #"\s"))

(defn lines [s]
  (clojure.string/split s #"\n"))

(defn phrases [s]
  (clojure.string/split s #"\n\n"))

(defn inp-words [n]
  (-> (input n) words))

(defn inp-lines [n]
  (-> (input n) lines))

(defn inp-phrases [n]
  (-> (input n) phrases))

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

(def fields #{:byr :iyr :eyr :hgt :hcl :ecl :pid :cid})

(defn get-field [s]
  (-> s
    (subs 0 3)
    (keyword)))

(defn get-fields [passport]
  (->> passport
    (map get-field)
    (into #{})))

(defn valid-passport [passport]
  (let [passport-fields (get-fields passport)
        diff (clojure.set/difference fields passport-fields)]
    (or
      (empty? diff)
      (= #{:cid} diff))))

(defn aoc-4 []
  (->> (inp-phrases 4)
    (map words)
    (filter valid-passport)
    (count)))

(defn between [as min max]
  (when as
    (let [a (parse-int as)]
      (and (>= a min) (<= a max)))))

(defn split-str-backwards [s i]
  (let [l (count s)
        l' (- l i)]
    [(subs s 0 l') (subs s l')]))

(defn valid-height [s]
  (when s
    (let [[d m] (split-str-backwards s 2)]
      (cond
        (= "cm" m) (between d 150 193)
        (= "in" m) (between d 59 76)
        :else      false))))

(def color-r #"^#[0-9a-f]{6}$")
(def pid-r #"^\d{9}$")
(def eyes #{:amb :blu :brn :gry :grn :hzl :oth})

(defn safe-re-seq [r s]
  (when s
    (re-seq r s)))

(def passport-rules
  {:byr #(between % 1920 2002)
   :iyr #(between % 2010 2020)
   :eyr #(between % 2020 2030)
   :hgt valid-height
   :hcl #(safe-re-seq color-r %)
   :ecl #(eyes (keyword %))
   :pid #(safe-re-seq pid-r %)
   :cid (constantly true)})

(defn get-field-value [s]
  (let [[f v] (clojure.string/split s #":")]
    [(keyword f) v]))

(defn get-fields-2 [passport]
  (->> passport
    (map get-field-value)
    (into {})))

(defn valid-passport-2 [passport]
  (let [passport-fields (get-fields-2 passport)]
    (every?
      (fn [[k f]]
        (f (k passport-fields)))
     passport-rules)))

(defn aoc-4-2 []
  (->> (inp-phrases 4)
    (map words)
    (filter valid-passport-2)
    (count)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
