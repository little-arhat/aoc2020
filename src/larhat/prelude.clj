(ns larhat.prelude
  (:require
   [clojure.string :as str]
   [clojure.set :as cl-set]))

(defn input [n]
  (slurp (format "resources/larhat/inp%d.txt" n)))

(defn words [s]
  (str/split s #"\s"))

(defn lines [s]
  (str/split-lines s))

(defn phrases [s]
  (str/split s #"\n\n"))

(defn inp-words [n]
  (-> (input n) words))

(defn inp-lines [n]
  (-> (input n) lines))

(defn inp-phrases [n]
  (-> (input n) phrases))

(defn parse-int
  ([n]  (Integer. n))
  ([n x] (Integer/parseInt n x)))

(defn parse-long [n]
  (Long. n))
