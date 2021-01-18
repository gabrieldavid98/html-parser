(ns html-parser.core
  (:require [clojure.string :as string])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(def normalizer (comp string/lower-case name))

(defn map->html-attributes [attrs]
  (->> attrs
       (map (fn [[k v]]
              (if (string? v)
                (str (normalizer k) "='" (string/lower-case v) "'")
                (normalizer k))))
       (string/join " ")))

(defn keyword->opening-tag [kw]
  (str "<" (normalizer kw) ">"))

(defn keyword-attributes->opening-tag [kw attrs]
  (str "<" (normalizer kw) " " (map->html-attributes attrs) ">"))

(defn keyword->closing-tag [kw]
  (str "</" (normalizer kw) ">"))

(defn has-attributes? [tree]
  (map? (second tree)))

(defn singleton? [tree]
  (and (vector? tree)
       (#{:img :meta :link :input :br} (first tree))))

(defn singleton-with-attrs? [tree]
  (and (singleton? tree) (has-attributes? tree)))

(defn element-with-attrs? [tree]
  (and (vector? tree) (has-attributes? tree)))

(defn ->html [tree]
  (cond
    (not tree) tree
    (string? tree) tree
    (singleton-with-attrs? tree) 
    (keyword-attributes->opening-tag (first tree) (second tree))
    (singleton? tree)
    (keyword->opening-tag (first tree))
    (element-with-attrs? tree)
    (apply str 
           (concat 
            [(keyword-attributes->opening-tag (first tree) (second tree))]
            (map ->html (next (next tree)))
            [(keyword->closing-tag (first tree))]))
    (vector? tree)
    (apply str
           (concat 
            [(keyword->opening-tag (first tree))]
            (map ->html (next tree))
            [(keyword->closing-tag (first tree))]))))

;; In repl: (->html test-tree)

(def test-tree
  [:html
   [:head
    [:title "HTML ouput from vectors!"]]
   [:body
    [:h1 {:id "page-title"} "HTML output from vectors!"]
    [:div {:class "main-content" :data-checked true}
     [:p "Converting nested lists into HTML is an old Lisp trick"]
     [:p "But Clojure uses vectors instead."]]]])