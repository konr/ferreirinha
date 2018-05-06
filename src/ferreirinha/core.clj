(ns ferreirinha.core
  (:require [clojure.set :refer [map-invert]]))

(def notas
  [:dó :dó# :ré :ré# :mi :fá :fá# :sol :sol# :lá :lá# :si])

(def oitavas [0 1 2 3])

(def notas-da-viola
  (for [oitava oitavas nota notas]
    {:nota nota :oitava oitava}))

(def índice->nota
  (->> notas-da-viola
       (map-indexed hash-map)
       (reduce merge)))

(def nota->índice (map-invert índice->nota))

(defn próximo-semitom [nota]
  (-> nota
      nota->índice
      (+ 1)
      índice->nota))

(defn nota-inicial->braço [inicial]
  (let [índice (nota->índice inicial)]
    (->> (range 12)
         (map #(assoc (índice->nota (+ índice %)) :posição %)))))

(defn corda [índice-da-corda nota-inicial]
  (hash-map :notas (nota-inicial->braço nota-inicial)
            :corda (inc índice-da-corda) ; corda zero não dá, né?
            :nota-inicial nota-inicial))

(def ordens [{:nota :si :oitava 0}
             {:nota :mi :oitava 1}
             {:nota :sol# :oitava 1}
             {:nota :si :oitava 1}
             {:nota :mi :oitava 2}])


(def braço (map-indexed corda ordens))

;; --- 

(defn primeiro-que [predicado coleção]
  (->> coleção (filter predicado) first))


(defn qual-a-nota [corda posição]
  (->> braço
       (primeiro-que #(= (:corda %) corda))
       :notas
       (primeiro-que #(= (:posição %) posição))))

(defn achar [nota notas]
  (primeiro-que #(= (select-keys % [:nota :oitava]) nota) notas))

(defn achar-no-braço [nota braço]
  (->> braço
       (map #(assoc % :nota (achar nota (:notas %))))
       (filter :nota)
       (map #(assoc (:nota %) :corda (:corda %)))
       set))

(defn map-vals [f map]
  (into {} (for [[k v] map] [k (f v)])))

(defn map-keys [f map]
  (into {} (for [[k v] map] [(f k) v])))

(defn migué-transpose [& xs]
  (apply map list xs))
