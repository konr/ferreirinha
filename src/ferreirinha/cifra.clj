(ns ferreirinha.cifra
  (:require [ferreirinha.core :as f]
            [clojure.string :as string]))


(defn texto [posição]
  (format "%3s" (or posição " --")))

(def ordem-das-cordas-na-impressão [5 4 3 2 1])

(defn bloco-de-nota->texto [bloco-de-nota]
  (let [corda->posição (->> bloco-de-nota
                            (group-by :corda)
                            (f/map-vals (comp :posição first)))]
    (map (comp texto corda->posição) ordem-das-cordas-na-impressão)))

(defn cifrar [blocos-de-notas]
  (->> blocos-de-notas
       (map bloco-de-nota->texto)
       (apply f/migué-transpose)
       (map string/join)
       (string/join "\n")))
