(ns ferreirinha.soberano
  (:require [ferreirinha.cifra :as cifra]
            [ferreirinha.core :as f]))

(def frase-1a
  [{:posição 0 :batidas 3 :corda 4}
   {:posição -1 :batidas 2 :corda 4}
   {:posição -2 :batidas 2 :corda 3}
   {:posição -3 :batidas 1 :corda 3}])

(def modo-do-soberano
  (->>
   [[3 2]
    [4 0]
    [4 2]
    [4 4]
    [4 6]
    [4 7]
    [4 9]
    [4 11]]
   (map #(apply f/qual-a-nota %))
   (map-indexed #(assoc %2 :posição-no-modo %1))))

(defn faz-frase [começo frase modo]
  (let [posição-inicial-no-modo (:posição-no-modo (f/achar {:nota :lá# :oitava 2} modo-do-soberano))]
    (->> frase
         (map #(assoc % :posição-no-modo (+ posição-inicial-no-modo (:posição %))))
         (mapcat #(repeat (:batidas %) %))
         (map (fn [padrão] (->
                           #(= (:posição-no-modo %) (:posição-no-modo padrão))
                           (f/primeiro-que modo)
                           (select-keys [:nota :oitava])
                           (f/achar-no-braço f/braço)
                           (->> (f/primeiro-que #(= (:corda %) (:corda padrão)))))))
         (map vector)))) ; FIX duetar


(-> {:nota :lá# :oitava 2}
    (faz-frase frase-1a modo-do-soberano)
    cifra/cifrar
    println)
