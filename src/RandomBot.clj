(ns RandomBot
  (:gen-class)
  (:require io
            [Moves :as moves]))

(def bot-name "RandomClojureBot")

(defn -main []
  (let [{:keys [my-id productions width height game-map]} (io/get-init!)]

    ;; Do any initialization you want with the starting game-map before submitting the bot-name

    (println bot-name)

    (doseq [turn (range)]
      (let [game-map (io/create-game-map width height productions (io/read-ints!))]
        (io/send-moves! (moves/improve-moves-2 my-id game-map))))))
