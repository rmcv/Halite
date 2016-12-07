(ns MyBot
  (:require [game]
            [io])
  (:gen-class))

(def bot-name "CLJ_RMCV")

(defn random-moves
  "Takes a 2D vector of sites and returns a list of [site, direction] pairs"
  [my-id game-map]
  (let [width  (count (first game-map))
        height (count game-map)]
    (for [c    (range width)
          r    (range height)
          :let [{:keys [production strength]
                 :as site} (get-in game-map [c r])
                dir (if (< strength (* 5 production))
                      :still
                      (if (> (rand-int 100) 50)
                        :north
                        :west))]]
      [site dir])))

(defn -main []
  (let [{:keys [my-id productions width height game-map]} (io/get-init!)]

    ;; Do any initialization you want with the starting game-map before submitting the bot-name

    (println bot-name)

    (doseq [turn (range)]
      (let [game-map (io/create-game-map width height productions (io/read-ints!))]
        (io/send-moves! (random-moves my-id game-map))))))
