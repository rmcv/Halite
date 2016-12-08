(ns Moves
  (:require game))

(defn heuristic [game-map {:keys [owner strength production]
                           :as   site}]
  (if (and owner (zero? owner) (pos? strength))
    (/ production strength)
    (->> (map #(game/adjacent-site game-map site %) game/directions)
         (filter (fn [{o :owner}] (not= o owner)))
         (map :strength)
         (reduce +))))

(defn find-nearest-enemy-dir [game-map site]
  (let [max-dist (/ (min (count (first game-map)) (count game-map)) 2)
        paths    (map #(->> site
                            (iterate (fn [s] (game/adjacent-site game-map s %)))
                            (take-while (fn [{o :owner}] (= o (:owner site))))
                            (take max-dist))
                      game/directions)]
    (first (apply min-key
                  (fn [[dir paths]] (count paths))
                  (zipmap game/directions paths)))))

(defn improve-moves-2
  [my-id game-map]
  (for [{:keys [production strength owner]
         :as   site} (flatten game-map)
        :when        (= my-id owner)
        :let         [dir (let [nbs     (map #(assoc
                                               (game/adjacent-site game-map site %)
                                               :dir %) game/directions)
                                targets (->> nbs
                                             (remove (fn [{o :owner}] (= o my-id))))
                                target  (->> targets
                                             (sort-by #(heuristic game-map %))
                                             last)]
                            (cond
                              (and target
                                   (< (:strength target) strength)) (:dir target)
                              (< strength (* 5 production))         :still
                              (empty targets)                       (find-nearest-enemy-dir game-map site)
                              :else                                 :still))]]
    [site dir]))

(defn improve-moves-1
  "Takes a 2D vector of sites and returns a list of [site, direction] pairs"
  [my-id game-map]
  (for [{:keys [production strength owner]
         :as   site} (flatten game-map)
        :when      (= my-id owner)
        :let       [dir (if (< strength (* 5 production))
                          :still
                          (if (> (rand-int 100) 50)
                            :north
                            :west))]]
    [site dir]))

(defn random-moves
  "Takes a 2D vector of sites and returns a list of [site, direction] pairs"
  [my-id game-map]
  (let [my-sites (->> game-map
                      flatten
                      (filter #(= (:owner %) my-id)))]
    (map vector my-sites (repeatedly #(rand-nth game/directions)))))
