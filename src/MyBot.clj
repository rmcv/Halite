(ns MyBot
  (:gen-class)
  (:require [taoensso.timbre.appenders.core :as appenders]
            [taoensso.timbre :as timbre
             :refer [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]))



(def bot-name "CLJ_RMCV")

(defn same-team? [src target]
  (= (:owner src) (:owner target)))

(defn is-neutral? [target]
  (zero? (:owner target)))

(defn is-opponent? [src target]
  (and (not= (:owner src) (:owner target))
       (not (is-neutral? src))
       (not (is-neutral? target))))

#_(defn distance [width height {x1 :x y1 :y} {x2 :x y2 :y}]
    (let [dx (Math/abs (- x1 x2))
          dy (Math/abs (- y1 y2))
          dx (if (> dx (/ width 2))
               (- width dx)
               dx)
          dy (if (> dy (/ height 2))
               (- height dy)
               dy)]
      (+ dx dy)))

(defn get-direction [width height {fromx :x fromy :y} {tox :x toy :y}]
  (let [h2 (/ height 2)
        w2 (/ width 2)]
    (cond
      (= fromx tox) (cond
                      (and (< fromy toy)
                           (< (- toy fromy) h2))  :south
                      (and (< fromy toy)
                           (>= (- toy fromy) h2)) :north
                      (and (< toy fromy)
                           (< (- fromy toy) h2))  :north
                      :else                       :south)
      :else         (cond
                      (and (< fromx tox)
                           (< (- tox fromx) w2))  :east
                      (and (< fromx tox)
                           (>= (- tox fromx) w2)) :west
                      (and (< tox fromx)
                           (< (- fromx tox) w2))  :west
                      :else                       :east))))

(defn strong-enough? [{:keys [production strength]}]
  (or (zero? production)
      (> strength (* 5 production))))

(defn find-mvs-dir [game-map site border?]
  {:post [(do
            (info site border? %)
            true)]}
  (let [get-dirs (fn [dir]
                   (->> (iterate #(game/adjacent-site game-map % dir) site)
                        (take (if border? 5 100))
                        (take-while #(or border? (is-opponent? %) (is-neutral? %)))
                        (map #(/ (:production %) (:strength %)))
                        (reduce +)))]
    (->> game/cardinal-directions
         (map get-dirs)
         (zipmap game/cardinal-directions)
         (group-by val)
         (sort-by (comp - first))
         first
         last
         rand-nth
         first)))

(defn get-dir [game-map {:keys [opponents teams neutral]} site]
  (let [width         (count (first game-map))
        height        (count game-map)
        get-targets   (fn [dir] (->> (iterate #(game/adjacent-site game-map % dir) site)
                                     (take 2)))
        get-opponents (comp (fn [targets]
                              (->> (filter (partial is-opponent? site) targets)
                                   (filter #(< (:strength %) (:strength site)))))
                            get-targets)
        get-neighbour (comp first get-targets)
        opps          (zipmap game/cardinal-directions
                              (map (comp count get-opponents) game/cardinal-directions))]
    (cond

      ;; some opponents we can defeat
      (some seq (vals opps))
      (->> opps
           (group-by val)  ;; group by number of opponents
           (sort-by (comp - first))  ;; more opponents first
           first
           last ;; get list of [dir cnt]
           rand-nth
           first)

      ;; surrounded by teammates and strong enough to move
      (and (every? same-team? (map get-neighbour game/cardinal-directions))
           (strong-enough? site))
      (if-let [target (->> opponents
                           (filter (comp (partial < 6) :strength))
                           (filter #(< (game/distance game-map site %) (/ width 4)))
                           (group-by (comp - (partial game/distance game-map site)))
                           first
                           last
                           rand-nth)]
        (get-direction width height site target)
        (if-let [mvs (find-mvs-dir game-map site false)]
          mvs
          :still))

      :else (if-let [mvs (find-mvs-dir game-map site true)]
              mvs
              :still))))

(defn move
  [my-id game-map]
  (let [sites (flatten game-map)
        groups (reduce #(update %1
                                (cond
                                  (= my-id (:owner %)) :teammates
                                  (= 0 (:owner %)) :neutral
                                  :else :opponents)
                                conj %2)
                       {:teams []
                        :opponents []
                        :neutral []}
                       sites)
        teams (:teams groups)]
    (zipmap teams
            (map (partial get-dir game-map groups)
                 teams))))

(defn -main []
  (timbre/merge-config!
   {:appenders {:spit (appenders/spit-appender {:fname "app.log"})}})

  (info "Starting...")

  (let [{:keys [my-id productions width height game-map]} (io/get-init!)]

    ;; Do any initialization you want with the starting game-map before submitting the bot-name

    (println bot-name)

    (doseq [turn (range)]
      (let [game-map (io/create-game-map width height productions (io/read-ints!))]
        (io/send-moves! (move my-id game-map))))))

