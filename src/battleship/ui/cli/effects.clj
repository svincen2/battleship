(ns battleship.ui.cli.effects)

(defn println-d
  [delay-ms & args]
  (let [output (apply str (interpose " " args))]
    (loop [remaining (seq output)]
      (when-not (empty? remaining)
        (Thread/sleep delay-ms)
        (print (first remaining))
        (flush)
        (recur (rest remaining))))
    (println)))

(defn println-block-delayed
  [print-delay line-delay & args]
  (doall
    (map (fn [arg]
           (println-d print-delay arg)
           (Thread/sleep line-delay))
         args)))

