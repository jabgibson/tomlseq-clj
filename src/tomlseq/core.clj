(ns tomlseq.core)

(def default-options {:respects :all :start 0 :label "seq"})

(defn table-start? [^String line]
  "decides on whether the line is the start of a table definition or not. If so, returns table name
  if not, returns :false"
  (if (>= (count line) 2)
    (if (= (subs line 0 2) "[[")
      (let [length (count line)]
        (if (and (= (subs line 0 2) "[[") (= "]]" (subs line (- length 2) length)))
          (subs line 2 (- length 2))
          :!table))
      :!table)
    :!table))

(def mls-pattern #"\"\"\"")
(defn update-mls-status
  "processes a line looking for multiline strings, updating a state atom when finding"
  [^String line stat]
  (let [mls-marks (re-seq mls-pattern line)]
    (if (odd? (count mls-marks)) (swap! stat not))))

(defn respect-table? [table options]
  (or (= :all (:respects options)) (contains? (:respects options) table)))

(defn preprocess [^String toml option-overrides]
  (let [opts (merge default-options option-overrides)
        counter (atom (:start opts))
        mls-state (atom false)
        lines (clojure.string/split-lines toml)
        new-toml (atom [])]
    (doseq [line lines]
      (if @mls-state
        (do
          (update-mls-status line mls-state)
          (swap! new-toml conj line))
        (let [table (table-start? line)
              is-table (not= :!table table)]
          (update-mls-status line mls-state)
          (if (and is-table (respect-table? table opts))
            (do
              (swap! new-toml conj line (str (:label opts)  " = " @counter))
              (swap! counter inc))
            (swap! new-toml conj line)))))
    (str (clojure.string/join "\n" @new-toml) "\n")))
