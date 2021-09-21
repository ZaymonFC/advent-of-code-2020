(require
 '[clojure.data.json :as json]
 '[clojure.data.csv :as csv]
 '[camel-snake-kebab.core :as csk]
 '[java-time :as time]
 '[clj-http.client :as client]
 '[clojure.string :refer [split-lines split] :as string]
 '[clojure.set :as set]
 '[common :as common]
 '[malli.core :as m]
 '[hashp.core :refer :all])

;; --- Day 1
(defn sum-is-2020 [xs] (->> xs (apply +) (= 2020)))

(defn day-1-challenge [xs]
  (->> xs
       common/outer-join
       (filter sum-is-2020)
       (take 1)
       (flatten)
       (apply *)))

(-> (common/numbers "1.txt")
    (day-1-challenge))

;; --- Part 2
(defn day-1-part-2 [xs]
  (->> xs
       common/take-3
       (filter sum-is-2020)
       (take 1)
       (flatten)
       (apply *)))

(-> (common/numbers "1.txt")
    (day-1-part-2))

;; --- Day 2

;; 1-3 a: abcde
;; 1-3 b: cdefg
;; 2-9 c: ccccccccc

;; Each line gives the password policy and then the password. 
;; The password policy indicates the lowest and highest number of times 
;; a given letter must appear for the password to be valid. 

(defn parse-password-line [s]
  (let [[range [letter _] password] (common/split s #" ")
        [min max] (map common/parse-int (common/split range #"-"))]
    {:min min :max max :letter letter :password password}))

(defn count-letter [letter xs]
  (->> xs
       (filter (fn [x] (= x letter)))
       count))

(defn valid-password [{:keys [min max letter password]}]
  (let [letter-count (count-letter letter password)]
    (<= min letter-count max)))

(defn valid-password-2 [{:keys [min max letter password]}]
  (let [pos-1 (nth password (dec min))
        pos-2 (nth password (dec max))]
    (->> [pos-1 pos-2]
         (map (fn [x] (= x letter)))
         (filter identity)
         count
         (= 1))))

(defn day-2-1 [check-fn entries]
  (->> entries
       (map parse-password-line)
       (filter check-fn)
       count))

(->> (common/lines "2.txt")
     (day-2-1 valid-password))

(->> (common/lines "2.txt")
     (day-2-1 valid-password-2))

;; --- Day 3
(def terrain (common/lines "3.txt"))

(defn whats-at [x y]
  (if-let [row (nth terrain y nil)]
    (let [width (count row)
          repeat-x (mod x width)]
      (nth row repeat-x))
    nil))

(defn traverse-slope [[x y] [step-x step-y] tree-count]
  (let [t (whats-at x y)
        tree? (= t \#)
        next-position [(+ x step-x) (+ step-y y)]
        new-count (+ tree-count (if tree? 1 0))]

    (if (not (nil? t))
      (traverse-slope next-position [step-x step-y] new-count)
      tree-count)))

(->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
     (map (fn [step] (traverse-slope [0 0] step 0)))
     (apply *))

(traverse-slope [0 0] [3 1] 0)


;; --- Day 4
(defn make-passport-record [s]
  (let [pairs (map (fn [x] (split x #":")) (split s #" "))
        record (->> pairs
                    (map (fn [[k v]] [(keyword k) v]))
                    (into {}))]
    (-> record
        (update :ecl keyword)
        (update :eyr common/parse-number)
        (update :byr common/parse-number)
        (update :iyr common/parse-number)
        (dissoc :cid))))

(defn validate-height [height]
  (let [amount (->> height (take-while common/isDigit) (apply str) common/parse-number)
        unit (->> height (take-last 2) (apply str) keyword)]

    (condp = unit
      :cm (<= 150 amount 193)
      :in (<= 59 amount 76)
      false)))

(def Passport
  [:map {:closed true}
   [:byr [:and int? [:>= 1920] [:<= 2002]]]
   [:iyr [:and int? [:>= 2010] [:<= 2020]]]
   [:hgt [:fn validate-height]]
   [:hcl [:re #"^#[a-f0-9]{6}$"]]
   [:pid [:re #"^\d{9}$"]]
   [:ecl [:enum :amb :blu :brn :gry :grn :hzl :oth]]
   [:eyr [:and int? [:>= 2020] [:<= 2030]]]])

(defn validate-passport-record [p] (m/validate Passport p))

(->> (common/lines "4.txt")
     (partition-by empty?)
     (map (fn [line] (reduce (fn [a b] (str a " " b)) line)))
     (filter (complement empty?))
     (map make-passport-record)
     (filter validate-passport-record)
     count)

;; -- Day 5
(defn halve [xs] (split-at (quot (count xs) 2) xs))
(defn upper-half [xs] (second (halve xs)))
(defn lower-half [xs] (first (halve xs)))

(defn space-divider [[rows cols] instruction]
  (case instruction
    \F [(lower-half rows) cols]
    \B [(upper-half rows) cols]
    \R [rows (upper-half cols)]
    \L [rows (lower-half cols)]))

(defn calculate-row-col [rows columns instructions]
  (let [rows (range rows)
        columns (range columns)]
    (flatten (reduce space-divider [rows columns] instructions))))

(defn line->seat-details [line & {:keys [rows columns]
                                  :or {rows 128 columns 8}}]
  (let [[row column] (->> line
                          (calculate-row-col rows columns))]
    {:row row :column column :id (+ (* columns row) column)}))

(line->seat-details "BFFFBBFRRR")

(def ids (->> (common/lines "5.txt")
              (map line->seat-details)
              (map :id)
              sort))

(apply min ids)
(set/difference (set (range 100 862)) (set ids))

;; Day 6
(->> (common/lines "6.txt")
     (partition-by empty?)
     (filter (complement (partial = [""])))
     (map (partial map set))
     (map (partial apply set/intersection))
     (map count)
     (reduce +))

;; Day 7 -- Part 1
(defn words [line] (string/split line #" "))

(defn make-rule-1 [source [amount & colour]]
  {colour [source (Integer/parseInt amount)]})

(defn make-rules-1 [words]
  (let [source (take 2 words)
        targets (partition 3 (drop 2 words))
        make-rule (partial make-rule-1 source)]
    (map make-rule targets)))

(def data-1
  (->> (common/lines "7.txt")
       (map (partial filter (fn [c] (and (not= \, c) (not= \. c)))))
       (map (partial apply str))
       (map words)
       (map (partial filter (partial not= "contain")))
       (map (partial filter (partial not= "bags")))
       (map (partial filter (partial not= "bag")))
       (mapcat make-rules-1)
       (apply merge-with concat)
       (map (fn [[k v]] [k (into {} (map (partial into []) (into [] (partition 2 v))))]))
       (into {})))

(defn get-neighbours [map node]
  (->> (map node)
       (keys)
       (into [])))

(defn explore-1 [data source]
  (loop [explored #{source}
         frontier (get-neighbours data source)]

    (if-let [next (peek frontier)]
      (if (explored next)
        ;; We've seen `next` before. Pop next -> frontier and recurse
        (recur explored (pop frontier))

        ;; Push next -> seen, 
        ;; Get next -> neighbours, 
        ;; Pop next from frontier, recurse
        (let [neighbours (get-neighbours data next)]
          (recur (conj explored next) (apply conj neighbours (pop frontier)))))

      ;; No nodes left to explore, return explored nodes
      explored)))

(def shiny-gold ["shiny" "gold"])
(dec (count (explore-1 data-1 shiny-gold)))

;; --- Day 7 Part 2
(defn make-rule [[amount & colour]]
  [colour (Integer/parseInt amount)])

(defn make-rules [words]
  (let [source (take 2 words)
        targets (partition 3 (drop 2 words))]
    {source (into [] (map make-rule targets))}))

(def data
  (->> (common/lines "7.txt")
       (map (partial filter (fn [c] (and (not= \, c) (not= \. c)))))
       (map (partial apply str))
       (map words)
       (map (partial filter (partial not= "contain")))
       (map (partial filter (partial not= "bags")))
       (map (partial filter (partial not= "bag")))
       (mapcat make-rules)
       (into {})))

(defn count-bags [data bag]
  (if-let [children (data bag)]
    (->> children
         (map (fn [[bag value]] (+ value (* value (count-bags data bag)))))
         (apply +))
    0))

(data ["shiny" "gold"])
(data ["vibrant" "bronze"])

(count-bags data shiny-gold)

;; --- Day 8
(def data
  (->> (common/lines "8.txt")
       (map words)
       (map (fn [[op val]] [op (Integer/parseInt val)]))
       (into [])))

(defn process [data]
  (loop [position 0 acc 0 executed? #{}]
    (cond
      (executed? position) executed?
      (= position (count data)) [:end acc]
      :else
      (let [[op operand] (nth data position)
            executed? (conj executed? position)]
        (case op
          "nop" (recur (inc position) acc executed?)
          "acc" (recur (inc position) (+ acc operand) executed?)
          "jmp" (recur (+ position operand) acc executed?))))))

(defn flip-operator [[op operand]]
  (case op
    "nop" ["jmp" operand]
    "jmp" ["nop" operand]
    "acc" [op operand]))

(->> data
     (map-indexed (fn [idx _row] (update data idx flip-operator)))
     (map process)
     (drop-while (complement vector?))
     first)

;; Day 9

;; Write a function that returns a sequence of a rolling buffer of:
;; - buffer-size values
;; - the next value
(defn buffer-next
  ([source buffer-size] (buffer-next source buffer-size 0))
  ([source buffer-size position]
   (if (>= (+ position buffer-size) (count source))
     nil
     (let [buffer (->> source (drop position) (take buffer-size))
           next (->> source (drop (+ position buffer-size)) first)]
       (lazy-seq
        (cons
         [buffer next]
         (buffer-next source buffer-size (inc position))))))))

(defn is-sum-of [[xs n]]
  (let [combinations (->> (for [x xs y xs] (if (= x y) nil [x y]))
                          (filter (complement nil?))
                          (filter (fn [[x y]] (not (= x y)))))]
    (->> combinations
         (filter (fn [[x y]] (= (+ x y) n)))
         empty?
         not)))

(def data
  (->> (common/lines "9.txt")
       (map common/parse-number)))

(def result
  (->> (buffer-next data 25)
       (drop-while is-sum-of)
       first
       second))

;; Part 2

;; An efficient algorithm for determining sub-array sum
;; If all the numbers are positive, 
;; - if a sub-array sum, exceeds target sum -> abandon

;; Imagine a sliding window
; 1 2 >[3 4 5]-> 6 7
;; If the sum of the sliding window exeeds target, shrink left
;; 1 2 3 >[4 5]-> 6 7
;; If the sum of the sliding window is less than target, expand right
;; 1 2 3 >[4 5 6]-> 7

(defn sub-array-sum [source target-sum]
  (let [n (count source)]
    (loop [start 0 finish 2]
      (if (= finish n)
        nil
        (let [arr (subvec source start finish)
              sum (apply + arr)]
          (cond
            (< sum target-sum) (recur start (inc finish))
            (< target-sum sum) (recur (inc start) finish)
            :else arr))))))

(def min-max (juxt (partial apply min) (partial apply max)))

(apply + (min-max (sub-array-sum (into [] data) result)))
