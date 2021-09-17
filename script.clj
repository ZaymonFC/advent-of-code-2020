(require
 '[clojure.data.json :as json]
 '[clojure.data.csv :as csv]
 '[camel-snake-kebab.core :as csk]
 '[java-time :as time]
 '[clj-http.client :as client]
 '[clojure.string :refer [split-lines split] :as string]
 '[clojure.set :as set]
 '[common :as common]
 '[malli.core :as m])

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
