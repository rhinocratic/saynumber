(ns rhinocratic.saynumber
  (:require [clojure.string :as str])
  (:gen-class))

(def group-suffixes
  [""
   " thousand"
   " million"
   " billion"
   " trillion"
   " quadrillion"
   " quintillion"
   " sextillion"])

(def units
  {\0 "zero"
   \1 "one"
   \2 "two"
   \3 "three"
   \4 "four"
   \5 "five"
   \6 "six"
   \7 "seven"
   \8 "eight"
   \9 "nine"})

(def teens
  {[\1 \0] "ten"
   [\1 \1] "eleven"
   [\1 \2] "twelve"
   [\1 \3] "thirteen"
   [\1 \4] "fourteen"
   [\1 \5] "fifteen"
   [\1 \6] "sixteen"
   [\1 \7] "seventeen"
   [\1 \8] "eighteen"
   [\1 \9] "nineteen"})

(def decades
  {\2 "twenty"
   \3 "thirty"
   \4 "forty"
   \5 "fifty"
   \6 "sixty"
   \7 "seventy"
   \8 "eighty"
   \9 "ninety"})

(defn to-groups
  "Break the digits of a number into groups of 3, starting at the least significant digit
   and padding with leading zeros when necessary.  Returns the groups in order from most to
   least significant."
  [digits]
  (->> digits
       reverse
       (partition 3 3 (repeat \0))
       (map vec)
       (map reverse)
       (map-indexed (fn [i n] [n (group-suffixes i)]))
       reverse))

(defn digit-zero?
  [digit]
  (= \0 digit))

(defn digits-all-zero? [digits]
  (every? digit-zero? digits))

(defn has-hundreds? [[x _y _z]]
  (not (digit-zero? x)))

(defn has-internal-conjunction?
  "Returns true if the triplet of digits should be spoken with a conjuction
   (e.g. [one hundred and thirty-two]), false otherwise ([three hundred])."
  [[_x y z :as digits]]
  (and
   (has-hundreds? digits)
   (not (digits-all-zero? [y z]))))

(defn has-group-conjunction?
  "Returns true if a triplet of digits with the given prefix and suffix should
   be spoken with a leading \"and\" (e.g. one thousand [and thirty]), false otherwise
   (e.g. one thousand [one hundred and thirty])."
  [digits prefix suffix]
  (and
   (str/blank? suffix)
   (not (str/blank? prefix))
   (not (has-internal-conjunction? digits))))

(defn group-needs-space?
  "Returns true if a triplet of digits with the given prefix and suffix should
   be preceded by a space (e.g. one million [one thousand] and thirty)."
  [digits prefix suffix]
  (and
   (not (has-group-conjunction? digits prefix suffix))
   (not (str/blank? prefix))
   (not (digits-all-zero? digits))))

(defn has-teen?
  "Returns true if a triplet of digits ends in a number from 10 - 19."
  [[_x y _z]]
  (= \1 y))

(defn has-tens?
  "Returns true if a triplet of digits has a tens digit other than 0 or 1."
  [[_x y _z :as digits]]
  (and
   (not (has-teen? digits))
   (not= \0 y)))

(defn has-units?
  "Returns true if a triplet of digits ends in a non-zero digit."
  [[_x _y z :as digits]]
  (and
   (not (has-teen? digits))
   (not= \0 z)))

(defn hyphenated?
  "Returns true if a triplet of digits contains hyphenated tens and units
   (e.g. forty-two), false otherwise (e.g. thirteen, forty)."
  [[_x y z :as digits]]
  (and
   (not (has-teen? digits))
   (not= \0 y)
   (not= \0 z)))

(defn is-zero?
  "Returns true if the whole number (digits with prefix and suffix) is zero."
  [digits prefix suffix]
  (and
   (str/blank? prefix)
   (str/blank? suffix)
   (digits-all-zero? digits)))

(defn say-suffix?
  "Returns true if the suffix of the given triplet of digits should be spoken
   (e.g. [one hundred and fifty thousand] and three), false otherwise (e.g. [nine])"
  [digits suffix]
  (and
   (not (digits-all-zero? digits))
   (not (str/blank? suffix))))

(defn say-group
  "Returns the \"spoken\" representation of a triplet of digits given its context
   (the part of the spoken number preceding it and group suffix immediately following it)."
  [prefix [[x y z :as digits] suffix]]
  (cond-> prefix
    (group-needs-space? digits prefix suffix) (str " ")
    (has-group-conjunction? digits prefix suffix) (str " and ")
    (has-hundreds? digits) (str (units x) " hundred")
    (has-internal-conjunction? digits) (str " and ")
    (has-teen? digits) (str (teens [y z]))
    (has-tens? digits) (str (decades y))
    (hyphenated? digits) (str "-")
    (has-units? digits) (str (units z))
    (say-suffix? digits suffix) (str suffix)
    (is-zero? digits prefix suffix) (str "zero")))

(defn negate-if-necessary
  "Prefix a spoken number with \"minus\" if necessary."
  [n as-words]
  (cond->> as-words
    (neg-int? n) (str "minus ")))

(defn say-number
  "Express a number in words.  The number must be in the range Long/MIN_VALUE <= n <= Long/MAX_VALUE."
  [n]
  (->> n
       abs
       str
       (#(str/replace % "-" ""))
       to-groups
       (reduce say-group "")
       (negate-if-necessary n)))

(defn -main
  "Express a number in words.  The number must be in the range Long/MIN_VALUE <= n <= Long/MAX_VALUE."
  [& args]
  (try
    (-> args
        first
        parse-long
        say-number
        println)
    (catch Exception _e
      (println (str "Unable to interpret " (first args) " as a Long integer value")))))

