(ns rhinocratic.saynumber-test
  (:require [clojure.test :refer [deftest testing is]]
            [rhinocratic.saynumber :refer [say-group say-number]]))

(deftest test-say-group
  (testing "Test processing of individual groups"
    (is (= "twenty-one" (say-group "" [[\0 \2 \1] ""])))
    (is (= "zero" (say-group "" [[\0 \0 \0] ""])))
    (is (= "one hundred and twenty-three" (say-group "" [[\1 \2 \3] ""])))
    (is (= "ninety-nine thousand" (say-group "" [[\0 \9 \9] " thousand"])))))

(deftest test-say-number
  (testing "Test numbers of various magnitudes and patterns."
    (is (= "twenty-one" (say-number 21)))
    (is (= "zero" (say-number 0)))
    (is (= "zero" (say-number -0)))
    (is (= "nine" (say-number 9)))
    (is (= "minus nine" (say-number -9)))
    (is (= "eighty" (say-number 80)))
    (is (= "one hundred and twenty-three" (say-number 123)))
    (is (= "nine hundred and eight million thirty-four thousand and five" (say-number 908034005)))
    (is (= "one trillion and nine" (say-number 1000000000009)))
    (is (= "minus one thousand and five" (say-number -1005)))
    (is (= "nine quintillion two hundred and twenty-three quadrillion three hundred and seventy-two trillion thirty-six billion eight hundred and fifty-four million seven hundred and seventy-five thousand eight hundred and seven"
           (say-number Long/MAX_VALUE)))
    (is (= "minus nine quintillion two hundred and twenty-three quadrillion three hundred and seventy-two trillion thirty-six billion eight hundred and fifty-four million seven hundred and seventy-five thousand eight hundred and eight"
           (say-number Long/MIN_VALUE)))))
