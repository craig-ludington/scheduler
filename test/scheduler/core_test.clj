(ns scheduler.core-test
  (:require [clojure.test :refer :all]
            [scheduler.core :refer :all])
  (use [clj-time.core :as time :only (interval overlaps?)]
       [clj-time.coerce :as coerce :only (from-string to-string)]))

(deftest test-reservation
  (let [from "2014-01-01"
        to   "2014-12-31"
        r    (reservation :a from to)]
    (testing "A reservation has a resource and an interval."
      (is (= (into #{} (keys r)) #{:resource :interval})))
    (testing "Resource is properly assigned."
      (is (= (:resource r) :a)))
    (testing "Interval is properly created."
      (is (= (:interval r) (interval (from-string from) (from-string to)))))))

(deftest test-reserve
  (reset! schedules {})
  (let [from "2014-01-01"
        to   "2014-12-31"
        r    (reservation :a from to)
        _    (reserve r)]
    (testing "Reserve adds the resource as a key in schedules."
      (is (= #{:a} (into #{} (keys @schedules)))))
    (testing "The interval is stored under the resource's schedules."
      (is (= (:a @schedules)
             [(:interval r)])))))

(deftest test-scheduled?
  (let [from      "2014-01-01"
        to        "2014-12-31"
        other-to  "2014-12-30"
        r         (reservation :a from to)
        other-r   (reservation :a from other-to)]
    (testing "scheduled? is false if this identical reservation is not in schedules."
      (is (do (reset! schedules {})
              (not (scheduled? r)))))
    (testing "scheduled? is true if this identical reservation is in schedules."
      (is (do (reset! schedules {})
              (reserve r)
              (scheduled? r))))
    (testing "scheduled? is false if a similar but different reservation is in schedules."
      (is (not (do (reset! schedules {})
                   (reserve r)
                   (scheduled? other-r)))))))

(deftest test-available?
  (testing "available? is true when a reservation could be made."
    (do
      (reset! schedules {})
      (reserve (reservation :a "2014-01-01" "2014-12-31"))
      (is (available? (reservation :a "2013-01-01" "2013-12-31")))
      (is (available? (reservation :a "2015-01-01" "2015-12-31")))))
  (testing "available? is false when a reservation would overlap another reservation."
    (do
      (reset! schedules {})
      (reserve (reservation :a "2014-01-01" "2014-12-31"))
      (is (not (available? (reservation :a "2014-01-01" "2014-12-31"))))
      (is (not (available? (reservation :a "2014-01-01" "2014-01-03"))))
      (is (not (available? (reservation :a "2013-12-31" "2014-01-02"))))
      (is (not (available? (reservation :a "2014-12-30" "2015-01-01")))))))

(deftest test-release
  (testing "release removes a reservation from schedules."
    (let [r     (reservation :a "2014-01-01" "2014-12-31")
          other (reservation :a "2015-01-01" "2015-12-31")]
      (reset! schedules {})
      (reserve r)
      (is (scheduled? r))
      (reserve other)
      (is (scheduled? other))
      (release r)
      (is (not (scheduled? r)))
      (is (scheduled? other)))))
