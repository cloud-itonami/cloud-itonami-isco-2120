(ns quant.sim-test
  (:require [clojure.test :refer [deftest is testing]]
            [quant.phase :as phase]
            [quant.sim :as sim]))

(deftest the-scenario-table-passes
  (let [r (sim/run)]
    (is (:ok? r) (sim/report r))
    (is (empty? (:mismatches r)))
    (is (empty? (:wrote-anyway r)) "a refusal that still wrote a record")
    (is (empty? (:ledger-breaks r)))))

(deftest the-table-actually-demonstrates-refusals
  (testing "a harness that ran only clean scenarios would print green while
            demonstrating nothing"
    (let [r (sim/run)]
      (is (pos? (:refusals r)))
      (is (<= 20 (:refusals r))
          "every refusal measured as MISSING on the pre-change tree is in this table"))))

(deftest a-refusal-free-table-is-reported-as-a-failure-not-a-pass
  (testing "This is the rule the harness exists to enforce, exercised against
            a table that triggers it. Without this test the fourth conjunct of
            run's :ok? could be deleted and every other assertion here would
            keep returning the same value — which is precisely the shape
            (a check that cannot fail looking like one that passed) the
            harness is built to refuse."
    (let [clean-only (filterv #(= :commit (:expect %)) sim/scenarios)
          r (sim/run clean-only)]
      (is (seq clean-only) "the fixture must not be empty, or this proves nothing")
      (is (zero? (:refusals r)))
      (is (empty? (:mismatches r)) "the clean scenarios still reach :commit")
      (is (not (:ok? r)) "no mismatch, no ledger break — and still not a pass")
      (is (re-find #"REFUSING TO REPORT A PASS" (sim/report r))))))

(deftest the-table-contains-admissible-scenarios-too
  (testing "a table that refused everything would show the actor cannot act,
            which is a different defect with the same green"
    (let [committing (filterv #(= :commit (:expect %)) sim/scenarios)]
      (is (<= 3 (count committing)))
      (is (every? #(not (phase/refusal? (:expect %))) committing)))))

(deftest the-boundary-pair-is-present-and-opposed
  (testing "p == alpha and the smallest step above it must be in the table
            with OPPOSITE expectations. A comparison whose scenarios are all
            far from the line cannot show which operator it uses."
    (let [by-name (into {} (map (juxt :name identity)) sim/scenarios)]
      (is (= :commit (:expect (by-name :p-value-on-the-line))))
      (is (= :hold (:expect (by-name :p-value-just-over-the-line)))))))

(deftest every-scenario-declares-why-it-is-there
  (doseq [s sim/scenarios]
    (is (keyword? (:name s)))
    (is (contains? #{:commit :hold :request-approval} (:expect s)) (str (:name s)))
    (is (string? (:why s)) (str (:name s)))
    (is (< 20 (count (:why s))) (str (:name s)))))

(deftest scenario-names-are-unique
  (testing "a duplicate name would silently overwrite its twin in any report
            keyed by name, and share a checkpoint thread-id in run-one"
    (is (= (count sim/scenarios)
           (count (set (map :name sim/scenarios)))))))

(deftest report-names-a-failure-as-a-failure
  (let [bogus [{:name :deliberately-wrong
                :request {:client-id "sim-client-1" :op :approve-analysis
                          :model-id "M-1" :method "cox-ph" :claim :significant
                          :p-value 0.9}
                :expect :commit
                :why "a p-value of 0.9 claimed significant cannot commit; this scenario is wrong on purpose"}]
        r (sim/run bogus)]
    (is (not (:ok? r)))
    (is (= 1 (count (:mismatches r))))
    (is (re-find #"FAIL" (sim/report r)))
    (is (re-find #"BAD deliberately-wrong" (sim/report r)))))
