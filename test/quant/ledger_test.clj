(ns quant.ledger-test
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [quant.ledger :as ledger]))

(defn- three []
  (-> []
      (ledger/append (ledger/commit-entry {:op :approve-analysis :p-value 0.01} :actor))
      (ledger/append (ledger/hold-entry {:hard? true :violations [{:rule :unapproved-method}]}))
      (ledger/append (ledger/commit-entry {:op :publish-finding :p-value 0.01} :human))))

(deftest an-intact-chain-verifies
  (let [l (three)]
    (is (= 3 (count l)))
    (is (= [0 1 2] (mapv :ledger/seq l)))
    (is (:ok? (ledger/verify l)))
    (is (= 3 (:length (ledger/verify l))))
    (is (:ok? (ledger/verify [])) "an empty ledger is intact, not broken")))

(deftest a-mutated-entry-breaks-the-chain
  (testing "'append-only' was a property of the code path, not of the
            artifact — and the code path is exactly what an audit is checking"
    (let [l (three)
          tampered (assoc-in l [1 :verdict] {:hard? false})
          v (ledger/verify tampered)]
      (is (not (:ok? v)))
      (is (= 1 (:broken-at v)))
      (is (= :hash-mismatch (:reason v))))))

(deftest a-dropped-entry-breaks-the-chain
  (testing "any prefix or permutation used to be indistinguishable from the
            real thing. Removing a middle entry must now be detectable."
    (let [l (three)
          dropped (vec (concat [(nth l 0)] [(nth l 2)]))
          v (ledger/verify dropped)]
      (is (not (:ok? v)))
      (is (= 1 (:broken-at v)))
      (is (= :seq-mismatch (:reason v))))))

(deftest a-reordered-ledger-breaks-the-chain
  (let [l (three)
        swapped [(nth l 0) (nth l 2) (nth l 1)]
        v (ledger/verify swapped)]
    (is (not (:ok? v)))
    (is (contains? #{:seq-mismatch :prev-mismatch :hash-mismatch} (:reason v)))))

(deftest truncation-at-the-end-is-NOT-detected-and-says-so
  (testing "A chain cannot detect entries it never saw; detecting truncation
            needs an external anchor this in-memory store does not have. This
            asserts the documented LIMIT, so that if someone later claims the
            ledger is tamper-evident against truncation, this test is what
            contradicts them."
    (is (:ok? (ledger/verify (vec (butlast (three))))))))

(deftest the-hash-commits-to-position-not-only-content
  (testing "two identical entries at different positions must hash differently,
            or a reorder of equal-looking entries would be invisible"
    (let [m (ledger/commit-entry {:op :approve-analysis} :actor)
          l (-> [] (ledger/append m) (ledger/append m))]
      (is (not= (:ledger/hash (nth l 0)) (:ledger/hash (nth l 1)))))))

(deftest chain-hash-is-in-range-and-deterministic
  (is (= (ledger/chain-hash 0 {:a 1}) (ledger/chain-hash 0 {:a 1})))
  (is (not= (ledger/chain-hash 0 {:a 1}) (ledger/chain-hash 1 {:a 1})))
  (doseq [content [{} {:a 1} {:x "long string here" :y [1 2 3]}]]
    (let [h (ledger/chain-hash 7 content)]
      (is (<= 0 h))
      (is (< h 2147483647)))))

(deftest approval-provenance-is-always-recorded
  (testing "recording :actor explicitly rather than omitting the key keeps the
            two cases the same shape, so a reader cannot mistake an absent
            field for an unaudited one"
    (is (= :actor (:approved-by (ledger/commit-entry {} :actor))))
    (is (= :human (:approved-by (ledger/commit-entry {} :human))))
    (is (= :none (:approved-by (ledger/hold-entry {}))))
    (doseq [e (three)]
      (is (contains? e :approved-by)))))

(deftest summary-names-every-entry
  (let [s (ledger/summary (three))]
    (is (re-find #"approved-by=actor" s))
    (is (re-find #"approved-by=human" s))
    (is (re-find #"approved-by=none" s))
    (is (= 3 (count (str/split-lines s))))))
