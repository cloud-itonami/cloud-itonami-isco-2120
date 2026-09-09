(ns quant.operation-test
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [quant.operation :as operation]))

(deftest supported-and-reserved-are-disjoint
  (testing "an op in both sets would make the refusal reason depend on clause
            order rather than on what the op is"
    (is (empty? (set/intersection (set (keys operation/supported))
                                          (set (keys operation/reserved)))))))

(deftest undeclared-ops-are-not-declared
  (testing "the vocabulary is an allowlist. Everything the governor was
            measured admitting clean must fall outside it."
    (doseq [op [:some-new-op :delete-everything nil "approve-analysis" 42]]
      (is (not (operation/declared? op)) (str (pr-str op) " must be undeclared)")))))

(deftest every-reserved-op-explains-itself
  (testing "a refusal that cannot say why is one an operator cannot act on"
    (doseq [op (keys operation/reserved)]
      (is (operation/reserved? op))
      (is (operation/declared? op) "reserved is declared: it is an authority boundary, not a typo")
      (is (string? (operation/reserved-reason op)))
      (is (< 20 (count (operation/reserved-reason op)))))))

(deftest both-model-bound-operations-are-marked
  (testing "This is the field that closes the widest measured gap. Before it,
            every model invariant was gated on (= :approve-analysis op), so
            :publish-finding — external publication — was exempt from all
            four. If a future edit drops :model-op? from :publish-finding this
            assertion is what notices."
    (is (operation/model-op? :approve-analysis))
    (is (operation/model-op? :publish-finding))
    (is (not (operation/model-op? :draft-analysis)))
    (is (not (operation/model-op? :flag-model-risk)))))

(deftest reserved-and-undeclared-ops-are-never-model-bound-or-escalating
  (testing "the governor hard-blocks before consulting these, so a true here
            would be an admission reachable by a future reordering"
    (doseq [op (concat (keys operation/reserved) [:some-new-op nil])]
      (is (not (operation/model-op? op)))
      (is (not (operation/escalates? op))))))

(deftest external-and-advisory-operations-escalate
  (is (operation/escalates? :publish-finding))
  (is (operation/escalates? :flag-model-risk))
  (is (not (operation/escalates? :approve-analysis)))
  (is (not (operation/escalates? :draft-analysis))))

(deftest every-supported-op-declares-all-three-properties
  (testing "a missing key reads as false, which is the permissive answer for
            both :escalates? and :model-op?"
    (doseq [[op m] operation/supported]
      (is (contains? m :escalates?) (str op))
      (is (contains? m :model-op?) (str op))
      (is (string? (:summary m)) (str op)))))
