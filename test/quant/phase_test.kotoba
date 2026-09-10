(ns quant.phase-test
  (:require [clojure.test :refer [deftest is testing]]
            [quant.phase :as phase]))

(deftest hard-beats-escalate
  (testing "A verdict carrying BOTH flags must hold, not escalate: escalating
            it would put a question to a human that they have no authority to
            answer yes to. quant.governor already zeroes :escalate? when
            anything is hard, so this is the second of two independent guards
            — and the only one a caller building a verdict by hand gets.
            Reversing the two clauses in of-verdict leaves quant.sim entirely
            green, which is why this assertion lives here and not there."
    (is (= :hold (phase/of-verdict {:hard? true :escalate? true})))
    (is (= :hold (phase/of-verdict {:hard? true})))
    (is (= :request-approval (phase/of-verdict {:escalate? true})))
    (is (= :commit (phase/of-verdict {})))
    (is (= :commit (phase/of-verdict {:hard? false :escalate? false})))))

(deftest only-commit-writes
  (is (phase/writes? :commit))
  (is (not (phase/writes? :hold)))
  (is (not (phase/writes? :request-approval))
      "an escalation has not been approved yet; it must not have written"))

(deftest both-non-writing-phases-are-refusals
  (testing ":request-approval is a refusal to act without a human, not an
            approval-in-waiting. quant.sim counts these; if it stopped
            counting one of them a table could report refusals it never made."
    (is (phase/refusal? :hold))
    (is (phase/refusal? :request-approval))
    (is (not (phase/refusal? :commit)))))

(deftest only-escalation-requires-a-human
  (is (phase/human-required? :request-approval))
  (is (not (phase/human-required? :commit)))
  (is (not (phase/human-required? :hold))))

(deftest approval-provenance-is-derived-from-the-escalated-disposition
  (is (phase/approved-commit? :request-approval))
  (is (not (phase/approved-commit? :commit)))
  (is (not (phase/approved-commit? :hold)))
  (is (not (phase/approved-commit? nil))))

(deftest an-unknown-phase-is-not-permitted-anything
  (testing "predicates read a missing entry as false, so an unknown phase
            neither writes nor is trusted"
    (is (not (phase/writes? :no-such-phase)))
    (is (not (phase/human-required? :no-such-phase)))
    (is (not (phase/terminal? :no-such-phase)))))
