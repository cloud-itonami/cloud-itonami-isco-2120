(ns quant.governor-test
  (:require [clojure.test :refer [deftest is testing]]
            [quant.store :as store]
            [quant.governor :as governor]))

(defn- fresh-store []
  (let [st (store/mem-store)]
    (store/register-client! st {:client-id "client-1" :name "Kobo Trade"})
    (store/register-model! st {:model-id "M-1" :client-id "client-1"
                               :name "churn-model"
                               :significance-threshold 0.05
                               :approved-methods #{"logistic-regression" "cox-ph"}})
    st))

(defn- analyze [p method claim]
  {:op :approve-analysis :effect :propose :model-id "M-1"
   :p-value p :method method :claim claim :confidence 0.9 :stake :low})

(def ^:private req {:client-id "client-1"})

(deftest ok-significant-within-threshold-and-approved-method
  (let [st (fresh-store)
        v (governor/check req {} (analyze 0.03 "logistic-regression" :significant) st)]
    (is (:ok? v))))

(deftest ok-at-exact-threshold
  (testing "p-value exactly equal to alpha is within margin"
    (let [st (fresh-store)
          v (governor/check req {} (analyze 0.05 "cox-ph" :significant) st)]
      (is (:ok? v)))))

(deftest ok-non-significant-claim-ignores-threshold
  (testing "a non-significant claim does not trigger the threshold check"
    (let [st (fresh-store)
          v (governor/check req {} (analyze 0.80 "cox-ph" :not-significant) st)]
      (is (:ok? v)))))

(deftest hard-on-significance-threshold-violated
  (testing "significance claims are arithmetic, not a judgement call"
    (let [st (fresh-store)
          v (governor/check req {} (assoc (analyze 0.20 "logistic-regression" :significant)
                                          :confidence 0.99) st)]
      (is (:hard? v))
      (is (some #(= :significance-threshold-violated (:rule %)) (:violations v))))))

(deftest hard-on-unapproved-method
  (let [st (fresh-store)
        v (governor/check req {} (analyze 0.03 "p-hacked-anova" :significant) st)]
    (is (:hard? v))
    (is (some #(= :unapproved-method (:rule %)) (:violations v)))))

(deftest hard-on-unknown-model
  (testing "renamed :unknown-model -> :unusable-model: the rule now covers a
            model that is registered but carries no alpha or no approved
            methods, which is unusable for the same reason and used to throw"
    (let [st (fresh-store)
          v (governor/check req {} (assoc (analyze 0.03 "logistic-regression" :significant)
                                          :model-id "M-ghost") st)]
      (is (:hard? v))
      (is (some #(= :unusable-model (:rule %)) (:violations v))))))

(deftest hard-on-foreign-model
  (let [st (fresh-store)]
    (store/register-client! st {:client-id "client-2" :name "Other"})
    (let [v (governor/check {:client-id "client-2"} {} (analyze 0.03 "logistic-regression" :significant) st)]
      (is (:hard? v))
      (is (some #(= :model-wrong-client (:rule %)) (:violations v))))))

(deftest hard-on-unregistered-client
  (let [st (fresh-store)
        v (governor/check {:client-id "nobody"} {} (analyze 0.03 "logistic-regression" :significant) st)]
    (is (:hard? v))
    (is (some #(= :no-client (:rule %)) (:violations v)))))

(deftest hard-on-no-actuation-violation
  (let [st (fresh-store)
        v (governor/check req {} (assoc (analyze 0.03 "logistic-regression" :significant)
                                        :effect :direct-write) st)]
    (is (:hard? v))
    (is (some #(= :no-actuation (:rule %)) (:violations v)))))

(deftest escalates-finding-publication
  (testing "a WELL-FORMED publication escalates to a human"
    (let [st (fresh-store)
          v (governor/check req {} (assoc (analyze 0.03 "cox-ph" :significant)
                                          :op :publish-finding :stake :high) st)]
      (is (not (:hard? v)))
      (is (:escalate? v)))))

(deftest publication-is-not-exempt-from-the-model-invariants
  (testing "This test replaces an assertion that used to pass: a
            :publish-finding naming only a model, with no method, no claim and
            no p-value, escalated to a human with an EMPTY violation list.
            Every model invariant was gated on (= :approve-analysis op), so
            the one operation whose premise is that a number leaves the
            building was exempt from all four. See quant.operation."
    (let [st (fresh-store)]
      (doseq [[label prop]
              [[:under-specified {:op :publish-finding :effect :propose
                                  :model-id "M-1" :confidence 0.9 :stake :high}]
               [:unregistered-model (assoc (analyze 0.03 "cox-ph" :significant)
                                           :op :publish-finding :model-id "M-ghost")]
               [:unapproved-method (assoc (analyze 0.03 "p-hacked-anova" :significant)
                                          :op :publish-finding)]
               [:over-threshold (assoc (analyze 0.9 "cox-ph" :significant)
                                       :op :publish-finding)]]]
        (let [v (governor/check req {} prop st)]
          (is (:hard? v) (str label " must hold, not escalate"))
          (is (seq (:violations v))
              (str label " must reach a human with a non-empty violation list, or not reach one at all")))))))

(deftest escalates-low-confidence
  (let [st (fresh-store)
        v (governor/check req {} (assoc (analyze 0.03 "logistic-regression" :significant)
                                        :confidence 0.3) st)]
    (is (not (:hard? v)))
    (is (:escalate? v))))

;; --- the vocabulary, the arithmetic and the value checks that were absent ---

(deftest hard-on-undeclared-and-reserved-operations
  (testing "the governor bound two ops and admitted everything else clean"
    (let [st (fresh-store)]
      (doseq [op [:some-new-op nil]]
        (let [v (governor/check req {} (assoc (analyze 0.03 "cox-ph" :significant) :op op) st)]
          (is (:hard? v) (str op " must be refused as undeclared"))
          (is (some #(= :undeclared-operation (:rule %)) (:violations v)))))
      (doseq [op [:sign-actuarial-certificate :set-reserves :issue-policy-quote
                  :delete-model :disable-audit-logging]]
        (let [v (governor/check req {} (assoc (analyze 0.03 "cox-ph" :significant) :op op) st)]
          (is (:hard? v) (str op " must be refused as reserved"))
          (is (some #(= :reserved-operation (:rule %)) (:violations v))
              "a reserved op is an authority boundary, never an escalation"))))))

(deftest hard-on-unusable-p-value
  (testing "the comparison was guarded by (number? p-value), so an unreadable
            p-value skipped the arithmetic and committed clean"
    (let [st (fresh-store)]
      (doseq [p ["0.9" nil -3 2]]
        (let [v (governor/check req {} (analyze p "cox-ph" :significant) st)]
          (is (:hard? v) (str "p-value " (pr-str p) " must refuse"))
          (is (some #(= :unusable-p-value (:rule %)) (:violations v))))))))

(deftest hard-on-stronger-and-reserved-claim-words
  (testing "the check fired on the literal :significant, so a stronger word bypassed it"
    (let [st (fresh-store)
          conclusive (governor/check req {} (analyze 0.9 "cox-ph" :conclusive) st)
          proven (governor/check req {} (analyze 0.001 "cox-ph" :proven) st)
          unknown (governor/check req {} (analyze 0.9 "cox-ph" :basically-certain) st)]
      (is (some #(= :significance-threshold-violated (:rule %)) (:violations conclusive))
          ":conclusive asserts significance, so it takes the same arithmetic")
      (is (some #(= :reserved-claim (:rule %)) (:violations proven))
          "no alpha makes :proven true, so it is refused rather than compared")
      (is (some #(= :undeclared-claim (:rule %)) (:violations unknown))))))

(deftest hard-on-undeclared-method
  (testing "naming no method satisfied 'the method is registered' vacuously"
    (let [st (fresh-store)
          v (governor/check req {} (analyze 0.03 nil :significant) st)]
      (is (:hard? v))
      (is (some #(= :undeclared-method (:rule %)) (:violations v))))))

(deftest hard-on-model-registered-without-alpha-or-methods
  (testing "a model with no :significance-threshold used to THROW a
            NullPointerException from inside the check; a check that throws
            has not refused"
    (let [st (fresh-store)]
      (store/register-model! st {:model-id "M-NOALPHA" :client-id "client-1"
                                 :approved-methods #{"cox-ph"}})
      (store/register-model! st {:model-id "M-NOMETHODS" :client-id "client-1"
                                 :significance-threshold 0.05})
      (doseq [id ["M-NOALPHA" "M-NOMETHODS"]]
        (let [v (governor/check req {} (assoc (analyze 0.04 "cox-ph" :significant)
                                              :model-id id) st)]
          (is (:hard? v) (str id " must refuse rather than throw"))
          (is (some #(= :unusable-model (:rule %)) (:violations v))))))))

(deftest hard-on-unusable-confidence
  (testing "confidence was compared against the floor without being on its scale"
    (let [st (fresh-store)]
      (doseq [c [5.0 -1 "high" nil]]
        (let [v (governor/check req {} (assoc (analyze 0.03 "cox-ph" :significant)
                                              :confidence c) st)]
          (is (:hard? v) (str "confidence " (pr-str c) " must refuse"))
          (is (some #(= :unusable-confidence (:rule %)) (:violations v))))))))

(deftest boundary-p-value-equals-alpha
  (testing "the registered alpha is the LARGEST p-value that still supports the
            claim, so the line conforms and the smallest step above it does
            not. These two assertions are what make the comparison operator
            visible: flipping <= to < in quant.facts/significant? turns the
            first red and leaves the rest of this suite green."
    (let [st (fresh-store)]
      (is (:ok? (governor/check req {} (analyze 0.05 "cox-ph" :significant) st)))
      (is (:hard? (governor/check req {} (analyze 0.0500001 "cox-ph" :significant) st))))))

(deftest a-refusal-names-every-reason
  (testing "hard violations are collected in full, not short-circuited at the
            first: a reviewer reading the ledger sees every reason the
            proposal was refused"
    (let [st (fresh-store)
          v (governor/check req {} (analyze 0.9 "p-hacked-anova" :significant) st)]
      (is (:hard? v))
      (is (<= 2 (count (:violations v)))
          "an unapproved method AND an over-threshold claim are both reported"))))
