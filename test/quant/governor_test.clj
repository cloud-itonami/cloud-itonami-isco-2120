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
  (let [st (fresh-store)
        v (governor/check req {} (assoc (analyze 0.03 "logistic-regression" :significant)
                                        :model-id "M-ghost") st)]
    (is (:hard? v))
    (is (some #(= :unknown-model (:rule %)) (:violations v)))))

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
  (let [st (fresh-store)
        v (governor/check req {} {:op :publish-finding :effect :propose
                                  :model-id "M-1" :confidence 0.9 :stake :high} st)]
    (is (not (:hard? v)))
    (is (:escalate? v))))

(deftest escalates-low-confidence
  (let [st (fresh-store)
        v (governor/check req {} (assoc (analyze 0.03 "logistic-regression" :significant)
                                        :confidence 0.3) st)]
    (is (not (:hard? v)))
    (is (:escalate? v))))
