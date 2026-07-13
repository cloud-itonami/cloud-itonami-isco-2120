(ns quant.actor-test
  (:require [clojure.test :refer [deftest is testing]]
            [quant.actor :as actor]
            [quant.store :as store]))

(defn- fresh-store []
  (let [st (store/mem-store)]
    (store/register-client! st {:client-id "client-1" :name "Kobo Trade"})
    (store/register-model! st {:model-id "M-1" :client-id "client-1"
                               :name "churn-model"
                               :significance-threshold 0.05
                               :approved-methods #{"logistic-regression"}})
    st))

(deftest commits-a-valid-significant-finding
  (let [st (fresh-store)
        graph (actor/build-graph {:store st})
        request {:client-id "client-1" :op :approve-analysis :stake :low
                 :model-id "M-1" :p-value 0.02 :method "logistic-regression"
                 :claim :significant}
        result (actor/run-request! graph request {} "thread-1")]
    (is (= :done (:status result)))
    (is (some? (get-in result [:state :record])))
    (is (= 1 (count (store/records-of st "client-1"))))))

(deftest holds-a-threshold-violating-finding
  (let [st (fresh-store)
        graph (actor/build-graph {:store st})
        request {:client-id "client-1" :op :approve-analysis :stake :low
                 :model-id "M-1" :p-value 0.30 :method "logistic-regression"
                 :claim :significant}
        result (actor/run-request! graph request {} "thread-2")]
    (is (= :hold (:disposition (:state result))))
    (is (empty? (store/records-of st "client-1")))))

(deftest interrupts-then-publishes-on-human-approval
  (let [st (fresh-store)
        graph (actor/build-graph {:store st})
        request {:client-id "client-1" :op :publish-finding :stake :high
                 :model-id "M-1"}
        interrupted (actor/run-request! graph request {} "thread-3")]
    (is (= :interrupted (:status interrupted)))
    (is (empty? (store/records-of st "client-1")))
    (let [resumed (actor/approve! graph "thread-3")]
      (is (= :done (:status resumed)))
      (is (= 1 (count (store/records-of st "client-1")))))))
