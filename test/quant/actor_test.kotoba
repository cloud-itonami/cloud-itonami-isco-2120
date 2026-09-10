(ns quant.actor-test
  (:require [clojure.test :refer [deftest is testing]]
            [quant.actor :as actor]
            [quant.ledger :as ledger]
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
  (testing "the request now carries a method, a claim and a p-value. It used
            to carry none of them and still reach a human — see
            quant.governor-test/publication-is-not-exempt-from-the-model-invariants"
    (let [st (fresh-store)
          graph (actor/build-graph {:store st})
          request {:client-id "client-1" :op :publish-finding :stake :high
                   :model-id "M-1" :p-value 0.02 :method "logistic-regression"
                   :claim :significant}
          interrupted (actor/run-request! graph request {} "thread-3")]
      (is (= :interrupted (:status interrupted)))
      (is (empty? (store/records-of st "client-1")))
      (let [resumed (actor/approve! graph "thread-3")]
        (is (= :done (:status resumed)))
        (is (= 1 (count (store/records-of st "client-1"))))))))

(deftest holds-an-under-specified-publication
  (testing "the shape the previous test used to assert was admissible"
    (let [st (fresh-store)
          graph (actor/build-graph {:store st})
          request {:client-id "client-1" :op :publish-finding :stake :high
                   :model-id "M-1"}
          result (actor/run-request! graph request {} "thread-4")]
      (is (= :hold (:disposition (:state result))))
      (is (empty? (store/records-of st "client-1"))))))

(deftest the-ledger-records-who-approved-each-write
  (testing "a human-approved publication and an automatic approval used to
            leave two {:disposition :commit} entries with no field telling
            them apart. For a number that leaves the building under this
            operator's name, that distinction is why the interrupt exists."
    (let [st (fresh-store)
          graph (actor/build-graph {:store st})
          base {:client-id "client-1" :model-id "M-1" :p-value 0.02
                :method "logistic-regression" :claim :significant}]
      (actor/run-request! graph (assoc base :op :approve-analysis :stake :low) {} "t-auto")
      (actor/run-request! graph (assoc base :op :publish-finding :stake :high) {} "t-human")
      (actor/approve! graph "t-human")
      (let [entries (store/ledger st)
            by (mapv :approved-by entries)]
        (is (= 2 (count entries)))
        (is (= [:actor :human] by))
        (is (:ok? (ledger/verify entries))
            "and the chain the two entries sit in verifies")))))

(deftest a-refused-run-leaves-a-verifying-ledger-and-no-record
  (let [st (fresh-store)
        graph (actor/build-graph {:store st})
        request {:client-id "client-1" :op :approve-analysis :stake :low
                 :model-id "M-1" :p-value 0.30 :method "logistic-regression"
                 :claim :significant}]
    (actor/run-request! graph request {} "t-hold")
    (let [entries (store/ledger st)]
      (is (= [:hold] (mapv :disposition entries)))
      (is (= [:none] (mapv :approved-by entries)))
      (is (:ok? (ledger/verify entries)))
      (is (empty? (store/records-of st "client-1"))))))
