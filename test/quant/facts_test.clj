(ns quant.facts-test
  (:require [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [quant.facts :as facts]))

;; --- the significance arithmetic, and its boundary ---

(deftest significance-is-at-or-below-alpha
  (testing "The registered alpha is the LARGEST p-value that still supports
            the claim, so the line itself conforms. These assertions are the
            boundary: flipping <= to < in significant? turns
            `(significant? 0.05 0.05)` red and leaves every other assertion in
            this suite green. A comparison with no case exactly on the line
            cannot show which operator it uses."
    (is (facts/significant? 0.05 0.05) "exactly on the line conforms")
    (is (facts/significant? 0.0499999 0.05))
    (is (not (facts/significant? 0.0500001 0.05)) "the smallest step above does not")
    (is (facts/significant? 0.0 0.05))
    (is (not (facts/significant? 1.0 0.05)))))

(deftest a-p-value-is-a-probability
  (testing "the pre-change guard was (number? p-value), which admitted -3:
            it IS a number, and (> -3 0.05) is false, so it cleared twice"
    (is (nil? (facts/p-value-defect 0.05)))
    (is (nil? (facts/p-value-defect 0)))
    (is (nil? (facts/p-value-defect 1)))
    (is (= :p-value/out-of-range (facts/p-value-defect -3)))
    (is (= :p-value/out-of-range (facts/p-value-defect 2)))
    (is (= :p-value/not-a-number (facts/p-value-defect "0.9")))
    (is (= :p-value/not-a-number (facts/p-value-defect nil)))
    (is (= :p-value/not-a-number (facts/p-value-defect ##NaN)))))

(deftest an-alpha-must-be-strictly-inside-zero-and-one
  (testing "alpha 0 makes significance unreachable and alpha 1 makes it
            vacuous, so neither is a threshold a claim can be checked against.
            An absent alpha is the value that THREW on the pre-change tree."
    (is (nil? (facts/alpha-defect 0.05)))
    (is (= :alpha/not-registered (facts/alpha-defect nil)))
    (is (= :alpha/out-of-range (facts/alpha-defect 0)))
    (is (= :alpha/out-of-range (facts/alpha-defect 1)))
    (is (= :alpha/out-of-range (facts/alpha-defect 1.5)))))

;; --- the claim vocabulary ---

(deftest claims-and-reserved-claims-are-disjoint
  (is (empty? (set/intersection (set (keys facts/claims))
                                        (set (keys facts/reserved-claims))))))

(deftest stronger-words-take-the-same-arithmetic
  (testing "the pre-change check fired on the literal :significant, so a
            stronger word was exempt from the comparison it strengthens"
    (is (facts/asserts-significance? :significant))
    (is (facts/asserts-significance? :conclusive))
    (is (not (facts/asserts-significance? :not-significant)))
    (is (not (facts/asserts-significance? :suggestive)))
    (is (not (facts/asserts-significance? :inconclusive)))))

(deftest reserved-claims-are-refused-not-compared
  (testing "no alpha makes these true, so escalating them would ask a human a
            question the arithmetic cannot answer yes to"
    (doseq [c (keys facts/reserved-claims)]
      (is (= :claim/reserved (facts/claim-defect c)))
      (is (not (facts/asserts-significance? c))
          "a reserved claim must not also route into the comparison")
      (is (string? (get-in facts/reserved-claims [c :reason]))))))

(deftest undeclared-claims-are-refused
  (doseq [c [:basically-certain :very-strong nil "significant"]]
    (is (= :claim/undeclared (facts/claim-defect c)) (str (pr-str c))))
  (doseq [c (keys facts/claims)]
    (is (nil? (facts/claim-defect c)))))

;; --- method membership ---

(deftest an-absent-method-is-a-defect-not-a-vacuous-pass
  (testing "naming no method satisfies 'the proposed method is registered'
            vacuously; pre-change that committed clean"
    (is (nil? (facts/method-defect "cox-ph")))
    (is (= :method/not-declared (facts/method-defect nil)))
    (is (= :method/not-declared (facts/method-defect "")))
    (is (= :method/not-declared (facts/method-defect "   ")))
    (is (= :method/not-declared (facts/method-defect :cox-ph)))))

(deftest an-empty-approved-set-cannot-establish-membership
  (is (nil? (facts/approved-methods-defect #{"cox-ph"})))
  (is (= :methods/none-registered (facts/approved-methods-defect #{})))
  (is (= :methods/not-a-set (facts/approved-methods-defect nil)))
  (is (= :methods/not-a-set (facts/approved-methods-defect ["cox-ph"])))
  (is (= :methods/not-a-name (facts/approved-methods-defect #{"cox-ph" ""}))))

;; --- confidence, client, model ---

(deftest confidence-must-be-on-the-floors-scale
  (testing "an escalation floor stated on [0,1] only bounds a quantity on
            [0,1]; 5.0 cleared it on the pre-change tree"
    (is (nil? (facts/confidence-defect 0.9)))
    (is (nil? (facts/confidence-defect 0)))
    (is (nil? (facts/confidence-defect 1)))
    (is (= :confidence/out-of-range (facts/confidence-defect 5.0)))
    (is (= :confidence/out-of-range (facts/confidence-defect -1)))
    (is (= :confidence/not-a-number (facts/confidence-defect "high")))
    (is (= :confidence/not-a-number (facts/confidence-defect nil)))))

(deftest client-provenance-is-checked-against-the-id-that-was-asked-for
  (testing "a request naming no client must not resolve to whatever happens to
            be registered under the key nil"
    (is (nil? (facts/client-defect {:client-id "c1"} "c1")))
    (is (= :client/unregistered (facts/client-defect nil "c1")))
    (is (= :client/no-requested-id (facts/client-defect {:client-id "c1"} nil)))
    (is (= :client/id-mismatch (facts/client-defect {:client-id "c2"} "c1")))
    (is (= :client/record-has-no-id (facts/client-defect {:name "x"} "c1")))))

(deftest a-model-without-an-alpha-or-methods-is-unusable-not-stricter
  (let [ok {:model-id "M-1" :client-id "c1" :significance-threshold 0.05
            :approved-methods #{"cox-ph"}}]
    (is (nil? (facts/model-defect ok)))
    (is (= :model/unregistered (facts/model-defect nil)))
    (is (= :model/no-id (facts/model-defect (dissoc ok :model-id))))
    (is (= :model/no-client-id (facts/model-defect (dissoc ok :client-id))))
    (is (= :alpha/not-registered (facts/model-defect (dissoc ok :significance-threshold)))
        "this is the model whose alpha comparison THREW a NullPointerException")
    (is (= :methods/not-a-set (facts/model-defect (dissoc ok :approved-methods)))
        "absent is :not-a-set; :none-registered is the empty set")
    (is (= :methods/none-registered (facts/model-defect (assoc ok :approved-methods #{}))))))
