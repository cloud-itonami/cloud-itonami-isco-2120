(ns quant.governor
  "QuantitativeProfessionalsGovernor — the independent safety/traceability
  layer for the ISCO-08 2120 community mathematicians, actuaries and
  statisticians actor (itonami actor pattern, ADR-2607011000 / CLAUDE.md
  Actors section).

  Statistical twist: a significance claim is an arithmetic comparison against
  the registered alpha, and the analysis method is either a member of the
  registered approved-methods set or it is not. Swapping methodology after
  seeing the data is not something the governor can detect by content, but it
  CAN refuse a method nobody registered, which forces disclosure before the
  fact rather than after.

  The operation vocabulary lives in `quant.operation` and the well-formedness
  of the values lives in `quant.facts`. This namespace decides; it does not
  also define what there is to decide about. Before that split it did both,
  and the result was a denylist: two named ops were bound and everything else
  — including `nil` — was admitted clean, while the four model invariants were
  all gated behind one of those two ops so the other, `:publish-finding`,
  escaped every one of them. The measurements are in those two docstrings.

  HARD invariants (:hard? true, ALWAYS :hold, never overridable):
    1. declared operation  — the proposal's :op must be in the declared
                           vocabulary. Undeclared is a vocabulary error.
    2. reserved operation  — a declared-but-reserved op names authority this
                           actor does not hold. Never escalatable.
    3. client provenance   — the record the store returned must identify the
                           client the request asked for.
    4. no-actuation        — proposal :effect must be :propose.
    5. usable confidence   — :confidence must be a number on [0,1], because
                           the escalation floor only bounds a quantity on the
                           scale the floor is stated in.
    6. model basis         — a model-bound op must cite a REGISTERED,
                           WELL-FORMED model, and that model must belong to
                           the client the request names.
    7. declared claim      — a model-bound op must state a claim from the
                           declared vocabulary. A reserved claim (:proven,
                           :causal, :replicated) is refused permanently: no
                           alpha makes it true, so escalating it would ask a
                           human a question the arithmetic cannot answer yes
                           to.
    8. method membership   — the proposed method must be declared AND a member
                           of the model's registered :approved-methods set (no
                           undisclosed methodology, and no omitted one).
    9. significance arithmetic — a claim that asserts significance must report
                           a readable p-value on [0,1] at or below the model's
                           registered alpha (arithmetic comparison, not a
                           judgement call).
  ESCALATION invariants (:escalate? true, human sign-off):
   10. the operation itself escalates (`quant.operation/escalates?`).
   11. low confidence (< `confidence-floor`).

  Ordering: hard invariants are collected in full, so a verdict names every
  reason it was refused rather than only the first. `:escalate?` is zeroed
  when anything is hard — see `quant.phase` on why that must not be the only
  guard."
  (:require [quant.facts :as facts]
            [quant.operation :as operation]
            [quant.store :as store]))

(def confidence-floor 0.6)

(defn- v [rule detail] {:rule rule :detail detail})

(defn- operation-violations [op]
  (cond
    (not (operation/declared? op))
    [(v :undeclared-operation
        (str "op " (pr-str op) " は宣言された語彙の外（未宣言の op は"
             "すべての不変条件を迂回する）"))]

    (operation/reserved? op)
    [(v :reserved-operation
        (str "op " (pr-str op) " はこの actor が持たない権限: "
             (operation/reserved-reason op)))]

    :else []))

(defn- model-violations
  "Invariants that apply because the operation binds to a registered model.
  Reached only for a supported `:model-op?`; see `quant.operation`."
  [request proposal m]
  (let [{:keys [method claim p-value]} proposal
        m-defect (facts/model-defect m)]
    (if m-defect
      ;; Nothing downstream can be established against a model that is not
      ;; usable, so this is reported alone rather than followed by a cascade
      ;; of derived complaints about a record that was never there.
      [(v :unusable-model
          (str "model " (pr-str (:model-id proposal)) " は "
               m-defect "（未登録、または閾値/承認手法が登録されていない）"))]
      (let [claim-defect (facts/claim-defect claim)
            method-defect (facts/method-defect method)
            approved (:approved-methods m)
            alpha (:significance-threshold m)]
        (cond-> []
          (not= (:client-id m) (:client-id request))
          (conj (v :model-wrong-client "model が別 client のもの"))

          (= :claim/reserved claim-defect)
          (conj (v :reserved-claim
                   (str "claim " (pr-str claim) " は永久拒否: "
                        (get-in facts/reserved-claims [claim :reason]))))

          (= :claim/undeclared claim-defect)
          (conj (v :undeclared-claim
                   (str "claim " (pr-str claim) " は宣言された語彙 "
                        (set (keys facts/claims)) " の外")))

          method-defect
          (conj (v :undeclared-method
                   "手法が宣言されていない（無手法は承認集合の membership を空虚に満たす）"))

          (and (not method-defect) (not (contains? approved method)))
          (conj (v :unapproved-method
                   (str "手法 " (pr-str method) " は登録済み承認集合 " approved
                        " の外（事後の手法差替は開示を要する）")))

          ;; The arithmetic. Applies to every claim word that asserts
          ;; significance, not only the literal `:significant`.
          (and (not claim-defect) (facts/asserts-significance? claim)
               (facts/p-value-defect p-value))
          (conj (v :unusable-p-value
                   (str "p値 " (pr-str p-value) " は "
                        (facts/p-value-defect p-value)
                        "（有意性の主張は [0,1] の数を要する）")))

          (and (not claim-defect) (facts/asserts-significance? claim)
               (not (facts/p-value-defect p-value))
               (not (facts/significant? p-value alpha)))
          (conj (v :significance-threshold-violated
                   (str "p値 " p-value " > 登録済み有意水準 " alpha
                        "（有意性主張は算術比較であって判断ではない）"))))))))

(defn- hard-violations [request proposal client-record m]
  (let [op (:op proposal)
        op-vs (operation-violations op)
        client-defect (facts/client-defect client-record (:client-id request))
        conf-defect (facts/confidence-defect (:confidence proposal))]
    (into
     (cond-> []
       client-defect
       (conj (v :no-client (str "client provenance: " client-defect)))

       (not= :propose (:effect proposal))
       (conj (v :no-actuation "effect は :propose のみ許可（直接書込禁止）"))

       conf-defect
       (conj (v :unusable-confidence
                (str "confidence " (pr-str (:confidence proposal)) " は " conf-defect
                     "（escalation floor は [0,1] の量に対してしか効かない）"))))
     ;; An undeclared or reserved op is a vocabulary/authority error, and the
     ;; model invariants are not meaningful for an op whose binding is
     ;; undefined — so they are reported instead of, not alongside, those.
     (if (seq op-vs)
       op-vs
       (when (operation/model-op? op)
         (model-violations request proposal m))))))

(defn check
  "Assess a proposal against `request`/`context`/`proposal` and a `store`
  implementing `quant.store/Store`. Pure — never mutates the store."
  [request _context proposal store]
  (let [client-record (store/client store (:client-id request))
        m (some->> (:model-id proposal) (store/model store))
        hard (hard-violations request proposal client-record m)
        hard? (boolean (seq hard))
        conf (:confidence proposal)
        ;; An unusable confidence is already a hard violation; treating it as
        ;; low here too keeps the escalation flag from reading `false` for a
        ;; value that was never on the scale.
        low? (or (not (number? conf)) (< conf confidence-floor))
        risky-op? (operation/escalates? (:op proposal))]
    {:ok? (and (not hard?) (not low?) (not risky-op?))
     :violations hard
     :confidence conf
     :hard? hard?
     :escalate? (and (not hard?) (or low? risky-op?))}))
