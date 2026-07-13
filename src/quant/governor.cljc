(ns quant.governor
  "QuantitativeProfessionalsGovernor — the independent safety/
  traceability layer for the ISCO-08 2120 community mathematicians,
  actuaries & statisticians actor (itonami actor pattern,
  ADR-2607011000 / CLAUDE.md Actors section). Modeled on
  cloud-itonami-isco-4311's bookkeeping.governor. Statistical twist: a
  significance claim is arithmetic comparison against the registered
  alpha, and the analysis method is either a member of the registered
  approved-methods set or it is not — swapping methodology after
  seeing the data (undisclosed p-hacking) is not something the
  governor can detect by content, but it CAN refuse an unregistered
  method, which forces disclosure before the fact.

  HARD invariants (:hard? true, ALWAYS :hold, never overridable):
    1. client provenance — the organization must be registered.
    2. no-actuation      — proposal :effect must be :propose.
    3. model basis         — an analysis approval must cite a
                           REGISTERED model belonging to this client.
    4. significance arithmetic — a proposed finding claimed as
                           :significant must report a p-value <= the
                           model's registered :significance-threshold
                           (arithmetic comparison, not a judgement
                           call).
    5. method membership   — the proposed method must be a member of
                           the model's registered :approved-methods
                           set (no undisclosed methodology).
  ESCALATION invariants (:escalate? true, human sign-off):
    6. :op :publish-finding (external publication).
    7. low confidence (< `confidence-floor`)."
  (:require [quant.store :as store]))

(def confidence-floor 0.6)

(defn- hard-violations [{:keys [request proposal]} client-record m]
  (let [{:keys [op p-value method claim]} proposal
        analyze? (= :approve-analysis op)]
    (cond-> []
      (nil? client-record)
      (conj {:rule :no-client :detail "未登録 client"})

      (not= :propose (:effect proposal))
      (conj {:rule :no-actuation :detail "effect は :propose のみ許可（直接書込禁止）"})

      (and analyze? (nil? m))
      (conj {:rule :unknown-model :detail "未登録 model への分析承認は不可"})

      (and analyze? m (not= (:client-id m) (:client-id request)))
      (conj {:rule :model-wrong-client :detail "model が別 client のもの"})

      (and analyze? m method (not (contains? (:approved-methods m) method)))
      (conj {:rule :unapproved-method
             :detail (str "手法 " method " は登録済み承認集合 "
                          (:approved-methods m)
                          " の外（事後の手法差替は開示を要する）")})

      (and analyze? m (= claim :significant) (number? p-value)
           (> p-value (:significance-threshold m)))
      (conj {:rule :significance-threshold-violated
             :detail (str "p値 " p-value " > 登録済み有意水準 "
                          (:significance-threshold m)
                          "（有意性主張は算術比較であって判断ではない）")}))))

(defn check
  "Assess a proposal against `request`/`context`/`proposal` and a
  `store` implementing `quant.store/Store`. Pure — never mutates the
  store."
  [request context proposal store]
  (let [client-record (store/client store (:client-id request))
        m (some->> (:model-id proposal) (store/model store))
        hard (hard-violations {:request request :proposal proposal}
                              client-record m)
        hard? (boolean (seq hard))
        conf (or (:confidence proposal) 0.0)
        low? (< conf confidence-floor)
        risky-op? (= :publish-finding (:op proposal))]
    {:ok? (and (not hard?) (not low?) (not risky-op?))
     :violations hard
     :confidence conf
     :hard? hard?
     :escalate? (and (not hard?) (or low? risky-op?))}))
