(ns quant.sim
  "Deterministic governed-scenario harness for the ISCO-08 2120 mathematicians,
  actuaries and statisticians actor: run a table of requests through the real
  StateGraph and report which ones the governor refused.

  Runtime: `run` and `report` are portable `.cljc`. `-main` is `:clj`-only,
  because process exit codes are a host concern; the `:cljs` branch throws
  rather than pretending to exit.

  Why this namespace exists, and why it fails loudly. A governed actor's claim
  is not that it acts — it is that there exist actions it refuses. A harness
  that ran only clean scenarios would print green while demonstrating nothing,
  which is the shape this workspace has repeatedly caught: a check that could
  not fail returning the same value as a check that passed.

  So `run` counts refusals, and `-main` exits non-zero when the count is zero.
  A scenario table that has stopped exercising the governor is a defect in the
  table, and it is reported as one rather than as a pass.

  The three questions this harness answers that a unit test does not:
    * does the *wired graph* refuse, or only the pure `check` function
    * does an escalated request actually interrupt rather than write
    * does the ledger it leaves behind verify, and does it record who
      approved each write

  Every scenario below is one of the twenty refusals measured as MISSING on
  the pre-change tree — see the docstrings of `quant.operation` and
  `quant.facts` for those measurements — plus the two boundary scenarios that
  make the significance comparison itself visible. This table is the standing
  evidence that they are refusals now."
  (:require [quant.actor :as actor]
            [quant.advisor :as advisor]
            [quant.ledger :as led]
            [quant.phase :as phase]
            [quant.store :as store]))

(def registered-client
  {:client-id "sim-client-1" :name "Awai Mutual Aid Society"})

(def other-client
  {:client-id "sim-client-2" :name "Another Operator"})

(def registered-model
  "A well-formed model: an alpha strictly inside (0,1) and a non-empty set of
  approved methods."
  {:model-id "M-1" :client-id "sim-client-1"
   :name "mortality experience study"
   :significance-threshold 0.05
   :approved-methods #{"logistic-regression" "cox-ph"}})

(def other-clients-model
  "Registered, well-formed, and belonging to someone else."
  {:model-id "M-OTHER" :client-id "sim-client-2"
   :name "another operator's lapse model"
   :significance-threshold 0.05
   :approved-methods #{"logistic-regression"}})

(def alphaless-model
  "Registered against sim-client-1 with no `:significance-threshold`. On the
  pre-change tree this did not refuse — `(> 0.04 nil)` THREW a
  NullPointerException from inside the check."
  {:model-id "M-NOALPHA" :client-id "sim-client-1"
   :name "model registered without an alpha"
   :approved-methods #{"logistic-regression"}})

(def methodless-model
  "Registered with no `:approved-methods` at all. On the pre-change tree a
  proposal that also named no method satisfied the membership invariant
  vacuously."
  {:model-id "M-NOMETHODS" :client-id "sim-client-1"
   :name "model registered without approved methods"
   :significance-threshold 0.05})

(defn- tweaking-advisor
  "An advisor that proposes as the mock does, then applies `f` to the
  proposal. Used to reach proposal shapes a well-formed request cannot
  produce — an unusable confidence, a direct write effect."
  [f]
  (let [inner (advisor/mock-advisor)]
    (reify advisor/Advisor
      (-advise [_ store request] (f (advisor/-advise inner store request))))))

(defn- analysis
  "A well-formed `:approve-analysis` request against M-1, with `overrides`
  applied. Keeping the clean shape in one place is what makes each scenario
  below differ from an admissible request in exactly one respect."
  [overrides]
  (merge {:client-id "sim-client-1" :op :approve-analysis :model-id "M-1"
          :method "cox-ph" :claim :significant :p-value 0.01 :stake :low}
         overrides))

(def scenarios
  "Each entry: the request, the phase it must reach, and why.

  `:expect` is the phase, not merely 'refused', so a scenario that starts
  holding for the wrong reason, or that escalates where it should hold, is a
  mismatch rather than a pass."
  [;; --- admissible, so the table can tell refusal from inability to act ---
   {:name :clean-approval
    :request (analysis {})
    :expect :commit
    :why "an approved method and a p-value under the registered alpha is admissible"}

   {:name :clean-non-significant-claim
    :request (analysis {:claim :not-significant :p-value 0.80})
    :expect :commit
    :why "a claim that does not assert significance is not subject to the arithmetic"}

   ;; --- the boundary. Without these two the comparison operator is invisible:
   ;; flipping `<=` to `<` in quant.facts/significant? leaves every other
   ;; scenario green and turns exactly :p-value-on-the-line red. ---
   {:name :p-value-on-the-line
    :request (analysis {:p-value 0.05})
    :expect :commit
    :why "p == alpha conforms: the registered alpha is the LARGEST p-value that still supports the claim"}

   {:name :p-value-just-over-the-line
    :request (analysis {:p-value 0.0500001})
    :expect :hold
    :why "the smallest step above alpha must refuse; this and the previous scenario are what make the comparison visible"}

   ;; --- A. the operation vocabulary was open (6 measured admitted clean) ---
   {:name :undeclared-op
    :request (analysis {:op :some-new-op :model-id "no-such" :method "made-up" :p-value 0.9})
    :expect :hold
    :why "an undeclared op bypassed EVERY model invariant; pre-change {:ok? true :violations []}"}

   {:name :nil-op
    :request (analysis {:op nil})
    :expect :hold
    :why "a proposal with no operation at all; pre-change {:ok? true :violations []}"}

   {:name :reserved-sign-actuarial-certificate
    :request (analysis {:op :sign-actuarial-certificate})
    :expect :hold
    :why "a statutory actuarial opinion is a named individual's responsibility; pre-change {:ok? true}"}

   {:name :reserved-set-reserves
    :request (analysis {:op :set-reserves})
    :expect :hold
    :why "committing the balance sheet is not a consequence of an analysis; pre-change {:ok? true}"}

   {:name :reserved-issue-policy-quote
    :request (analysis {:op :issue-policy-quote})
    :expect :hold
    :why "pricing binds the operator to a counterparty; pre-change {:ok? true}"}

   {:name :reserved-delete-model
    :request (analysis {:op :delete-model})
    :expect :hold
    :why "destroying the model registry can never be delegated; pre-change {:ok? true}"}

   {:name :reserved-disable-audit-logging
    :request (analysis {:op :disable-audit-logging})
    :expect :hold
    :why "removing the audit trail removes this actor's own evidence; pre-change {:ok? true}"}

   ;; --- B. :publish-finding escaped all four model invariants (4 measured
   ;; escalating to a human with an EMPTY violation list) ---
   {:name :publish-unregistered-model
    :request (analysis {:op :publish-finding :model-id "no-such"})
    :expect :hold
    :why "external publication citing a model nobody registered; pre-change it escalated clean"}

   {:name :publish-other-clients-model
    :request (analysis {:op :publish-finding :model-id "M-OTHER" :method "logistic-regression"})
    :expect :hold
    :why "publishing against another operator's model; pre-change it escalated clean"}

   {:name :publish-unapproved-method
    :request (analysis {:op :publish-finding :method "p-hacked-anova"})
    :expect :hold
    :why "publishing a method nobody registered; pre-change it escalated clean"}

   {:name :publish-over-threshold
    :request (analysis {:op :publish-finding :p-value 0.9})
    :expect :hold
    :why "publishing p=0.9 as significant; pre-change it escalated to a human with violations []"}

   {:name :publish-clean
    :request (analysis {:op :publish-finding})
    :expect :request-approval
    :why "external publication is always human sign-off — but now checked BEFORE it is asked"}

   ;; --- C. the significance arithmetic was skippable (6 measured) ---
   {:name :p-value-as-string
    :request (analysis {:p-value "0.9"})
    :expect :hold
    :why "a p-value that is not a number skipped the comparison; pre-change {:ok? true}"}

   {:name :p-value-absent
    :request (analysis {:p-value nil})
    :expect :hold
    :why "an absent p-value skipped the comparison entirely; pre-change {:ok? true}"}

   {:name :p-value-out-of-range
    :request (analysis {:p-value -3})
    :expect :hold
    :why "-3 IS a number and (> -3 0.05) is false, so it was admitted twice over; pre-change {:ok? true}"}

   {:name :stronger-claim-word
    :request (analysis {:claim :conclusive :p-value 0.9})
    :expect :hold
    :why "the check fired on the literal :significant, so a stronger word bypassed it; pre-change {:ok? true}"}

   {:name :reserved-claim-proven
    :request (analysis {:claim :proven :p-value 0.001})
    :expect :hold
    :why "no alpha makes 'proven' true, so it is refused rather than compared; pre-change {:ok? true}"}

   {:name :undeclared-claim
    :request (analysis {:claim :basically-certain :p-value 0.9})
    :expect :hold
    :why "the claim vocabulary was open; an unknown word was exempt from the arithmetic"}

   ;; --- D. method membership was skippable (2 measured) ---
   {:name :method-absent
    :request (analysis {:method nil})
    :expect :hold
    :why "naming no method satisfies 'the method is registered' vacuously; pre-change {:ok? true}"}

   {:name :model-without-approved-methods
    :request (analysis {:model-id "M-NOMETHODS" :method nil})
    :expect :hold
    :why "a model with nothing registered to be a member of; pre-change {:ok? true}"}

   ;; --- E. a model with no registered alpha THREW rather than refusing ---
   {:name :model-without-alpha
    :request (analysis {:model-id "M-NOALPHA" :method "logistic-regression" :p-value 0.04})
    :expect :hold
    :why "pre-change the check THREW a NullPointerException; a check that throws has not refused"}

   ;; --- G. confidence was compared without being on the floor's scale ---
   {:name :confidence-out-of-range
    :request (analysis {})
    :tweak #(assoc % :confidence 5.0)
    :expect :hold
    :why "5.0 clears a floor stated on [0,1] without being a confidence; pre-change {:ok? true}"}

   {:name :confidence-not-a-number
    :request (analysis {})
    :tweak #(assoc % :confidence "high")
    :expect :hold
    :why "an unreadable confidence must refuse, not compare"}

   ;; --- the no-actuation invariant, through the wired graph ---
   {:name :direct-write-effect
    :request (analysis {})
    :tweak #(assoc % :effect :direct-write)
    :expect :hold
    :why "the advisor only proposes; a direct write effect is refused at the governor"}

   ;; --- client provenance ---
   {:name :unregistered-client
    :request (analysis {:client-id "nobody"})
    :expect :hold
    :why "an unregistered organization has no provenance to govern against"}

   {:name :request-without-client-id
    :request (analysis {:client-id nil})
    :expect :hold
    :why "a request naming no client must not resolve to whatever is registered under nil"}])

(defn- run-one [scenario]
  (let [st (store/mem-store)
        _ (store/register-client! st registered-client)
        _ (store/register-client! st other-client)
        _ (store/register-model! st registered-model)
        _ (store/register-model! st other-clients-model)
        _ (store/register-model! st alphaless-model)
        _ (store/register-model! st methodless-model)
        graph (actor/build-graph
               (cond-> {:store st}
                 (:tweak scenario) (assoc :advisor (tweaking-advisor (:tweak scenario)))))
        thread (str "sim-" (name (:name scenario)))
        result (actor/run-request! graph (:request scenario) {} thread)
        state (:state result)
        actual (or (:disposition state)
                   ;; A run that never reached :decide produced no phase at
                   ;; all; report that rather than defaulting it to a phase,
                   ;; which would make an unrun scenario look like a verdict.
                   :no-phase)]
    {:name (:name scenario)
     :expect (:expect scenario)
     :actual actual
     :why (:why scenario)
     :status (:status result)
     :match? (= actual (:expect scenario))
     :refusal? (and (not= actual :no-phase) (phase/refusal? actual))
     :wrote? (pos? (count (store/records-of st (:client-id (:request scenario)))))
     :ledger-verify (led/verify (store/ledger st))}))

(defn run
  "Run `table` (default `scenarios`). Returns
  `{:results [..] :refusals n :mismatches [..] :ok? bool}`.

  `:ok?` requires all four: every scenario reached its expected phase, no
  refusal wrote a record anyway, every ledger left behind verifies, and at
  least one refusal was demonstrated.

  The table is a parameter so the fourth conjunct is reachable from a test. A
  harness whose 'a refusal-free table is a failure' rule can only be exercised
  by the table that already refuses 26 things is a rule nobody is checking —
  it would keep returning the same value if the conjunct were deleted."
  ([] (run scenarios))
  ([table]
   (let [results (mapv run-one table)
         refusals (count (filter :refusal? results))
         mismatches (filterv (complement :match?) results)
         ;; A refusal that still wrote a record is the worst outcome available
         ;; and would otherwise hide inside a matching phase.
         wrote-anyway (filterv #(and (:refusal? %) (:wrote? %)) results)
         ledger-breaks (filterv #(not (:ok? (:ledger-verify %))) results)]
     {:results results
      :refusals refusals
      :mismatches mismatches
      :wrote-anyway wrote-anyway
      :ledger-breaks ledger-breaks
      :ok? (and (empty? mismatches)
                (empty? wrote-anyway)
                (empty? ledger-breaks)
                (pos? refusals))})))

(defn report
  "Human-readable run report. Pure: takes the result of `run`."
  [{:keys [results refusals mismatches wrote-anyway ledger-breaks ok?]}]
  (str
   "quant.sim — governed scenario run\n"
   (apply str
          (for [r results]
            (str "  " (if (:match? r) "ok  " "BAD ")
                 (name (:name r))
                 " expect=" (name (:expect r))
                 " actual=" (name (:actual r))
                 (when (:refusal? r) " [refused]")
                 "\n")))
   "  scenarios=" (count results)
   " refusals=" refusals
   " mismatches=" (count mismatches)
   " wrote-anyway=" (count wrote-anyway)
   " ledger-breaks=" (count ledger-breaks)
   "\n"
   (cond
     (zero? refusals)
     "  REFUSING TO REPORT A PASS: the scenario table demonstrated no refusal.\n"
     ok? "  PASS\n"
     :else "  FAIL\n")))

#?(:clj
   (defn -main [& _]
     (let [r (run)]
       (print (report r))
       (flush)
       (System/exit (if (:ok? r) 0 1))))
   :cljs
   (defn -main [& _]
     (throw (ex-info "quant.sim/-main is :clj-only (process exit codes are a host concern); call `run` and inspect the result instead" {}))))
