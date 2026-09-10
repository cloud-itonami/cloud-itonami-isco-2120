(ns quant.facts
  "Well-formedness of the values the ISCO-08 2120 governor compares, and the
  significance arithmetic itself, as pure named functions.

  Runtime: portable `.cljc`. The only host-specific expression is the NaN
  test, which is reader-conditioned; every `cond` below establishes
  `number?` before reaching it.

  Why this namespace exists. The governor's two statistical invariants were
  each written as one clause of a `cond`, and each clause quietly required its
  inputs to already be well-formed. Where they were not, the check did not
  refuse — it either skipped or it threw. Measured on the pre-change tree,
  against a registered model with alpha 0.05 and approved methods
  `#{\"logistic-regression\" \"cox-ph\"}`:

  (1) The significance comparison was guarded by `(number? p-value)`, so a
      claim of significance with an unreadable p-value skipped the arithmetic
      entirely and committed clean:

        claim :significant, p-value \"0.9\" (a string) => {:ok? true}
        claim :significant, p-value absent            => {:ok? true}
        claim :significant, p-value nil               => {:ok? true}
        claim :significant, p-value -3                => {:ok? true}

      The last is worth stating plainly: -3 IS a number, so the guard admitted
      it, and `(> -3 0.05)` is false, so the comparison admitted it too. A
      p-value outside [0,1] is not a small p-value; it is not a p-value.

  (2) The comparison fired only on the literal claim `:significant`, and the
      claim vocabulary was open, so a stronger word bypassed it:

        claim :conclusive, p-value 0.9 => {:ok? true}
        claim :proven,     p-value 0.9 => {:ok? true}

  (3) Method membership was guarded by `method` being truthy, so omitting the
      method satisfied membership vacuously:

        method absent entirely => {:ok? true}

      and a model registered with no `:approved-methods` at all admitted any
      proposal that also named no method:

        model M-NOMETHODS, no method => {:ok? true}

  (4) A model registered without a `:significance-threshold` did not refuse.
      `(> 0.04 nil)` threw:

        java.lang.NullPointerException: Cannot invoke \"Object.getClass()\"
        because \"x\" is null

      A check that throws has not refused. It has failed to answer, and the
      caller gets an exception where it expected a verdict.

  (5) Confidence was compared against the floor with no test that it was on
      the floor's scale:

        confidence 5.0 => {:ok? true}, no escalation

      An advisor reporting 5.0 is not more confident than one reporting 0.9;
      it is reporting on some other scale, or nothing. Admitting it lets any
      unusable value clear an escalation floor stated on [0,1].

  Each function below answers 'why can this value not be used', returning a
  namespaced keyword or nil. The governor turns a non-nil answer into a hard
  violation. Returning the reason rather than a bare false is what lets the
  ledger entry say which check failed."
  (:require [clojure.string :as str]))

(defn- nan? [x]
  #?(:clj  (and (number? x) (Double/isNaN (double x)))
     :cljs (and (number? x) (js/isNaN x))))

(defn- name-string?
  "An identity string: present, a string, and not only whitespace."
  [s]
  (and (string? s) (not (str/blank? s))))

(def claims
  "The closed vocabulary of claims a finding may make about its evidence.

  `:asserts-significance?` is what decides whether the arithmetic applies. It
  is a property of the word, declared once here, rather than a literal in the
  governor's `cond` — which is how `:conclusive` came to mean 'exempt from the
  significance check' on the pre-change tree."
  {:significant
   {:asserts-significance? true
    :summary "the reported p-value is at or below the model's registered alpha"}

   :conclusive
   {:asserts-significance? true
    :summary "a stronger word for the same arithmetic; it does not license a larger p-value"}

   :suggestive
   {:asserts-significance? false
    :summary "evidence short of the registered alpha, reported as such"}

   :not-significant
   {:asserts-significance? false
    :summary "the reported p-value does not clear the registered alpha"}

   :inconclusive
   {:asserts-significance? false
    :summary "the analysis does not support a determination either way"}})

(def reserved-claims
  "Claims that no alpha can make true, refused permanently rather than
  compared. These are not stricter versions of `:significant` — they assert
  something a p-value is not evidence for at any threshold, so escalating them
  would put a question to a human that the arithmetic cannot answer yes to.

  Declaring them, rather than letting them fall through as undeclared, is what
  lets the refusal explain itself to the statistician who proposed it."
  {:proven
   {:reason "a p-value is evidence against a null hypothesis, never a proof of the alternative; no alpha makes this claim true"}

   :causal
   {:reason "an association at any alpha is not a causal claim; causality rests on a design the governor cannot read off a p-value"}

   :replicated
   {:reason "replication is a fact about other studies, not about this one's p-value; it cannot be established from a single analysis"}})

(defn claim-defect
  "Why `c` cannot be governed as a claim, or nil if it can."
  [c]
  (cond
    (contains? reserved-claims c) :claim/reserved
    (contains? claims c)          nil
    :else                         :claim/undeclared))

(defn asserts-significance?
  "True if the claim word asserts that the finding cleared the registered
  alpha. Reserved and undeclared claims never reach this — the governor
  hard-blocks first — so a false here is not an admission."
  [c]
  (boolean (get-in claims [c :asserts-significance?])))

(defn p-value-defect
  "Why `p` cannot be compared against a registered alpha, or nil if it can.
  A p-value is a probability: it must be a number on [0,1]. Absent, unparsed
  and out-of-range values are all unusable, not merely small."
  [p]
  (cond
    (not (number? p)) :p-value/not-a-number
    (nan? p)          :p-value/not-a-number
    (not (<= 0 p 1))  :p-value/out-of-range
    :else nil))

(defn alpha-defect
  "Why `a` cannot serve as a registered significance threshold, or nil if it
  can. Strictly inside (0,1): alpha 0 makes significance unreachable and
  alpha 1 makes it vacuous, so neither is a threshold a claim can be checked
  against. This is the value whose absence THREW in measurement (4)."
  [a]
  (cond
    (not (number? a)) :alpha/not-registered
    (nan? a)          :alpha/not-a-number
    (not (< 0 a 1))   :alpha/out-of-range
    :else nil))

(defn significant?
  "The arithmetic, named: a reported p-value supports a significance claim iff
  it is at or below the registered alpha.

  The comparison is `<=`, not `<`. The registered alpha is the largest
  p-value that still supports the claim, so the line itself conforms — this is
  the reading the README has always stated ('a p-value <= the registered
  significance threshold') and the one `quant.governor-test` has asserted
  since the repo was created. Callers must establish `p-value-defect` and
  `alpha-defect` are nil first; this function does arithmetic, not
  validation."
  [p alpha]
  (<= p alpha))

(defn approved-methods-defect
  "Why a model's registered `:approved-methods` cannot support a membership
  test, or nil if it can. An absent or empty set is not a stricter
  registration; it is one against which membership can never be established,
  and on the pre-change tree it admitted a proposal that named no method."
  [ms]
  (cond
    (not (set? ms))                :methods/not-a-set
    (empty? ms)                    :methods/none-registered
    (not (every? name-string? ms)) :methods/not-a-name
    :else nil))

(defn method-defect
  "Why the proposed `m` cannot be checked for membership, or nil if it can.
  An absent method is a defect for a model-bound operation: it satisfies
  'the proposed method is registered' vacuously, which is measurement (3)."
  [m]
  (if (name-string? m) nil :method/not-declared))

(defn confidence-defect
  "Why `c` cannot be compared against the escalation floor, or nil if it can.
  A confidence that is not a number on [0,1] is unusable, not merely high —
  measurement (5)."
  [c]
  (cond
    (not (number? c)) :confidence/not-a-number
    (nan? c)          :confidence/not-a-number
    (not (<= 0 c 1))  :confidence/out-of-range
    :else nil))

(defn client-defect
  "Why `record` cannot serve as client provenance, or nil if it can.

  Checked against `requested-id` so that the record the store returned is the
  record the request asked for: a request carrying no `:client-id` must not
  resolve to whatever happens to be registered under the key nil."
  [record requested-id]
  (cond
    (nil? record)                            :client/unregistered
    (not (map? record))                      :client/not-a-record
    (not (name-string? requested-id))        :client/no-requested-id
    (not (name-string? (:client-id record))) :client/record-has-no-id
    (not= (:client-id record) requested-id)  :client/id-mismatch
    :else nil))

(defn model-defect
  "Why `m` cannot be governed as a registered analysis model, or nil if it
  can.

  Folds in `alpha-defect` and `approved-methods-defect`: a model registered
  without a threshold to compare against, or without a method set to be a
  member of, is not a stricter model. It is one against which no statistical
  invariant can be established — and on the pre-change tree the first of those
  threw and the second admitted."
  [m]
  (cond
    (nil? m)                            :model/unregistered
    (not (map? m))                      :model/not-a-record
    (not (name-string? (:model-id m)))  :model/no-id
    (not (name-string? (:client-id m))) :model/no-client-id
    :else (or (alpha-defect (:significance-threshold m))
              (approved-methods-defect (:approved-methods m)))))
