(ns quant.operation
  "The closed vocabulary of operations the ISCO-08 2120 mathematicians,
  actuaries and statisticians actor may propose.

  Runtime: portable `.cljc` (pure data + pure predicates, no host interop).

  Why this namespace exists. Before it, the operation vocabulary lived in two
  places that could not disagree loudly: the README's prose, and the
  Governor's private `(= :approve-analysis op)` test plus one named escalating
  op. That made the Governor a *denylist* — it bound two named ops and
  admitted everything else. Measured on the pre-change tree, against the
  registered client `c1`:

      {:op :delete-model :effect :propose :confidence 0.95}
      => {:ok? true :violations []}

  Admitted, and admitted as a *clean* verdict: no escalation, no human, and an
  empty violation list to show a reviewer. `:sign-actuarial-certificate`,
  `:disable-audit-logging` and `:issue-policy-quote` were admitted the same
  way, and so was a proposal whose `:op` was `nil`.

  Worse, an undeclared op did not merely bypass its own rule — it bypassed
  every statistical invariant this repo exists to enforce, because all of them
  were gated behind the `:approve-analysis` test:

      {:op :some-new-op :effect :propose :model-id \"no-such\"
       :method \"made-up\" :claim :significant :p-value 0.9}
      => {:ok? true :violations []}

  An unregistered model, a method nobody approved, and a significance claim at
  p = 0.9 — admitted together, clean, because the op naming them was not one
  of the two the governor knew about. An actor whose operation set is open
  cannot be governed, because the governor is answering a question about a
  vocabulary nobody declared. So the vocabulary is declared here, once, as an
  allowlist, and `quant.governor` refuses anything outside it.

  Two disjoint maps:

  * `supported` — what the actor may propose. `:escalates?` and `:model-op?`
    are properties of the operation, not of the governor's mood, so they live
    beside it.
  * `reserved` — operations naming authority this cognitive actor does not
    hold: statutory actuarial sign-off, balance-sheet commitment, pricing, and
    removal of the model registry or the audit trail. These are *declared*
    rather than merely absent so the refusal can say why. An undeclared op is
    a vocabulary error; a reserved op is an authority boundary. Conflating
    them would let a future edit `supported`-list one of them by accident.

  `:model-op?` is the field that closes the widest gap this repo shipped with.
  All four model invariants — registration, ownership, method membership and
  the significance arithmetic — were gated on `(= :approve-analysis op)`, so
  `:publish-finding`, the operation whose entire purpose is external
  publication, was exempt from every one of them. Measured on the pre-change
  tree, the same four proposals under the two ops:

      approve  unregistered model      => :hold  [:unknown-model]
      publish  unregistered model      => :escalate, violations []
      approve  other client's model    => :hold  [:model-wrong-client]
      publish  other client's model    => :escalate, violations []
      approve  unapproved method       => :hold  [:unapproved-method]
      publish  unapproved method       => :escalate, violations []
      approve  p=0.9 claimed significant => :hold [:significance-threshold-violated]
      publish  p=0.9 claimed significant => :escalate, violations []

  The escalating op reached a human with an **empty violation list**, on the
  one operation whose premise is that a number is about to leave the building
  under this operator's name. Binding is a property of the operation, so it is
  declared here and the governor reads it, rather than the governor naming one
  op and forgetting the more consequential one."
  )

(def supported
  "Operations the actor may propose.

  `:escalates?` true means human sign-off is required regardless of advisor
  confidence. `:model-op?` true means the proposal binds to a REGISTERED
  model, and therefore must satisfy every registered fact about it — the
  approved-methods membership of the proposed method and the arithmetic
  comparison of the reported p-value against the registered alpha."
  {:draft-analysis
   {:escalates? false
    :model-op? false
    :summary "draft an internal analysis note (binds no model, leaves the operator nowhere)"}

   :approve-analysis
   {:escalates? false
    :model-op? true
    :summary "approve an analysis against a registered model"}

   :publish-finding
   {:escalates? true
    :model-op? true
    :summary "publish a finding outside the operator"}

   :flag-model-risk
   {:escalates? true
    :model-op? false
    :summary "surface a suspected model defect to the responsible actuary"}})

(def reserved
  "Operations reserved to someone this actor is not. Naming one in a proposal
  is a permanent hard block, never an escalation: escalation would imply a
  human could approve the *actor* doing it, and neither a responsible actuary
  nor an operator can delegate a statutory sign-off, a reserving decision, a
  pricing act, or the removal of an audit trail to a remote cognitive actor.

  This is the machine-readable form of the scope sentence the README has
  carried since the repo was created — the advisor only proposes. Prose in a
  README does not refuse anything."
  {:sign-actuarial-certificate
   {:reason "a statutory actuarial opinion is a named individual's personal professional and legal responsibility, and cannot be held by a process"}

   :set-reserves
   {:reason "committing the balance sheet is a reserving decision of the operator, not a consequence of an analysis"}

   :issue-policy-quote
   {:reason "pricing binds the operator to a counterparty; it is a commercial act, never an analytical output"}

   :delete-model
   {:reason "destroying the model registry makes every past finding unreviewable and is unrecoverable by construction"}

   :disable-audit-logging
   {:reason "removing the audit trail removes the evidence this actor's own governance rests on"}})

(defn supported? [op] (contains? supported op))
(defn reserved? [op] (contains? reserved op))

(defn declared?
  "True if `op` is named anywhere in this vocabulary. An op that is neither
  supported nor reserved is undeclared — the governor refuses it."
  [op]
  (or (supported? op) (reserved? op)))

(defn escalates?
  "True if the operation itself always requires human sign-off. Unsupported
  ops are never reached by this predicate (the governor hard-blocks first), so
  a false here is not an admission."
  [op]
  (boolean (get-in supported [op :escalates?])))

(defn model-op?
  "True if the operation binds to a registered model and must therefore
  satisfy every registered fact about it. False for undeclared and reserved
  ops, which the governor hard-blocks before this is consulted."
  [op]
  (boolean (get-in supported [op :model-op?])))

(defn reserved-reason [op] (get-in reserved [op :reason]))
