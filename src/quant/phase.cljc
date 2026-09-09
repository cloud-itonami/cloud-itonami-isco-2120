(ns quant.phase
  "The verdict -> phase mapping for the ISCO-08 2120 mathematicians, actuaries
  and statisticians actor, and what each phase is allowed to do.

  Runtime: portable `.cljc` (pure functions over data, no host interop).

  Why this namespace exists. The mapping used to be an inline `cond` inside
  the StateGraph's `:decide` node. Being inline, it could only be exercised by
  building and running a graph, so the routing rule that decides whether a
  finding is committed, held, or sent to a human had no test of its own and no
  name a ledger entry could carry.

  Three phases, and the ordering between them is the whole safety claim:

    :hold             hard violation. Never written. Not overridable.
    :request-approval escalation. Written only after a human resumes.
    :commit           clean. Written.

  `of-verdict` checks `:hard?` before `:escalate?` deliberately. A proposal
  that is both hard-blocked and escalating must hold, not escalate —
  escalating it would put a question to a human that they have no authority to
  answer yes to. For this actor the two conditions coincide on exactly the
  request most likely to be waved through: `:publish-finding` is an escalating
  operation *and* one that can carry an unregistered model, an unapproved
  method, or a significance claim at p = 0.9, so before this change the two
  met on the one operation whose premise is that a number is about to leave
  the building.

  Note precisely what this ordering does and does not currently protect.
  `quant.governor` already computes `:escalate?` as `(and (not hard?) ...)`,
  so a verdict carrying BOTH flags is a shape it does not emit today; the
  ordering here is the second of two independent guards, and the one that
  holds for any caller building a verdict by hand or for a future governor
  that stops zeroing the flag. It is therefore covered by a unit test over
  this pure function and NOT by `quant.sim` — reversing the two clauses leaves
  the whole scenario table green. That is stated rather than left for a reader
  to discover, because a guard whose only test runs through the graph would be
  a guard nobody is actually checking."
  )

(def phases
  "Every phase this actor can route to, with what it may do."
  {:hold             {:writes? false :human-required? false :terminal? true}
   :request-approval {:writes? false :human-required? true  :terminal? false}
   :commit           {:writes? true  :human-required? false :terminal? true}})

(defn of-verdict
  "Route a governor verdict to a phase. Pure."
  [verdict]
  (cond
    (:hard? verdict)     :hold
    (:escalate? verdict) :request-approval
    :else                :commit))

(defn writes? [phase] (boolean (get-in phases [phase :writes?])))
(defn human-required? [phase] (boolean (get-in phases [phase :human-required?])))
(defn terminal? [phase] (boolean (get-in phases [phase :terminal?])))

(defn refusal?
  "True for phases that did not write. Both `:hold` and `:request-approval`
  are refusals of the proposal as submitted — the second one is a refusal to
  act without a human, not an approval-in-waiting. `quant.sim` counts these;
  a run that produces none has demonstrated nothing."
  [phase]
  (not (writes? phase)))

(defn approved-commit?
  "True when a commit is being reached from an escalation, i.e. a human
  resumed the interrupted thread. The commit node records this so the audit
  ledger can distinguish a human-approved write from an automatic one —
  measured on the pre-change tree, it could not: a finding published outside
  the operator after human sign-off and a routine internal approval committed
  automatically left two `{:disposition :commit ...}` entries with no field
  telling them apart. For a number that leaves the building under this
  operator's name, that distinction is the entire reason the interrupt
  exists."
  [disposition]
  (= :request-approval disposition))
