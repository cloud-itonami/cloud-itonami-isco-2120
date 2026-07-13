# cloud-itonami-isco-2120

Open Business Blueprint for **ISCO-08 2120**: Mathematicians, Actuaries and Statisticians — an ISCO
**Wave 1 (design & governance)** occupation per ADR-2607121000. This
is the THIRD wave-1 blueprint batch: management/professional work is
cognitive, **no robotics gate** — eligible for actor implementation
now.

**Maturity: `:implemented`** — QuantitativeProfessionalsAdvisor ⊣
QuantitativeProfessionalsGovernor as a langgraph StateGraph
(`intake → advise → govern → decide → commit/hold`, human-approval
interrupt), modeled on cloud-itonami-isco-4311's bookkeeping actor.
14 tests / 28 assertions green.

The statistical HARD invariants — arithmetic and set membership, not
a judgement call:

1. **Significance arithmetic** — a finding claimed as `:significant`
   must report a p-value ≤ the model's registered significance
   threshold (alpha).
2. **Method membership** — the proposed analysis method must be a
   member of the model's registered approved-methods set — swapping
   methodology after the fact requires prior disclosure.

Also HARD: unregistered/foreign model, unregistered organization,
non-`:propose` effect. Escalations (always human sign-off):
`:publish-finding` (external publication), low confidence (< 0.6).

AGPL-3.0-or-later, forkable by any qualified operator. Part of the
[cloud-itonami](https://itonami.cloud) open business fleet.
