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
68 tests / 413 assertions green, plus a 30-scenario governed-scenario
harness demonstrating 27 refusals.

## What the actor refuses

The statistical HARD invariants — arithmetic and set membership, not
a judgement call:

1. **Significance arithmetic** — a claim that asserts significance
   must report a p-value on [0,1] **at or below** the model's
   registered alpha. The registered alpha is the largest p-value that
   still supports the claim, so the line itself conforms.
2. **Method membership** — the proposed method must be declared and a
   member of the model's registered approved-methods set. Swapping
   methodology after the fact requires prior disclosure.

Also HARD: an operation outside the declared vocabulary; a **reserved**
operation (statutory sign-off, reserving, pricing, deleting the model
registry, disabling the audit trail); an unregistered or foreign
client; an unusable model (unregistered, or registered without an
alpha or without approved methods); a claim outside the declared
vocabulary; a **reserved claim** (`:proven`, `:causal`, `:replicated`
— no alpha makes these true); an unusable confidence; a
non-`:propose` effect.

Escalations (always human sign-off): `:publish-finding` (external
publication) and low confidence (< 0.6). **An escalation is checked
against every invariant before it is put to a human** — reaching a
person with an empty violation list is the failure this actor was
rebuilt to prevent.

## Layout

| namespace | holds |
|---|---|
| `quant.operation` | the closed operation vocabulary: `supported` and `reserved`, and whether an op binds to a model |
| `quant.facts` | well-formedness of every compared value, the claim vocabulary, and the significance arithmetic itself |
| `quant.governor` | the decision. It does not also define what there is to decide about |
| `quant.advisor` | proposes only. Never validates; a malformed request must reach the governor to be refused |
| `quant.phase` | verdict → phase (`:hold` / `:request-approval` / `:commit`), hard before escalate |
| `quant.ledger` | append-only audit trail with a portable hash chain, and who approved each write |
| `quant.store` | SSoT: clients, models, records, ledger |
| `quant.actor` | the wired StateGraph |
| `quant.sim` | the governed-scenario harness |

## Running it

```bash
kbb -M:test    # 68 tests / 413 assertions
kbb -M:sim     # 30 scenarios, 27 refusals; exits 1 if the table refuses nothing
kbb -M:lint
```

`quant.sim` answers three questions a unit test does not: does the
*wired graph* refuse (not just the pure `check`), does an escalated
request interrupt rather than write, and does the ledger it leaves
behind verify and record who approved each write. It **refuses to
report a pass** on a table that demonstrated no refusal — a harness
that only ran clean scenarios would print green while showing nothing.

AGPL-3.0-or-later, forkable by any qualified operator. Part of the
[cloud-itonami](https://itonami.cloud) open business fleet.
