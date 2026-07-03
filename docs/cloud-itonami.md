# cloud-itonami-unspsc Integration

`cloud-itonami-unspsc-*` repositories publish independent-operator
commodity/service blueprints keyed by UNSPSC segment. `kotoba-unspsc`
declares what technology capabilities are required to run them.

Runtime flow:

```text
cloud-itonami-unspsc-{segment}
        |
        v
kotoba.unspsc/execution-plan
        |
        v
kotoba.technology/stack
        |
        v
concrete repos and operator services
```

The blueprint should not import a capability implementation directly. It
should request the capability contract from `kotoba-technology`.

## Maturity & readiness

`kotoba.unspsc/maturity-summary` and `execution-plan` expose per-segment
maturity and UI/export readiness so an operator console can show them.

| Maturity tier | Meaning |
|---|---|
| `:implemented` | source actor exists (reference implementation) |
| `:blueprint` | blueprint repo published (`:repo` set) |
| `:spec` | registry entry only (blueprint repo pending) |

Current state:

- Total entries: 53 (segment-level, a representative subset -- see README
  provenance note, not a verified complete official UNSPSC segment list)
- `:implemented` 0 · `:blueprint` 5 · `:spec` 48

Every entry requires `:robotics` (ADR-2607011000 robotics-premise, adopted
here for parity with `kotoba-industry` / `kotoba-occupation` /
`kotoba-cofog`): a robot performs the physical domain work under an actor
+ independent segment-specific governor.

## Boundary with the etzhayyim/root `unspsc` actor

`20-actors/unspsc` (etzhayyim/root) is the workspace's LIVE, authoritative,
COMMODITY-level (18,342-code) UNSPSC data table + dispatch framework,
serving real procurement/classification queries via
`kotodama.unspsc.dispatch` and the `unspsc.etzhayyim.com` XRPC gateway
(ADR-2606172100). `kotoba-unspsc` / `cloud-itonami-unspsc-*` is a
SEPARATE, SEGMENT-level (2-digit, ~53-entry) registry purpose-built for
the open-business-blueprint pattern -- it does not read from, write to, or
compete with the etzhayyim actor's commodity data. If a
`cloud-itonami-unspsc-*` operator business ever needs real commodity-level
classification at runtime, it should call the etzhayyim `unspsc` actor's
XRPC surface rather than re-deriving commodity data here.
