---
date: 2026-05-27
title: "pipaux Team Presentation — Structure and Content"
status: decided
scope: "Extended"
chosen-approach: "Layered Narrative (adjusted)"
tags: [presentation, communication, team, pipaux]
---

# pipaux Team Presentation — Structure and Content

## Context

The PIP technical team needs to present the work done on pipaux to the
broader PIP team (developers + economists). The goal is to showcase what
was built, how it works, and why it matters. 30-minute in-person slot.

## Requirements

- **Audience:** PIP team — backend developers (technical) + economists (analytical)
- **Goal:** Present the system that was built, its capabilities, and design decisions
- **Tone:** Solution-first; link back to old pain points briefly ("this solved X")
- **Duration:** 30 minutes in person
- **Visuals:** Two Excalidraw diagrams:
  1. High-level data flow (sources → raw → input → output)
  2. Pipeline architecture (dependency graph, status checks, GH/Y-drive sync)
- **Key takeaways:**
  - Aux data updates are now fully automated and traceable
  - Change detection — nothing gets overwritten silently
  - Dependencies between measures are handled automatically
- **Style:** Technical and conceptual mixed throughout — show the work done

## Approaches Considered

### Approach 1: Layered Narrative (chosen, adjusted)

2 min problems → 18 min system walkthrough (both diagrams, mixed depth) →
5 min demo → 5 min Q&A. Technical details woven in naturally rather than
separated into layers.

### Approach 2: Visual-First

Entire talk built around progressive diagram reveals. Rejected — too much
design effort and poor standalone handout value.

### Approach 3: Two-Track Deck

Main deck + technical appendix. Rejected — artificial split unnecessary
for a 30-min talk where the audience knows each other.

## Decision

Approach 1 (adjusted): compressed problem statement (2 min, introduced by
someone else or as brief anchors), then the bulk is a walkthrough of the
system mixing conceptual and technical depth. Both diagrams used during the
main walkthrough. Brief demo or recorded screencast near the end.

## Proposed Structure (≈30 min)

### 1. Context & Pain Points (2 min)
- Slide: "What is auxiliary data?" — 3 types (raw, input, output), multiple sources
- Brief bullet list of old challenges (each linked to the solution coming next):
  - Manual triggers → now automated
  - No checks across stages → now validated
  - No versioning tied to releases → now release-scoped
  - Silent overwrites → now change-detected

### 2. The System: Data Flow (8 min)
- **Diagram 1: Data flow** (Excalidraw)
  - Sources: PovcalNet GP (PFW, CPI, PPP), WDI, external, manual
  - Stages: raw → formatted (input) → output (Y-drive)
  - Show where pipaux sits: orchestrating the raw→input transition
  - Highlight: GP-managed data (PFW, CPI, PPP) as most critical inputs
- Narration: walk through a single measure (e.g. CPI) end-to-end

### 3. The System: Pipeline Architecture (10 min)
- **Diagram 2: Pipeline internals** (Excalidraw)
  - Dependency graph resolution
  - Status checks: GH branch sync + Y-drive SHA/code-hash comparison
  - Decision logic: skip / update GH / update Y-drive
  - Logging and traceability
  - Versioning (stamp: content-based snapshots)
- Key points to narrate:
  - "One command updates everything in the right order"
  - "Change detection avoids unnecessary work"
  - "Every update is logged with provenance (who, when, what SHA)"
  - "Versioning is tied to releases — you can always go back"

### 4. Quick Demo (5 min)
- Live or pre-recorded:
  - `pipfun::setup_working_release()`
  - `update_aux_measures(measures = "cpi", log = TRUE)`
  - `aux_log_summary()` → show the measure/status table
  - `compare_aux_releases(measure = "cpi")` → show diff output
- Fallback: screenshots if environment unavailable

### 5. Q&A (5 min)

## Diagram Content Notes

### Diagram 1 — Data Flow (audience: everyone)
```
┌─────────────┐   ┌─────────────┐   ┌─────────────┐
│ PovcalNet GP│   │     WDI     │   │  External   │
│ (PFW,CPI,PPP)  │ (GDP,POP...)│   │ (Maddison..)│
└──────┬──────┘   └──────┬──────┘   └──────┬──────┘
       │                  │                  │
       ▼                  ▼                  ▼
┌─────────────────────────────────────────────────┐
│              GitHub aux_* repos (RAW)           │
│         (one repo per measure, DEV branch)      │
└──────────────────────┬──────────────────────────┘
                       │
                       ▼  ← pipaux orchestrates this
┌─────────────────────────────────────────────────┐
│         Formatted & Validated (INPUT)           │
│    (release-scoped branches, change-detected)   │
└──────────────────────┬──────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────┐
│            Y-Drive (OUTPUT)                     │
│   (versioned .qs2 artifacts + sidecar metadata) │
└─────────────────────────────────────────────────┘
```

### Diagram 2 — Pipeline Architecture (audience: developers + curious economists)
```
update_aux_measures()
  │
  ├─ Read dependency manifest
  ├─ Topological sort → execution order
  │
  └─ For each measure:
       ├─ check_github_status()
       │     branch missing? → create + sync
       │     branch behind DEV? → sync
       │     up to date? → skip GH
       │
       ├─ check_y_drive_status()
       │     SHA mismatch? → re-format + save
       │     code hash changed? → re-format + save
       │     all good? → skip entirely
       │
       ├─ execute_update()
       │     sync GH branch + regenerate Y-drive artifact
       │
       └─ log entry (measure, step, timestamp, provenance)
```

## Next Steps

- [ ] Create Excalidraw diagrams (both)
- [ ] Draft 4-5 slides (minimal text, diagram-heavy)
- [ ] Prepare demo script or record screencast
- [ ] Dry-run timing (target: 25 min to leave buffer)
