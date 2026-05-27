# pipaux Presentation — All Diagrams

All Mermaid diagrams for the team presentation. Paste each code block
into [mermaid.live](https://mermaid.live) to preview.

---

## Diagram 1 — Data Flow (audience: everyone)

Shows where aux data comes from, where pipaux sits, and where it goes.

```
flowchart TD
    subgraph sources["DATA SOURCES"]
        direction LR
        GP["Poverty GP\n(PFW, CPI, PPP)"]
        WDI["WDI\n(GDP, Population)"]
        EXT["External\n(Maddison, WEO)"]
        MAN["Manual\n(NaN rules, missing data)"]
    end

    subgraph github["GITHUB — aux_* repositories"]
        direction LR
        R1[aux_cpi]
        R2[aux_ppp]
        R3[aux_pfw]
        R4[aux_gdp]
        R5[aux_pop]
        R6[aux_weo]
        R7[...]
    end

    subgraph pipaux_box["pipaux"]
        direction LR
        S1["Sync branches"]
        S2["Validate & format"]
        S3["Detect changes"]
        S4["Save with provenance"]
    end

    subgraph ydrive["Y-DRIVE — Release-scoped artifacts"]
        direction LR
        F1[".qs2 + sidecar\n(versioned)"]
        F2[".qs2 + sidecar\n(versioned)"]
        F3["..."]
    end

    GP --> github
    WDI --> github
    EXT --> github
    MAN --> github

    github --> pipaux_box
    pipaux_box --> ydrive

    style GP fill:#4a90d9,color:#fff
    style WDI fill:#5ba55b,color:#fff
    style EXT fill:#e8923a,color:#fff
    style MAN fill:#999,color:#fff
    style pipaux_box fill:#fff,stroke:#333,stroke-width:3px
    style github fill:#f0e6ff,stroke:#9370db
    style ydrive fill:#e6ffe6,stroke:#5ba55b
```

---

## Diagram 2 — Pipeline Architecture (audience: everyone, devs get detail)

The internal decision logic when updating measures.

```
flowchart TD
    START["update_aux_measures()"]

    subgraph dep_resolution["1. DEPENDENCY RESOLUTION"]
        direction TB
        READ_DEP["Read dependency manifest\n(from GitHub YAML)"]
        TOPO["Topological sort\n→ execution order"]
        READ_DEP --> TOPO
    end

    subgraph per_measure["2. FOR EACH MEASURE (recursive)"]
        direction TB
        PROC{"Already\nprocessed?"}
        SKIP_PROC(("Skip"))
        DEPS["Process upstream\ndependencies first\n(recursive aux_fun)"]

        subgraph checks["STATUS CHECKS"]
            direction TB
            GH{"GH: Release branch\nin sync with DEV?"}
            SYNC["Sync / Create branch"]
            YD{"Y-drive: artifact\nup to date?"}
            YD_SUB["• Raw SHA match?\n• Code hash match?\n• Sidecar exists?"]
            SKIP_ALL(("Skip\nNo work needed"))
        end

        EXEC["Execute update"]
        EXEC_SUB["• Load raw data\n• Validate (raw + output)\n• Format\n• pip_aux_save()"]

        LOG["Log entry\n(measure, step, SHA, status)"]
    end

    DONE["Done → aux_log_summary()"]

    START --> dep_resolution
    dep_resolution --> per_measure
    PROC -- "YES" --> SKIP_PROC
    PROC -- "NO" --> DEPS
    DEPS --> GH
    GH -- "NO / Missing" --> SYNC --> YD
    GH -- "YES (in sync)" --> YD
    YD --- YD_SUB
    YD -- "ALL MATCH" --> SKIP_ALL
    YD -- "MISMATCH" --> EXEC
    EXEC --- EXEC_SUB
    EXEC --> LOG
    per_measure --> DONE

    style dep_resolution fill:#f0e6ff,stroke:#9370db
    style checks fill:#fff3cd,stroke:#ffc107
    style EXEC fill:#cce5ff,stroke:#4a90d9
    style SKIP_ALL fill:#f0f0f0,stroke:#999
    style SKIP_PROC fill:#f0f0f0,stroke:#999
    style DONE fill:#5ba55b,color:#fff
    style START fill:#4a90d9,color:#fff
```

---

## Diagram 3 — Actual Dependency Graph (audience: everyone)

Built from the real `read_dependencies()` YAML manifest. Darker = more upstream dependencies.

```
flowchart TD
    %% Tier 0: No dependencies (leaf nodes)
    country_list["country_list"]
    pfw["pfw"]
    wdi["wdi"]
    sna["sna"]
    maddison["maddison"]
    nan["nan"]

    %% Tier 1: Depends on Tier 0 only
    ppp["ppp"]
    pop["pop"]
    cpi["cpi"]
    regions["regions"]
    countries["countries"]
    metadata["metadata"]

    %% Tier 2: Depends on Tier 0 + Tier 1
    weo["weo"]
    pce["pce"]
    gdm["gdm"]
    censoring["censoring"]

    %% Tier 3: Depends on multiple tiers
    gdp["gdp"]

    %% Tier 4: Depends on nearly everything
    missing_data["missing_data"]

    %% === EDGES (from → to means "to" depends on "from") ===

    country_list --> ppp
    country_list --> pop
    country_list --> cpi
    country_list --> regions
    country_list --> countries
    country_list --> weo
    country_list --> gdm
    country_list --> pce
    country_list --> censoring
    country_list --> gdp
    country_list --> missing_data

    pfw --> countries
    pfw --> metadata
    pfw --> gdm
    pfw --> censoring
    pfw --> missing_data

    pop --> weo
    pop --> gdp
    pop --> missing_data

    wdi --> pce
    wdi --> gdp
    wdi --> missing_data

    sna --> pce
    sna --> gdp
    sna --> missing_data

    maddison --> gdp
    maddison --> missing_data

    nan --> gdp
    nan --> missing_data

    countries --> censoring
    regions --> censoring

    weo --> gdp
    weo --> missing_data

    pce --> missing_data
    gdp --> missing_data

    %% === STYLING ===
    %% Single hue (blue), darker = more dependencies

    style country_list fill:#e6f2ff,stroke:#4a90d9,color:#1a1a1a
    style pfw fill:#e6f2ff,stroke:#4a90d9,color:#1a1a1a
    style wdi fill:#e6f2ff,stroke:#4a90d9,color:#1a1a1a
    style sna fill:#e6f2ff,stroke:#4a90d9,color:#1a1a1a
    style maddison fill:#e6f2ff,stroke:#4a90d9,color:#1a1a1a
    style nan fill:#e6f2ff,stroke:#4a90d9,color:#1a1a1a

    style ppp fill:#cce5ff,stroke:#4a90d9,color:#1a1a1a
    style pop fill:#cce5ff,stroke:#4a90d9,color:#1a1a1a
    style cpi fill:#cce5ff,stroke:#4a90d9,color:#1a1a1a
    style regions fill:#cce5ff,stroke:#4a90d9,color:#1a1a1a

    style weo fill:#99caff,stroke:#2a6db5,color:#1a1a1a
    style countries fill:#99caff,stroke:#2a6db5,color:#1a1a1a
    style gdm fill:#99caff,stroke:#2a6db5,color:#1a1a1a
    style metadata fill:#99caff,stroke:#2a6db5,color:#1a1a1a

    style pce fill:#66afff,stroke:#1a5290,color:#fff
    style censoring fill:#66afff,stroke:#1a5290,color:#fff

    style gdp fill:#3385d6,stroke:#0d3d73,color:#fff
    style missing_data fill:#1a5290,stroke:#0a2e52,color:#fff
```

---

## Diagram 4 — Recursive Resolution Process (audience: developers)

Traces what happens when `aux_fun("gdp")` is called — deps first, then self.

```
flowchart TD
    CALL["aux_fun(measure = 'gdp')"]
    CHECK_PROC{"'gdp' in\nprocessed?"}
    READ_DEPS["Read dependencies\nfor 'gdp':\nwdi, weo, maddison,\nsna, nan, pop"]

    subgraph recursive["RECURSIVE CALLS"]
        direction TB
        D1["aux_fun('wdi')"]
        D2["aux_fun('weo')"]
        D3["aux_fun('maddison')"]
        D4["aux_fun('sna')"]
        D5["aux_fun('nan')"]
        D6["aux_fun('pop')"]
    end

    MARK["Mark 'gdp' as processed"]
    STATUS["check_github_status()\ncheck_y_drive_status()"]
    UPDATE["execute_update('gdp')"]

    CALL --> CHECK_PROC
    CHECK_PROC -- "YES" --> SKIP(("Return\nimmediately"))
    CHECK_PROC -- "NO" --> READ_DEPS
    READ_DEPS --> recursive
    recursive --> MARK
    MARK --> STATUS
    STATUS --> UPDATE

    style recursive fill:#f0e6ff,stroke:#9370db
    style SKIP fill:#f0f0f0,stroke:#999
    style CALL fill:#4a90d9,color:#fff
    style UPDATE fill:#cce5ff,stroke:#4a90d9
```

---

## Diagram 5 — Loading Architecture (audience: developers)

How different measure types load their data.

```
flowchart TD
    subgraph loading["HOW DATA IS LOADED"]
        direction TB
        Q{"What type\nof measure?"}

        subgraph gh_raw["GH-RAW measures"]
            direction TB
            LOAD_GH["pipfun::load_from_gh()\n→ returns data + gh attribute\n(owner, repo, branch, SHA)"]
            EX1["aux_cpi, aux_ppp, aux_pfw,\naux_country_list, aux_wdi,\naux_weo, aux_nan, aux_cp..."]
        end

        subgraph derived["DERIVED measures"]
            direction TB
            LOAD_AUX["pipload::load_aux_data()\n→ reads from Y-drive\n(inherits gh attributes from inputs)"]
            EX2["aux_countries, aux_regions,\naux_metadata, aux_gdp,\naux_pce, aux_indicators..."]
        end

        subgraph external["EXTERNAL measures"]
            direction TB
            LOAD_EXT["Direct URL download\n→ NO gh attribute\n(change detection via code_hash only)"]
            EX3["aux_maddison\n(option: pipaux.madsrc)"]
        end

        subgraph multi_file["MULTI-FILE measures"]
            direction TB
            LOAD_MULTI["Multiple load_from_gh() calls\n→ list of gh attributes\n(any SHA mismatch → full re-save)"]
            EX4["aux_cp, aux_censoring,\naux_gdm (GH + inventory.fst)"]
        end
    end

    Q -- "Raw from GitHub" --> gh_raw
    Q -- "Computed from\nother measures" --> derived
    Q -- "External URL" --> external
    Q -- "Multiple files" --> multi_file

    subgraph save["SAVING (all types)"]
        SAVE_FN["pip_aux_save()\n• data (.qs2)\n• sidecar: gh, code_hash, code_label, pk"]
    end

    gh_raw --> save
    derived --> save
    external --> save
    multi_file --> save

    style gh_raw fill:#e6f3ff,stroke:#4a90d9
    style derived fill:#fff3cd,stroke:#ffc107
    style external fill:#ffe6e6,stroke:#d9534f
    style multi_file fill:#f0e6ff,stroke:#9370db
    style save fill:#e6ffe6,stroke:#5ba55b
```

---

## Diagram 6 — Change Detection & Comparison (audience: everyone)

Post-update verification tools.

```
flowchart TD
    subgraph compare["POST-UPDATE: COMPARISON TOOLS"]
        direction TB

        subgraph releases["compare_aux_releases()"]
            direction TB
            CR_IN["Input:\n• measures (vector)\n• old_release (auto-detected or specified)"]
            CR_LOGIC["For each measure:\n1. Load current release from Y-drive\n2. Load previous release from Y-drive\n3. Identify key columns (pk)\n4. Compare via myrror::myrror()"]
            CR_OUT["Output per measure:\n• diff_values (changed cells)\n• diff_rows (added/removed rows)\n+ attributes: key_cols, paths, releases"]
        end

        subgraph vintages["compare_aux_vintages()"]
            direction TB
            CV_IN["Input:\n• measures (vector)\n• version (-1 = prev, -2 = two back)"]
            CV_LOGIC["For each measure:\n1. Load latest version\n2. Load earlier vintage (stamp history)\n3. Compare via myrror::myrror()"]
            CV_OUT["Output per measure:\n• diff_values (changed cells)\n• diff_rows (added/removed rows)\n+ attributes: version IDs, paths"]
        end
    end

    UPDATE_DONE["Updates complete"] --> compare
    CR_IN --> CR_LOGIC --> CR_OUT
    CV_IN --> CV_LOGIC --> CV_OUT

    subgraph use_cases["USE CASES"]
        UC1["'What changed between\nthis release and last?'"]
        UC2["'Did my re-run change\nanything within this release?'"]
    end

    CR_OUT --> UC1
    CV_OUT --> UC2

    style releases fill:#e6f3ff,stroke:#4a90d9
    style vintages fill:#fff3cd,stroke:#ffc107
    style use_cases fill:#e6ffe6,stroke:#5ba55b
```

---

## Diagram 7 — Per-Measure Specificities (audience: developers / reference)

Special cases and exceptions in the system.

```
flowchart LR
    subgraph special_cases["SPECIAL CASES & EXCEPTIONS"]
        direction TB

        S1["country_list & income_groups\nRepo: GPID-WB/Class\n(not PIP-Technical-Team)"]

        S2["nan\nOwner override:\nPIP-Technical-Team always"]

        S3["countries & missing_data\nGitHub check SKIPPED\n(no aux_* repo on GH)"]

        S4["maddison\nExternal URL source\nNo gh attribute\nChange detection: code_hash only"]

        S5["gdm\nHybrid: GH raw + inventory.fst\n(fixed Y-drive path dependency)"]

        S6["gdp / pce\nDerived composite:\nWDI + WEO + Maddison + SNA + NAN\nPublished to GH first, then re-read\nfor provenance capture"]

        S7["cp (Country Profiles)\nMulti-file: many CSVs/DTAs\nAny single file change →\nfull republication"]
    end

    style S1 fill:#f0e6ff,stroke:#9370db
    style S2 fill:#f0e6ff,stroke:#9370db
    style S3 fill:#fff3cd,stroke:#ffc107
    style S4 fill:#ffe6e6,stroke:#d9534f
    style S5 fill:#ffe6e6,stroke:#d9534f
    style S6 fill:#e6f3ff,stroke:#4a90d9
    style S7 fill:#e6f3ff,stroke:#4a90d9
```

---

## Summary: Which diagrams to use when

| # | Diagram | Audience | Suggested use |
|---|---------|----------|---------------|
| 1 | Data Flow | Everyone | Opening — sets the scene |
| 2 | Pipeline Architecture | Everyone | Core of the talk |
| 3 | Dependency Graph | Everyone | Visual impact — complexity of the system |
| 4 | Recursive Resolution | Developers | If asked "how does ordering work?" |
| 5 | Loading Architecture | Developers | Shows source diversity & design |
| 6 | Comparisons | Everyone | "After updating, here's how you verify" |
| 7 | Per-Measure Specificities | Reference | Backup slide or handout |

**Recommended for the 30-min talk:** Diagrams 1, 2, 3, 6.
**Backup (show if time/questions allow):** Diagrams 4, 5, 7.
