# DECO — Dyslexia Eye Tracking COrpus

Eye-tracking data from 20 Dutch–English bilingual adults with dyslexia reading
*The Mysterious Affair at Styles* (Agatha Christie) — approximately 60,000 words —
split across their first language (Dutch) and second language (English).
DECO is a direct extension of the
[GECO corpus](https://doi.org/10.3758/s13428-016-0734-0) of typical reading,
collected under matched conditions to enable dyslexia vs. control comparisons
in both L1 and L2.

> **Preprint:** Boeve, S., François, L., Salgado Figueroa, E., Badaya, E.,
> Bogaerts, L., & Dirix, N. (2025). *Dyslexia Eye Tracking COrpus (DECO):
> First- and Second Language Book Reading in Adults with Dyslexia.*

---

## Repository structure

```
DECO/
├── ET Data/
│   ├── DECO.csv          # Word-level eye-tracking measures (all participants, L1 & L2)
│   └── DECO.rds          # Same data as an R object (faster to load in R)
│
├── Material/
│   ├── DutchMaterials.csv    # Word-level stimulus properties for the Dutch half
│   └── EnglishMaterial.csv   # Word-level stimulus properties for the English half
│
├── PP Data/
│   └── PPInfo - DECO.xlsx    # Participant demographics and language proficiency scores
│
├── R Scripts/
│   ├── 01_LanguageTests.R            # Language proficiency analyses (Tables 1–2)
│   ├── 02_DECO_ReadingTimes.R        # Reading-time descriptives and L1–L2 comparisons (Tables 3–5)
│   ├── 03_DECO_GECO_Comparison.R     # DECO vs. GECO group comparisons
│   └── 04_Supplementary.R            # Supplementary analyses
│
└── Appendix/
    ├── Misalignments Session 1-2.xlsx   # Interest-area misalignments across data-collection periods
    └── Supplementary material.pdf       # Description of the variables and supplementary tables and figures
```

---

## Eye-tracking measures

The following word-level measures are included in `ET Data/DECO.csv`:

| Column | Description |
|--------|-------------|
| `FFD`  | First-fixation duration (ms) |
| `SFD`  | Single-fixation duration (ms) |
| `GD`   | Gaze duration (ms) — sum of first-pass fixations |
| `TRT`  | Total reading time (ms) — all fixations including regressions |
| `GPT`  | Go-past time (ms) — fixations before moving right, including regressions |
| `SKIP` | Word skipping (1 = skipped in first pass) |
| `REFIX`| Refixation (1 = fixated more than once in first pass) |

See the preprint for full preprocessing details and filtering criteria.

---

## Participants

- N = 20 (15 women, 5 men), mean age 21.8 years
- All had a formal diagnosis of developmental dyslexia, confirmed at university enrolment
- Dutch–English bilinguals; mean L2 age of acquisition: 11 years
- All enrolled in higher education at Ghent University at time of testing

Participant-level language proficiency measures (LexTALE, spelling, lexical decision
accuracy, LEAP-Q) are available in `PP Data/PPInfo - DECO.xlsx`.

---

## Citation

If you use DECO in your research, please cite:

```
Boeve, S., François, L., Salgado Figueroa, E., Badaya, E., Bogaerts, L., & Dirix, N. (2025).
Dyslexia Eye Tracking COrpus (DECO): First- and Second Language Book Reading in Adults
with Dyslexia. [Preprint]
```

---

## Funding

This project was supported by the Research Foundation – Flanders (FWO; grant 11P3T24N,
awarded to Sam Boeve) and the Special Research Fund (BOF) of Ghent University
(BOF.PDO.2018.0010.01, awarded to Nicolas Dirix).

---

## Ethics

Data collection was approved by the Ethics Committee of Ghent University
(references: 2020/6 Nicolas Dirix; 2024-127A). All identifying information has been
removed from the data files.

---

## License

Data files (`ET Data/`, `Material/`, `PP Data/`, `Appendix/`) are licensed under
[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/) — you are free to share
and adapt them for any purpose, provided you give appropriate credit.

Code (`R Scripts/`) is licensed under the
[MIT License](https://opensource.org/licenses/MIT).

---

## Contact 

sam [dot] boeve [at] ugent [dot] be

