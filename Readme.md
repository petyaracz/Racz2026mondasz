[![DOI](https://zenodo.org/badge/1062793079.svg)](https://doi.org/10.5281/zenodo.22681904)

## Linking vowel alternation in Hungarian verb paradigms

### Concept

Hungarian CC-final verbs show alternation between a linking-vowel form (V-form) and a no-linking-vowel form (NV-form) in four paradigm slots: 2Sg (*mondasz* ~ *mondsz*), 3Pl (*mondanak* ~ *mondnak*), 1Sg>2 (*mondalak* ~ *mondlak*), and 2Pl (*mondotok* ~ *mondtok*). These are the quasi-analytic inflectional slots identified by Rebrus.

The project asks what predicts V-form preference: phonological structure of the final consonant cluster, paradigmatic analogy, or corpus frequency. The short answer (see `summary.txt`) is that phonological reduction of the coda cluster accounts for corpus behaviour almost entirely; analogy and raw counts add nothing. This is indirect evidence against a paradigm-based account of the alternation.

### Workflow

#### 1. Corpus extraction (`script/build_data.R`)

Reads verb forms from Hungarian Webcorpus 2 (via DuckDB and a parquet frequency list). Filters to the four target paradigm slots, excludes ik-verbs, prefixed forms, geminate-final lemmas, and V-final lemmas. Pairs V-forms and NV-forms per lemma × slot, recovers NV-forms that were attested but did not pass the initial `linking_vowel` filter, and computes log-odds of the V-form. Produces two output files:

- `dat/corpus_cc_varies.tsv` -- lemmas with both forms attested (log-odds defined); used as the experiment stimulus pool
- `dat/corpus_cc.tsv` -- all CC-final lemmas including categorical cases; adds phonological boundary features (coda bigram, stem-suffix trigram) for modelling

#### 2. Nonword generation (`script/build_words.R`)

Builds nonce verbs with four target codas: *ng*, *nt*, *jt*, *st*. Samples onsets, first vowels, and medial clusters independently from real 2-syllable verb forms in Webcorpus 2. Assigns linking vowel by vowel harmony. Filters out near-homophones of real lemmas (minimum Levenshtein distance > 1). Generates V and NV inflected forms for all four paradigm slots. Writes four sheets to a Google Sheets file for hand-filtering.

#### 3. Experiment construction (`script/build_experiment.R`)

Takes the hand-filtered nonword stimuli (`dat/nonwords_final.tsv`), pivots to long, builds sentence-completion prompts with *Mari* as subject, and bundles both forms per trial. Writes JSON to `dat/mondasz.js` and to the Pavlovia experiment repository.

#### 4. Data processing (`script/process_experiment.R`)

Reads raw participant CSVs from the Pavlovia data folder. Extracts device metadata, survey responses (participant ID, gender, year of birth), and comprehension check scores. Filters to experimental trials (stimulus starts with *Mari*), attaches stimulus information, and flags V-form choices. Outputs:

- `dat/exp_data_tidy.csv.gz` -- trial-level tidy data
- `dat/passed.tsv` -- participant IDs who passed comprehension checks (at least 6/8)

#### 5. Analysis (`script/paper.R`, `script/agg_results.R`)

`script/paper.R` compares corpus and experiment log-odds across coda types and paradigm slots. Computes per-item and per-cluster log(V/NV) for both sources, produces the boxplot and corpus-vs-experiment scatter figures in `viz/`, and tests the rank correlation between them (Spearman). Uses `ggthemes` (Tufte boxplots) and `patchwork` for multi-panel figures.

`script/agg_results.R` recomputes the same corpus/experiment log-odds and Spearman correlation as plain console output, for pulling numbers into `Results.md`.

### Data files

| File | Description |
|------|-------------|
| `dat/corpus_cc.tsv` | All CC-final verb lemmas × paradigm slots from Webcorpus 2, with coda structure, form frequencies, log-odds, and phonological boundary features |
| `dat/corpus_cc_varies.tsv` | Subset of `corpus_cc.tsv`: only lemmas with both V and NV forms attested |
| `dat/nonwords_final.tsv` | Hand-filtered nonce stimuli; one row per nonce lemma, with V and NV forms for all four paradigm slots |
| `dat/correct_answers_to_checks.tsv` | Answer key for the eight comprehension check questions in the experiment |
| `dat/passed.tsv` | Participant IDs retained after comprehension check filtering |
| `dat/exp_data_tidy.csv.gz` | Tidy trial-level forced-choice responses with RT, participant metadata, and stimulus info |

### Dependencies

R packages: `tidyverse`, `glue`, `duckdb`, `hunspell`, `stringdist`, `jsonlite`, `googlesheets4`, `ggthemes`, `ggrepel`, `DescTools`, `patchwork`, `knitr`.

External: Hungarian Webcorpus 2 frequency list (parquet, not included); Pavlovia experiment and data directory (not included).

### Reproducibility notes

- All scripts assume the repository root as the working directory (`setwd('~/Github/Racz2026mondasz/')`); edit this line to match your local checkout before running.
- `script/build_data.R`, `script/build_words.R`, and `script/process_experiment.R` also read from absolute paths outside this repository (the Webcorpus 2 frequency list, a sibling `Racz2024` repo, and a local Pavlovia data export) that are not included here — see "External" above. These three scripts cannot be re-run from this archive alone.
- When exporting this repository for archiving (e.g. to Zenodo), use `git archive` or a fresh clone rather than zipping the working directory, so untracked local files (`.DS_Store`, editor/tool directories) don't end up in the deposited snapshot.
