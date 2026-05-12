## Results

### Participants

- 76 participants
- Data collected October--December 2025 (2025-10-25 to 2025-12-18)
- Recruited from university participant pool; completed the task at home on their own devices
- Gender: ~32 female, ~32 male, ~12 missing or other [CHECK: two participants appear to have typed their name into the gender field]
- Year of birth: 1988--2006, median 2004
- Eight comprehension check questions; 75/76 participants passed at least 6/8; one participant (JFO3D9) passed 5/8 [DECISION NEEDED: include or exclude?]
- **ADD ETHICS STATEMENT**

---

### Task

- Online forced-choice sentence completion task, delivered via Pavlovia
- Instructions (paraphrase): participants were told they would see words, some familiar and some invented; for each word they would see two inflected forms and should pick whichever sounded more natural; no wrong answers
- Example: *Mari szívesen verekszik. Mi is szívesen...* -- choices: *verekszünk* / *verekedünk*
- Comprehension check warning: participants were warned that some trials were attention checks, and that wrong answers on those would cost points
- Trial structure: sentence-completion prompt displayed; two button choices presented in randomised left/right order; participant tapped/clicked their choice
- 320 trials per participant (80 nonce verbs × 4 paradigm slots)

---

### Stimuli

- 80 nonce verbs, 20 per coda type: *ng*, *nt*, *jt*, *st*
- Each nonce verb appeared in four paradigm slots: 2Sg (*mondasz/mondsz*), 3Pl (*mondanak/mondnak*), 1Sg>2 (*mondalak/mondlak*), 2Pl (*mondotok/mondtok*)
- Nonce verbs constructed by sampling phonological components (onset, first vowel, medial cluster) from real two-syllable Hungarian verbs; near-homophones of real words excluded (Levenshtein distance > 1 to any real lemma)
- Coda types selected to represent a range of phonological reduction potential: *ng* reduces to [ŋ], *nt* degeminates, *jt* and *st* are more stable clusters
- Corpus reference set: 193 lemmas with attested V/NV variation across the four target paradigm slots, drawn from Hungarian Webcorpus 2
- **CITE: Hungarian Webcorpus 2**

---

### Results

#### Corpus: linking-vowel log-odds by coda (V-form / NV-form frequency)

| Coda | log-odds | n lemmas | Interpretation |
|------|----------|----------|----------------|
| ng   | 0.97     | 59       | weak V preference |
| nt   | 5.35     | 42       | strong V preference |
| jt   | 6.51     | 17       | strong V preference |
| st   | 6.73     | 30       | strong V preference |

- *ng* is the outlier: cluster reduces to [ŋ], making the NV-form more acceptable

#### Corpus: linking-vowel log-odds by paradigm slot

| Slot | log-odds |
|------|----------|
| 2Pl (*mondotok*) | -0.04 |
| 1Sg>2 (*mondalak*) | 2.51 |
| 2Sg (*mondasz*) | 2.51 |
| 3Pl (*mondanak*) | 4.55 |

- 2Pl is near parity; 3Pl shows the strongest V preference in the corpus

#### Experiment: V-form choice rate by coda

| Coda | % V-form | log-odds |
|------|----------|----------|
| ng   | 46       | -0.15 |
| jt   | 73       | 0.99 |
| nt   | 73       | 0.99 |
| st   | 77       | 1.18 |

- *ng* is again the outlier; only coda type where participants preferred the NV-form
- *jt* and *nt* are near-identical; *st* slightly higher

#### Experiment: V-form choice rate by paradigm slot

| Slot | % V-form | log-odds |
|------|----------|----------|
| 2Pl (*mondotok*) | 61 | 0.44 |
| 1Sg>2 (*mondalak*) | 63 | 0.54 |
| 2Sg (*mondasz*) | 71 | 0.88 |
| 3Pl (*mondanak*) | 74 | 1.04 |

- Slot ranking mirrors the corpus ranking

#### Corpus--experiment correspondence

- Spearman rank correlation between corpus and experiment log-odds per consonant cluster (16 clusters): rho = 0.91, p < 0.001
- Clusters favouring the V-form in the corpus also favour it in the experiment
- Effect is compressed in the experiment (log-odds range much smaller than corpus), consistent with a weaker signal for nonce words
