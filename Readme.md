## Paradigmatic analogy something something linking vowel, in Hungarian

### Concept

- verbs like mondsz/mondasz vary in linking vowel, this works different for different paradigm slots
- identify variable verbs across paradigm slots. do the same verbs vary in all paradigm slots? does this differ across slots?
- generate nonverbs and test them in a forced choice task across paradigm slots
- train learners using the variable real verbs as training data and the nonverbs as test data
- use "analogy" and "rules" learners (GCM and SVM) to model nonverb behaviour
- first, fit the learners on the 3sg canonical form for all varying verbs
- second, fit the learners across paradigm slots
- still fit the learners on the 3sg canonical form, but vary training sets depending on which verbs actually vary in a given paradigm slot
- this is two steps removed from what's probably actually going on: real verbs vary because certain paradigm slots are underdetermined (these verbs look a lot like both stable mondsz and stable mondasz verbs); this works differently in different paradigms (mondasz/mondsz versus mondlak/mondalak etc)
- but this approach still provides evidence on whether learners trained on different paradigms are more accurate in capturing what's going on in that paradigm
- this would be evidence for analogy across paradigm slots, not only canonical forms. 

### Workflow

1. build_data.R: identify stable and varying verbs across a range of paradigm slots
2. explore_data.R: further narrow down the varying verbs to a smaller set of paradigm slots and verb stems that actually vary at least a little!
3. build_words.R: create nonverbs based on the varying verbs
4. distance_maker: establish phonological distance between varying verbs and nonverbs, to calculate a full distance matrix. the julia script is built on Frisch and Pierrehumbert, Dawdy-Hesterberg and Pierrehumbert, and Rácz Beckner Hay and Pierrehumbert. the minimal classes are based on phonological features based on Siptár and Törkenczy and Rácz Rebrus and Tóth.
5. explore_distances.R: hand-filter the nonverbs, calculate distances for the hand-filtered nonverbs, and then pick a subset that are (more or less) evenly distributed across the similarity space so responses will be (more or less) maximally informative
6. fit_learners.R: fit the SVM and the GCM, training them on the variable verbs and predicting the nonverbs.
7. a simple forced-choice task on pavlovia to gather responses for the nonverbs. [https://gitlab.pavlovia.org/petyaraczbme/mondasz](link)
8. glm to compare participant picks vs predictions

4. explore_distances.R: