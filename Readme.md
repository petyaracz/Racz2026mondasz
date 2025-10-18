1. verbs like mondsz/mondasz vary in linking vowel, this works different for different paradigm slots
2. identify variable verbs [x]
3. identify training data and relevant paradigm slots [x]
4. run phonological distance calculator for variable verbs x training data []
5. fit SVM on resulting distance matrices (see RaczRebrus2024cont for an implementation) []
6. check SVM predictions for various paradigm slots []
7. create nonwords []
8. run phon dist, generate SVM preds for nonwords x training data []
9. run "pick something" experiment for nonwords []
10. write paper []

idea: don't fit learners on each paradigm. instead, fit on stems but consider which stems vary in which paradigms.