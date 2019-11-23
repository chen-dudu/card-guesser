# card-guesser
This repository contians all the source code for COMP30020-Decalarative Programming project 1.
## Overview
For this project, you will implement a two-player logical guessing game. Two players face each
other, each with a complete standard deck of western playing cards (without jokers). One
player will be the answerer and the other is the guesser. The answerer begins by selecting
some number of cards from his or her deck without showing the guesser. These cards will
form the answer for this game. The aim of the game is for the guesser to guess the answer.
## Marks
### Correctness and Quality of guesses
- Marks: 68 / 70
  - 10 / 10 Correctness of your feedback function
  - 18 / 20 Correctness of your guessing for 2-card targets
  - 30 / 30 Quality of your guessing for 2-card targets
  - 2 / 5 Correctness of your guessing for 3- and 4-card targets
  - 5 / 5 Quality of your guessing for 3- and 4-card targets
### Quality of code
- Marks : 19 / 30
  - 3 / 5 Quality of file-level documentation, including authorship
  - 3 / 5 Quality of function- and type-level documentation
  - 4 / 5 Readability of code (lines not wrapped, good, consistent layout, no redundant comments)
  - 4 / 5 Understandability of code (good choice of names, subtleties documented, well organised)
  - 3 / 5 Appropriate abstraction (no chunks of repeated code, no really long functions, good types)
  - 2 / 5 Good use of language and libraries

- Assessment Criteria
  - 5 = practically flawless
  - 4 = very good, but not perfect
  - 3 = passable, but not great
  - 2 = unsatisfactory, but on the right track
  - 1 = very poor
  - 0 = no effort
## Comments from marker
- move the function comments (at the top of the file) to each function
- "elemRank" and "elemSuit" can be replaced by a more generic function
- comment for "deleteByRank" and "deleteBySuit" is not good, can't assume any about the reader
- overall : I didn't see any high order functions, even "map" and "filter" that we learn in the lectures. Try to apply them, do more research.