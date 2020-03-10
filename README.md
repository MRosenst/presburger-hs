# presburger-hs

A personal project intended to evaluate statements in [Presburger arithmetic](https://en.wikipedia.org/wiki/Presburger_arithmetic).

Presburger arithmetic is a first-order theory of arithmetic which only includes addition, which is
provably consistent, complete, and decidable, as it is too weak to fall under Goedel's
Incompleteness theorems. Since it is decidable, it is possible for an algorithm to determine the
truth-value of every meaningful statement in Presburger arithmetic. This program is designed to do
just that. However, the algorithm is doubly exponential and therefore it is infeasible to evaluate
more complex statements (i.e. with nested quantifiers).
