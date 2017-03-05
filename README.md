# xmetric
Multi-Echelon Techniques for Replacable Inventory Control

This project is dedicated to the development of expository code in a progressive manner to generate an understanding of the various algorithms that can be used for optimization of spare part holdings in support of the maintainability of active systems of importance. This project is expected to cover a family of algorithms of increasing complexity having identification as METRIC, MOD-METRIC, and VARI-METRIC. The primary texts to be referenced for this study are "Optimal Inventory Modeling of Systems, Multi-Echelon Techniques, Second Edition" by Craig C. Sherbrooke and "Analysis and Algorithms for Service Parts Supply Chains" by John A. Muckstadt. Further, acknowledgement must be made of Jorge Fukuda's METRIC site, which has been an inspiration and invaluable guide to understanding some of the key functions required for these implementations. This site also references some additional supporting texts as well as his own presentation of this technical material.


Currently the project is in a learning and planning stage with a goal of some day reaching a full understanding of the primary texts. It is unknown whether this effort will ever lead to a comprehensive application framework; but, one could hope. Development is posted on R-forge, where the most recent source and binaries are usually available for annonymous download. Due to some unpredictabilitiy in the availability of our packages on R-forge an alternate download of nearly recent, but stable, versions of our source and Windows binary packages are provided here.


The software for this project is contained in a single R package, named xmetric. Below is a listing of the primary example functions that have been, or are expected to be developed under this project.


Kettelle 

This is a first learning step developing an understanding of the simplest single echelon, single indenture part distribution problem.


Marginal Analysis

A second step will develope the "knapsack" or "greedy heuristic" algorithm for the previous single echelon, single indenture case example.


Queues for Limited Resources

Jorge Fukuda has added an example case considering a realistic limitation of repair resources within the single echelon level. This seems an appropriate place to consider this complexity.


METRIC1

This function implements the basic METRIC algorithm as it is presented by Sherbrooke given an example of one part to be distributed across a depot and 5 bases. A multi-part solution attempt has been made, but appears to disagree with work by Wagner de Sousa Boges. This requires more study. 


METRIC2

This function is expected to extend the basic METRIC algorithm to account for multiple parts as presented by Wagner de Sousa Borges.


VARI.METRIC

This function is expected to extend the basic METRIC algorithm using a Variable Back Order (VBO) function, based on the negative binomial, to better approximate the occurence of demands for parts in the multi-echelon system.


MOD.METRIC

This function is expected to extend the basic METRIC and VARI-METRIC algorithms considering multi-indenture part assemblies.

