# Impliment the ReaxFF-nn to the GULP package

ReaxFF-nn stands for Reactive Force Field (ReaxFF) with neural network bond-order correction. In the current function form, the short range interactions are mostly computed by neural networks, and long term interactions such as VDW and Coulomb terms are using ReaxFF formula. In this perspective, ReaxFF-nn is no different than other MLPs. Most neural network potentials focus on short-range interactions, and long-range interactions still a major challenge, as most models using static point charges that are assigned prespecified. One exception is ReaxFF, who use charges iteratively determined by the electronegativity equalization, and more method can be expected in the future implementation.

* The details of how to implement to GULP can be seen in "ReaxFFnnToGULP.md"
* The The library file used with this work is named as "reaxff_nn.lib"

## Refference

ReaxFF-nn: ReaxFF-nn: A Reactive Machine Learning Potential in GULP and the Applications in the Thermal Conductivity Calculation of Carbon
Nanostructures, Feng Guo et al. (Submitted).


