HAT-trick model
========================================================

The HAT-trick model was originally constructed by NAME as as series of Microsoft Excel spreadsheets and macros. The model simulates populations of the Tsetse fly (_latin_) within a gridded habitat and the associated transmission of Human African Trypanomiasis (Hat; sleeping sickness) to both humans and animals (nagana). 

The model consists of the following basic components: 

* A habitat - _a categorically classified grid of habitats within the study area_
* A tsetse fly population - _a vector of Tsetse flies, split by age in days, in each grid cell_
* A human population - _vector consisting of "children" and "adults" in each grid cell_
* A non-human population - _vector measured as ox equivalents in each grid cell - both livestock and wild species_

Habitat
--------

Habitat maps can be of any size but should be square. Note that increasing the size of this square (i.e. decreasing pixel size or increasing coverage area) will exponentially increase computation time. Habitat maps should be classiffied matrices with unique numbers representing habitat types. 

Tsetse population
------------------ 

The tsetse population is the main component of the model and is influenced by the habitat in each cell as well as the size of animal populations.

The tsetse population consists of two vectors each representing the age of tsetse flies from pupae over 15 days until they are on their fourth hunger cyle. One vector represents uninfected individuals and the other those carrying HAT. 

E.g. 

__ADD R CODE HERE__


Along with this vector, each tsetse species shoudl be accompanied by the following information (custom species can be added using __SPECIES__ as a default):

* Maximum age - the age at which the fly reaches the fourth hunger cycle
* Number of days spent as pupae
* Pupal survival rate
* Adult survival (changed by day if required)
* Fecundity (altered by day if necessary)
* Age at which breeding begins
* Probability of movement between grid cells (by day if necessary)
* Probability of infection on feeding (assumed to be the same for flies of all ages)
* Proability of detecting, visiting, probing and feeding on humans and non-humans (an associated object called feeds._species_)

E.g.

__ADD INFO HERE__

* Habitat preferences scaled by the preferred habitat for the species in question

E.g. 

__ADD EXAMPLE__



The growth of the population is simulated using a leslie matrix. Populated by the information associated with teh tsetse species in question.

E.g. 

__ADD TEXT EXAMPLE PLUS ACTUAL EXAMPLE FOR SPP__
