# KMC-haskell
Attempting to make a haskell KMC

# lattice setup

Takes an input lattice from a file and initialises the simulation.

Input file describes base lattice in form
Index, Type, (xord,ycord,zcord), Occupancy, Entity, Dentacity [Neighbours]

Index translates fine to line.
Type is an integer
x,y,z are 3 real floats
occupancy is an integer
entity is an integer
Dentacity is an integer
[Neighbours] is a series of integers.

eg.
T (cords) [neighbours] ent occ dent
1 (0,0,0) [2,3] 1 0 1 
