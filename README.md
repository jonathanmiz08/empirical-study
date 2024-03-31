# empirical-study
An empirical study comparing in-place radix, quick, heap, and insertion sorting algorithms.
For each sorting function, excluding helper functions, a vector of integers is passed as a parameter.

Racket File:
The empirical-study function takes a factor (natnum) and a list of sorters as parameters.
For the given factor a vector of random integers is generated of the length (factor * 500).
Each sorting function sorts the vector of random integers in non-decreasing order and 
the runtime results are displayed, composing of the number of milliseconds of CPU time 
required to obtain the result, the number of "real" milliseconds required for the result, 
and the numnber of milliseconds of CPU time spent on garbage collection.

The given factor is subtracted by 1 after each iteration of the empirical-study function and 
stops when factor is equal to 0.

Credit to Professor Marco Morazan of Seton Hall Univesity for the empirical-study, heap-sort-in-place!,
and qs_in_place! functions that were used for this empirical study.

Excel File:
The excel file contains a table of run times (cpu time - cpu time spent on garbage collection) for each
length of vector used for each sorter. A seperate tab contains a changeable graph to visualize the difference
in performance of the different sorting functions.
