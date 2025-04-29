Resist the urge to...

* **Make the adaptive dynamics code its own package**. This will happen after I submit my thesis.

* **Only load the functions needed for a given analysis**. By default all the R scripts in `functions/` are loaded. That's overkill and messy, but that's okay.

* **Move the description of the data** from their respective analyses to the data themselves (especially for the climate change experiment)

* **Generalize non-linear trade-off implementation** now that linear is a special case of the non-linear implementation. It is okay to combine them in the manuscript, but let us not rewrite all the scripts (plus, the linear implementation is much faster since it does not call the power function).

* **Update [simarray](https://github.com/rscherrer/simarray) to use it**. Simply download the scripts maybe and postpone the improvement of that package to later.

* **Reorganize and clean up my space on the cluster**. Now is not the time for that.