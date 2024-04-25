# plantRdata 0.0.0.9000

* Creation of the package and adding the `NEWS.md` file to track changes.

* I wrote the workflow for latamMaps. Rocc works perfectly, I formatted lightly* just to check the flow and the object sizes. That's going to be very tricky. We are losing set_crs somewhere along the way, maybe it's PROJ, maybe it's another thing. The package is weighing 10MB. I am avoiding the complicated formatting on purpose.

* Created a preliminary `world` object to check the workflow. The package is heavier than it should but installation is OK. By setting plantRmaps::world, shares_border() passes all the tests, even the French Guiana problem n_n



