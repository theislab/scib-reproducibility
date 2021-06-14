The simulated datasets were generated using an extended version of the Splat simulation method available in the Splatter package (version 1.10.0).
The simulation model was modified to allow different cell group proportions and expected library sizes for each batch.
Basic quality control, involving the removal of cells > 2 median absolute deviations below the median of counts per cell or number of expressed genes per cell within each batch, was then performed on the simulated data using the quickPerCellQC function in the Scater package.
Genes expressed in <1% of cells in the whole simulation were also removed.
This resulted in an integration task consisting of 12,097 cells and 9,799 genes in six batches for simulation 1.
The final dataset was saved as a `.h5ad` file using the **anndata** Python package via **{reticulate}**.

To create the nested batch effect simulation scenario (simulation 2), we added a step between adjusting cell group proportions and downsampling counts in order to create a sub-batch structure.
For each sub-batch, we used the Splat model to simulate a second count matrix with the same number of cells as the sub-batch but a lower expected library size and no cell group structure.
We then added this noise matrix to the counts for cells in that sub-batch.
Quality control for the nested batch scenario was performed at the sub-batch level, and sub-batches were used as batch IDs for integration. The nested batch integration task consisted of 19,318 cells and 10,000 genes in 16 nested sub-batches (four sets of four sub-batches).

### Files

* `sim1.R` - Code used to produce simulation 1
* `sim2.R` - Code used to produce simulation 2
* `sample.R` - Function used to adjust cell type proportions in each batch
* `renv.lock` - **{renv}** R environment file specifying package versions used for simulation
* `environment.yml` - conda environment specifying Python packages used when exporting datasets
