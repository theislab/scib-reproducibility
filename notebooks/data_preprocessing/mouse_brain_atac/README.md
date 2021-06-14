The ATAC brain integrations tasks are divided into two parts, both using adult mouse brain datasets from 10x Genomics, Cusanovich et al. 2018 and Fang et al. 2019.
The first part consists of three datasets (1 batch per dataset) subsampled to contain ~3000 cells each.
The second part contains 11 batches from the same 3 datasets as the first part.
In both cases, we processed the ATAC data using either 5kb windows, peaks or gene activity matrices.

In the preprocessing notebooks, we show how to filter, normalise and harmonise the data to later benchmark the tools for batch effect removal.
The datasets were profiled by different sources using different technologies.
The integration challenge is to remove technical variation between batches and to consider the nested batches in the large integration tasks while the biological information and the cell structure are still retained.

Note that these notebooks do not include earlier sequence processing steps such as alignment and peak calling.
