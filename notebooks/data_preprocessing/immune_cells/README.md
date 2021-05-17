The immune cell atlas contains immune cells from 8 datasets comprising human and mouse cells from bone marrow and peripheral blood.
Bone marrow datasets were retrieved from Oetjen et al.[1] (3 human donors), Dahlin et al.[2] (4 mouse samples), and the Mouse Cell Atlas[3] (MCA; 3 mouse samples).
For peripheral blood data, mouse samples were downloaded from the MCA[3] (6 samples) and human samples were obtained from 10X Genomics[4], Freytag et al.[5], Sun et al. [6] and Villani et al.[7].

Two separate integration tasks were investigated: the first, considering only human samples (n=10); the second, merging human and mouse samples (n=23).
For both integration tasks, we could identify the following challenges:

1. Inter-sample variability arising from the different donors
2. Integration across single-cell protocols (10X and smart-seq2 in Villani’s sample)
3. Capturing consistent cell populations across tissues of origin
4. Separation of cell subtypes that are transcriptomically similar
5. Preservation of tissue-specific cell annotations as separate clusters
5. Conservation of the trajectory of erythrocyte development across batches

The immune cells task that integrates human and mouse samples adds two more challenges namely, cross-species integration inside the same tissue of origin and cross-species integration between tissues.

The preprocessing notebooks contain the analysis of each dataset in terms of quality control, normalization, cell type labelling and harmonization across datasets.
We also illustrate how the dataset merging between human and mouse tissues was performed.

### References

1.	Oetjen, K. A. et al. Human bone marrow assessment by single-cell RNA sequencing, mass cytometry, and flow cytometry. JCI Insight 3, (2018).
2.	Dahlin, J. S. et al. A single-cell hematopoietic landscape resolves 8 lineage trajectories and defects in Kit mutant mice. Blood 131, e1–e11 (2018).
3.	Han, X. et al. Mapping the Mouse Cell Atlas by Microwell-Seq. Cell 173, 1307 (2018).
4.	Datasets -Single Cell Gene Expression -Official 10x Genomics Support. https://support.10xgenomics.com/single-cell-gene-expression/datasets/3.0.0/pbmc_10k_v3.
5.	Freytag, S., Tian, L., Lönnstedt, I., Ng, M. & Bahlo, M. Comparison of clustering tools in R for medium-sized 10x Genomics single-cell RNA-sequencing data. F1000Res. 7, 1297 (2018).
6.	Sun, Z. et al. A Bayesian mixture model for clustering droplet-based single-cell transcriptomic data from population studies. Nat. Commun. 10, 1649 (2019).
7.	Villani, A.-C. et al. Single-cell RNA-seq reveals new types of human blood dendritic cells, monocytes, and progenitors. Science 356, (2017).
