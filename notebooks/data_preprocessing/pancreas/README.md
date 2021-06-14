The human pancreas analysis task consists of six publicly available human pancreas datasets. 
Specifically, we used a pre-annotated collection of four datasets from the [Satija lab](https://satijalab.org/seurat/v3.0/integration.html) 
(retrieved on 28/08/2019) with accession codes GSE81076 (CEL-seq), 
GSE85241 (CEL-seq2), GSE86469 (Fluidigm C1) (all GEO), and E-MTAB-5061 (SMART-Seq2) (ArrayExpress). 
The two additional human pancreas datasets were provided in a pre-annotated format by the 
[Hemberg lab](https://hemberg-lab.github.io/scRNA.seq.datasets/human/pancreas/) (retrieved on 28/08/2019); 
their GEO accession codes are GSE84133 (inDrop) and GSE81608 (SMARTer). 
It should be noted that only the SMARTseq2 (E-MTAB-5061) and the inDrop (GSE84133) datasets contained integer count data, 
whereas the CEL-seq (GSE81076) and CEL-seq2 (GSE85241) datasets were adjusted by the authors to represent count-like data. 

In the preprocessing and normalization notebooks, we merged and jointly normalized all datasets that contained count data with scran pooling. 
This excluded the dataset from Xin et al., which was provided in normalized units of RPKM. Finally, all datasets were log+1-transformed. 
In total, there were 16,382 cells in the pancreas integration task with 18,771 genes. 
Each dataset was treated as a batch, except for the inDrop dataset, in which each donor was treated as a batch.
