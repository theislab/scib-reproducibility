The mouse brain RNA task consisted of four publicly available scRNA-seq (Saunders et al (Dropseq protocol), Zeisel et al (10X Genomics protocol) and Tabula Muris (Smart-seq2 protocol)) and snRNA-seq mouse brain studies (Rosenberg et al, SPLiT-seq protocol), in which additional information on cerebral regions was provided.

In this preprocessing notebook we show how the available count data is preprocessed to generate the task dataset. This includes quality control, normalization, harmonizing labels across the datasets.
For data integration, we used the study labels.
