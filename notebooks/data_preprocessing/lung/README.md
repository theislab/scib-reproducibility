The lung atlas task consists of three datasets with a total of 16 donors from a single publication (Vieira-Braga et al, Nat Med. 25 (2019)).
The datasets were profiled with different technologies on different sampling techniques.
The integration challenge consists of integrating across individuals and sampling locations and removing technical variation from protocol and sampling methods, while retaining nuanced cell identity signals and distinguishing rare cell types.
In this preprocessing notebook we show how the raw count data is preprocessed to generate the task dataset.
This includes quality control, normalization, harmonizing labels across the datasets, and filtering out sampling techniques and donors with poor cell type overlap.
