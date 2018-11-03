#### INPUT DATA REQUIREMENTS

Data should be input as a .txt or .csv file. The first two rows of the data file have information about the patients/specimens and their response/subtype; all remaining rows have gene expression data, one row per gene. In the case of Microarray gene expression data in which there are several probes corresponding to a single gene, a unique identifier would need to be created to separately identify each probe such as, 'Gene 1_p1', 'Gene1_p2' indicating Gene 1 has two probes. The columns represent the different experimental samples. A maximum of up to 10 different sample groups and 6 different gene groups may be used with this tool.

##### DATA FORMAT

1.	The first line of the file contains the gene identifier 'gene_id' (column 1), gene group classification 'Groups' (column 2) followed by the patient IDs e.g. TCGA.01.1A2B, one per column, starting at column 3. Column 1 gene identifier has to be labelled 'gene_id' and column 2 header should be labelled 'Groups' for using this tool. Other titles may cause the program to display errors. 
2.	The second line of the file contains the patient response classification e.g. Fav/Unf for favorable outcome group vs the unfavorable outcome group or Normal/Tumor, etc., in alphabetical order, starting at column 3. The first two columns for this row should be blank.
3.	The remaining lines contain gene expression measurements one line per gene, described in the format below.
a) Column_1. This should contain the gene name, for the user's reference. Each gene  name should be unique. When using microarray data, the gene and the probe id can be merged using a delimitor (except '|') to make a unique name.  Delimitors such as >,;:#%&(!)_+ are acceptable.
b) Column_ 2. This should contain the gene group classification e.g. O/U for Over-expressed/Under-expressed or Hyper/Hypo for hypermethylated/hypomethylated in alphabetical order. If only one gene group, use any alphabet e.g. A or na for each row instead. 
c) Remaining Columns. These should contain the expression measurements as numbers. Data inputted should be non-negative. Columns and rows with zero variance should be removed from the data. Rows containing missing expression measurements, should be also be removed from the input data or it will cause the tool to run into errors.

| gene_id           | Groups | GSM9981 | GSM1870  | GSM4618 | GSM7689  | GSM8772 | GSM1121  | GSM1250 | GSM3112  | GSM4987 | GSM1277 |
| -------------     |:------:| -------:|---------:|--------:|---------:|--------:|---------:|--------:|---------:|--------:|--------:|
|                   |        | MM      | MM       | MM      | MM       | MM      | MUGS     | MUGS    | NPC      | SM      | SM      |
| YWHAE>210996_s_at | A      | 1.47    |  2.18    | 5.87    |	9.12     |	7.34   | 1.56     |	3       |	7.77     |	3.4    |	1.56   |
| YWHAE>201020_at   | A      | 1.98    |  7.93    | 2.76	  | 9.11     |	8.46   | 0.98     |	5.98    |	8.19     |	8.91   |	5.98   |
| YWHAH>33323_r_at  | A      | 8.02    |  8       | 3.19	  | 11.86    |	6.54   | 8.17     |	2       |	0.99     |	2      |	1.17   |


