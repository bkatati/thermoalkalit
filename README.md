# Title: Aflatoxin-B1 Degradation and Nutrient Retention During Thermal-alkaline Preparation of *Nshima* from Maize Flour Porridge 

-----------------------------

Supporting files to manuscript:

DOI: (*under peer review*)

# *1) "AflaOxidation.R*"* # 
is run code, in base R, for statistical analysis of associated csv files. Code also provides visualisation of part of the data.

# *2) "AflOxidation.csv"* # 
contains the measured aflatoxin-B1 values (μg/kg) for maize flour, porridge and *Nshima*, including sodium bicarbonate fortified treatments.

- In the column 'ID' in the csv file, the treatment are represented as:

i) "MMeal (Raw) 1:9 5G58:Blank" is maize flour as starting material for porridge and *Nshima*.

ii) "Nshima" is the hardened porridge.

iii) "Nshima+HCO3" is *Nshima* treated with bicarbonate.

iv) "Nshima+HCO3+HCL" is *Nshima* treated with bicarbonate then later with HCl at 37*C for 3hrs.

v) "Porridge" is the untreated maize flour prepared as porridge.

vi) "Porridge+HCO3" is the bicarbonate treated porridge.

- In the column 'LbB1,' the values are the lower bound figures for aflatoxin-B1 signifying the use of zero for aflatoxin-B1 for values not detected by the instrument (HPLC), whose limit of detection (LoD) was 5 ug/kg. 

- Column 'B1' is the upper bound value using 5 ug/kg for any value less that the LoD. e.g. replacement of the value 5 for all zeroes of B1. In the analyses, the upper bound is used in place of lower bound.

# *3) "Nutrition.csv"* #
is csv file for the measured nutritional profiles of bicarbonated-treated *Nshima* and the control (untreated). All parameters are measured in % except for Energy measured as Kcal/100g. The parameters are on dry-weight basis
