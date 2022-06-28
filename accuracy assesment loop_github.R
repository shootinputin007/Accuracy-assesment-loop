# Pavle Kerkez

### Loop meant for automatic writing down of accuracy assesment table for multiple indicators across multiple breakdowns.
### It was written during internship at National Institute for Public Health if Slovenia and was extremely useful for
### greatly increasing the speed of an otherwise tedious work.
### The dataset name and indicator names have been changed from the original names for simplicity's sake.

# Necessary libraries
library(haven)        # For loading the SPSS data
library(dplyr)        # For data wrangling
library(tidyverse)    # For various things
library(xlsx)         # For writing the Excel table

# Loading the data
EHIS <- read_sav("EHIS_data.sav")

# Defining percentages function
percenteagator<- function (y){
  percentage<- (y/n_row_valid)*100
}

## Preparing the data for the loop
# Breakdown variables- processing for R to understand
sex<- EHIS[,11]
age<- EHIS[,12]
education<- EHIS[,21]
education<- education$HATLEVEL
labour_status<- EHIS[,22]
labour_status<- labour_status$MAINSTAT

# Attaching the processed breakdown variables to the data
EHIS<- cbind(age,sex,education, labour_status,EHIS)
EHIS<- EHIS[,-15:-16] # removing one column with identical name because it causes problem in the function

# Creating separate datasets for each breakdown category
# Age
EHIS_a_total<-  EHIS[is.na(EHIS$YEARBIRTH) != T,]
EHIS_a_75over<- EHIS[EHIS$YEARBIRTH >= 1919 & EHIS$YEARBIRTH <= 1944,]
EHIS_a_65_74<-  EHIS[EHIS$YEARBIRTH >= 1945 & EHIS$YEARBIRTH <= 1954,]
EHIS_a_55_64<-  EHIS[EHIS$YEARBIRTH >= 1955 & EHIS$YEARBIRTH <= 1964,]
EHIS_a_45_54<-  EHIS[EHIS$YEARBIRTH >= 1965 & EHIS$YEARBIRTH <= 1974,]
EHIS_a_35_44<-  EHIS[EHIS$YEARBIRTH >= 1975 & EHIS$YEARBIRTH <= 1984,]
EHIS_a_25_34<-  EHIS[EHIS$YEARBIRTH >= 1985 & EHIS$YEARBIRTH <= 1994,]
EHIS_a_15_24<-  EHIS[EHIS$YEARBIRTH >= 1995 & EHIS$YEARBIRTH <= 2004,]

# Sex
EHIS_s_total<- EHIS[is.na(EHIS$SEX) != T, ] #for Sex-TOTAL
EHIS_s_m<-  EHIS[EHIS$SEX == 1, ] #for Sex- MALES
EHIS_s_f<-  EHIS[EHIS$SEX == 2, ] #for Sex- FEMALES

# Educational attainment
EHIS_e_total<- EHIS[is.na(EHIS$education) != T, ] #for Education- TOTAL
EHIS_e_low<-  EHIS[EHIS$education >= 0 & EHIS$education <= 2, ] #for Education- below ISCED1. Etc.
EHIS_e_mid<- EHIS[EHIS$education >= 3 & EHIS$education <= 4, ]
EHIS_e_high<- EHIS[EHIS$education >= 5 & EHIS$education <= 8, ]

EHIS_l_total<- EHIS[is.na(EHIS$labour_status) != T, ]
EHIS_l_employed<- EHIS[EHIS$labour_status == 10, ]
EHIS_l_unemployed<- EHIS[EHIS$labour_status == 20, ]
EHIS_l_retired<- EHIS[EHIS$labour_status == 30, ]
EHIS_l_inactive_other<- EHIS[EHIS$labour_status >= 40 & EHIS$labour_status <= 80, ]


# Attaching the breakdown datasets to a list
breakdowns_list<- list(EHIS_s_total, EHIS_s_m, EHIS_s_f, EHIS_a_total, EHIS_a_15_24, EHIS_a_25_34, EHIS_a_35_44,
                      EHIS_a_45_54, EHIS_a_55_64, EHIS_a_65_74, EHIS_a_75over,EHIS_e_total, EHIS_e_low,EHIS_e_mid,
                       EHIS_e_high,EHIS_l_total,EHIS_l_employed,
                       EHIS_l_unemployed, EHIS_l_retired, EHIS_l_inactive_other)

# Naming the list elements (different breakdown tables)
names(breakdowns_list)<- c("Sex-TOTAL","Male", "Female","Age- TOTAL", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74","75+",
                           "Education- TOTAL", "Lower", "Middle", "Higher", 
                           "Labour status- TOTAL","Employed","Unemployed", "Retired", "other")

## NOTE: There can be as many breakdown data tables as you like. You can also introduce multiple conditions while creating
## data tables, such as Females in the age group of 15-24 years, or some other combination. Usually, survey research done by
## Eurostat and other large bodies reports results across many breakdowns, usually combining them.


# Extracting column names from EHIS table to use in the loop later, because the loop messes up the column names
ehis_colnames<- colnames(EHIS)

# Creating a list of all categorical variable names stored as strings
# NOTE: The accuracy assesment process is different for numerical indicators and the calculations are different. However,
# the looping process is essentially the same.
categorical_vars<- as.list(c("Indicator1", "Indicator2", "Indicator3", "Indicator4" #, "Etc"
                             ))



#Assesment loop
for(x in categorical_vars){           # Loops for all the variable names in the list of categorical variable names
  
  for(i in 1:length(breakdowns_list)){      # Next level of looping- loops each name from the categorials list 
                                            # trough each of the breakdown tables
  
  breakdown_table<-breakdowns_list[i]                 # Selecting the breakdown table               
  breakdown_table<- as.data.frame(breakdown_table)    # Because R extracts it as a "list element", it needs to be transformed
  colnames(breakdown_table)<- ehis_colnames           # Attaching EHIS colnames to the breakdown table, because otherwise it is all messed up
  variable<- select(breakdown_table, x)               # Extracting the specified indicator from the specified breakdown table
  variable<- na.omit(variable)                        # Removing missing values from the indicator variable
  
  varName<- x             # Creating column with indicator name
  
  breakdown<- names(breakdowns_list[i])   # Creating column with the breakdown name (extracted from the breakdowns_list)
  
  first_var<- select(breakdown_table, x)         # This whole chunk of code is done in order to extract and apply weight to the calculations.
  weight<- breakdown_table[, 158]                # There was some problem related to the loop and functions used that messed things up,
  colnames(breakdown_table)<- NULL               # so I had to do it this way. Looking at it now, the "weight" variable could maybe
  breakdown_table1<- cbind(first_var, weight, breakdown_table) # be extracted outside of the loop, but if I recall correctly there was some
                                                               # issue with that as well, which is why I "complicated it" like this.
  variable_tab<- na.omit(breakdown_table1 %>% count(breakdown_table1[,1], wt= weight))
  
  values<- variable_tab[,2]
  val_names<- variable_tab[,1]
  #val_names<- rownames(variable_tab) # In cases it malfunctions, use this one.
  
  if(length(val_names)==0){         # If-statement to filter out empty rows. Ideally, it would write them down, but this causes errors.
    val_names<- 0                   # Will solve it soon.
  }
  
  if(length(values)==0){
    values<- 0
  }
  
               
  n_row_valid<- sum(values) # Extracting the number of valid rows for specific indicator in a specific breakdown

  
  percentages<- sapply(values,percenteagator)     # Creating percentages column with the defined function applied to value frequencies
  if(is.nan(percentages) == T){
    percentages<- 0
  }
  
  percentages1<- percentages/100
  standard_error<-percentages1*(1- percentages1) / n_row_valid     # Creating the accuracy column
  if(is.nan(standard_error)== T){
    standard_error<- 0
  }
  standard_error<- sqrt(abs(standard_error))      # Rooting the transformed result of percentages, because R can't handle imaginary numbers
  
  
  prevalence_rate<- if (standard_error > 0.1){
                            prevalence_rate<- ".."
                    }else if (standard_error < 0.1){
                            prevalence_rate<- round(100 * percentages)
                    }
  
  accuracy<-  if (standard_error > 0.05 & standard_error < 0.15){
               accuracy<- "Less accurate"
             } else if (standard_error > 0.15){
               accuracy<- "Not accurate"
             } else if (standard_error < 0.05){
               accuracy<- " "
             }
  
  
  sum_tab<- cbind(varName, breakdown, val_names, values, n_row_valid, 
                  percentages, standard_error, prevalence_rate, accuracy)   # Binding the created columns into a table
  sum_tab<- sum_tab[order(val_names),]                                      # Ordering the table by the order of value labels (numbers)
  sum_tab<- as.matrix(sum_tab)                                              # Transforming it into a matrix for better handling
  

  
  sum_tab<- if(ncol(sum_tab) != 9){     # R sometimes randomly transposes the created table so that it writes the columns as rows.
    sum_tab<- t(sum_tab)                # I have no idea why it does this, but it messes everything up.
  } else if (ncol(sum_tab) == 9){       # For this reason, I wrote this else-if statements to just check the number of columns
    sum_tab<- sum_tab                   # and if the number is not 9 (the proper number of columns), it transposes the table back to normal.
  }
  
  
  
  write.table(sum_tab, file="ehis_assesment.tab", sep="\t", col.names=F,row.names=F, append=T, quote=F)   # Writing the created table into a file
  
  }

}     # Loop repeats for all combinations and writes them all in the file.


# Loading the file with data as data table
loaded_data<- read.csv("ehis_assesment.tab", sep = "\t", header = F)

# Attaching column names to the uploaded data table
colnames(loaded_data)<- c("Indicator","Breakdown", "Values", "N of respondents for each value", "Valid N of respondents", 
                          "% of respondents", "Standard error", "Prevalence rate", "Accuracy")
# Viewing the table
view(loaded_data)

# Writing Excel table
write.xlsx(loaded_data, file= "assesment1.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

# Cleaning the file, so that it can be properly used next time
cat("", file = "ehis_assesment.tab")
