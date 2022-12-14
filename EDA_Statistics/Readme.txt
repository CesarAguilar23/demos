


In this work we analyze educational attainment, city size, marital status, and age as factors that may explain differences in income for the population of Mexico. 
Using a publicly available dataset, we conducted several statistical tests to identify which demographic factors have an impact in income. 

For this work, we use publicly available data generated by the National Institute of Statistics and Geography, or Instituto Nacional de Estadística y Geografía (INEGI) 
of Mexico. INEGI is an autonomous public institution, established in 1983, which collects and disseminates information about Mexico’s population, economy, resources, 
and territory. The dataset used is the 2016 National Survey on Household Income and Expenditure or Encuesta Nacional de Ingresos y Gastos de los Hogares 2016 (ENIGH). 
This is a Survey sampled on a national level, with very detailed questions that encompass several facets of the participant’s economic and social life. 

The data is accessed through INEGI’s website (INEGI, 2021) which includes aggregated data in spreadsheets, summaries of relevant results as well as methodological 
information in PDF format, and non-aggregated data in formats such as csv. The non-aggregated data was used for this work. 
The ENIGH’s data structure is a relational database (CITE), in which different tables link to each other via a primary “key” variable. This allows for flexibility 
when using the data, as a single table with a great number of variables could be too unwieldly for most purposes. The ENIGH databases encompasses 11 tables with around 
800 variables in total. For the purposes of this work, we use 4 tables: población(population), viviendas(housing), hogares(households), and ingresos(income).

These tables were linked using several key variables. The final table included 10 variables: 'folioviv'(household key), 'foliohog'(housing key), 'numren'(individual key),
'edad'(age), 'nivelaprob'(educational attainment), 'edo_conyug'(marital status), 'ing_tri'(quarterly income), 'tam_loc'(city size), and id_persona(synthetic key).
This table is at the level of the individual. The final table underwent a process of data cleaning. First, all “NA” or missing values present in the used variables were 
cleared. Then, we kept only the individuals aged 16 years or older, as it is the minimum age to work without parental consent in Mexico. 
This “clean” table contains 148,659 records, representing the same number of individuals. 

