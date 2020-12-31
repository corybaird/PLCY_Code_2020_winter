
#Session notes can be found at the link below
#https://nbviewer.jupyter.org/github/corybaird/PLCY_Code_2020_winter/blob/main/Code_sessions/Session_1/Session_1_Notes.ipynb

##A.3.1 Import libraries

#Step 1 

#install.packages('dplyr')
#install.packages('gapminder')

#Step 2

library('dplyr')
library('gapminder')

gapminder = gapminder

##A.3.2 Import data
gapminder %>% head(5)


### 1. DPLYR review

#1.1 Select
## Pipe operator shortcut on mac is command+shift+M

gapminder %>%   
select(country, year, gdpPercap) 


#1.2 Filter
#1.2.1 Filter by 1 condition 
gapminder %>% 
filter(continent==2007) 


#1.2.2 Filter by 2 conditions 
gapminder %>% 
filter(year>1990 & year<2007) %>% 
head(2)


#1.3 Mutate
gapminder = gapminder %>% 
mutate(gdp_log = log(gdpPercap)) 


#1.4 Summarize 
gapminder  %>% 
summarise(mean_pop = mean(pop),
         median_pop = median(pop))


#1.4.1 Summarize and filter, chain two functions
gapminder  %>% 
filter(year==2007) %>% 
summarise(mean_pop = mean(pop))


#1.5 Groupby
gapminder  %>%
group_by(continent) %>% 
summarise(mean_gdp = mean(gdpPercap))


### 2. Data check

#2.1 Data types: str(DF_NAME)
str(gapminder)


#2.2 Summary stats: summary(DF_NAME)
summary(gapminder)


#2.3 Check for NA: is.na()
gapminder %>% 
is.na() %>% 
  any()


#2.4 Drop na: na.omit
#add na then drop

#2.4.1 Add NA row at the bottom of dataset
gapminder = gapminder %>% rbind(c(NA,NA, NA, NA, NA, NA))
gapminder %>% tail(2)


#2.4.2 Re-check for na
gapminder %>% 
is.na() %>% 
any()


#2.4.3 na.omit()
gapminder = gapminder %>% 
na.omit() 


### 3. Data Manipulation

#3.1 Dummy variable
gapminder_2007 = gapminder %>% filter(year==2007)

gapminder_2007 %>% head(5)


#3.1.1 Add dummy for high-income countries
gapminder_2007 = gapminder_2007 %>% 
mutate(highinc_dummy = as.numeric(gdpPercap>10000))



gapminder_2007  %>% head(2)


#3.1.2 Dummies are useful for summary stats
gapminder_2007  %>% 
group_by(highinc_dummy) %>% 
summarise(gdp_mean = mean(gdpPercap),
         pop_mean = mean(pop))


#3.2 Mapping values
gapminder_2007  %>% 
mutate(highinc_dummy_factor = recode(highinc_dummy, '0'='Low', '1'='High')) %>% 
tail(2)


#3.3 Cut-off dummies
cutoffs = c(seq(40, 100, by = 10))
cutoffs


gapminder_2007 = gapminder_2007  %>% 
mutate(cut_variable = 
         cut(gapminder_2007$lifeExp, cutoffs, 
             include.lowest=TRUE))

gapminder_2007 %>% head(3)

gapminder_2007 %>% 
group_by(cut_variable) %>% 
summarise(mean_gdp = mean(gdpPercap))


### 4. Misc

#4.1 Rename column
gapminder_2007 = gapminder_2007  %>% rename('gdp'='gdpPercap') 


#4.2 Unique
gapminder_2007 %>% 
select(continent) %>% 
unique()


#4.3 Table(row, column)
df_polity = read.csv('https://raw.githubusercontent.com/corybaird/PLCY_610_public/master/Discussion_sections/Disc4_PS2/demo.csv')
df_polity %>% head(2)


#4.3.1 Frequency table
freq_table = df_polity %>% select(wealth, regime) %>% table()
freq_table

#add row and column names for freq table
rownames(freq_table) = c('Wealth 1', 'Wealth 2', 'Wealth 3')
colnames(freq_table) = c('Regime 1', 'Regime 2', 'Regime 3')
freq_table 

#4.3.2 Proportion table
prop.table(freq_table)


#4.4 Filter list
country_list = c('Albania', 'Italy', 'France', 'Belgium')
gapminder_2007  %>% filter(country %in% country_list)


#4.5 Case when

gapminder_2007 %>% 
mutate(language = case_when(country=='Spain'~'Spanish', 
                           country=='Italy' ~ 'Italian', 
                           country=='United Kingdom'~'English')) %>% na.omit()


#4.6 Count
gapminder_2007  %>% count(highinc_dummy)


#4.7 Export data
#gapminder_2007 %>% write.csv('FILENAME.csv')
#gapminder_2007 %>% write.xlsx('FILENAME.xlsx')


### 5. Merge

#5.1 Merge rows
df_1 = gapminder_2007[1:3, ]
df_1

df_2 = gapminder_2007[5:7, ]
df_2

#5.1.1 rbind
rbind(df_1, df_2)


#5.2 Merge columns
df_1 = gapminder_2007[1:3, c('year','lifeExp')]
df_1
df_2 = gapminder_2007[5:7, c('continent','country')]
df_2

cbind(df_1, df_2)


#5.3 Merge rows and columns
#5.3.1 Case data
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
nyt_cases_df = read.csv(url)
nyt_cases_df  %>% head(3)


#5.3.2 Mask data
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/mask-use/mask-use-by-county.csv'
nyt_mask_df = read.csv(url)
nyt_mask_df %>% head(3)



#5.3.3 Merge
#5.3.3.1 For the column merge on make sure the name is the same in both data sets
nyt_mask_df = nyt_mask_df %>% rename('fips'='COUNTYFP')

nyt_mask_df %>% names()
nyt_cases_df %>% names()


merge(nyt_cases_df, nyt_mask_df, by='fips')


### End