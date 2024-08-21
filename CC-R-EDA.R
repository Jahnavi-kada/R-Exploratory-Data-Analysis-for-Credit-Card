## Check the tables imported  
head(Customer_Acqusition)
head(spend)
head(Repayment)

## removing unwanted columns
cust <- Customer_Acqusition %>% select(-c(No))
View(cust)

spend <- spend %>% select(-c(`Sl No:`))
View(spend)

repay <- Repayment %>% select(-c(`SL No:`, `...5`))
View(repay)

## View Information
str(cust)
dim(cust)

str(spend)
dim(spend)

str(repay)
dim(repay)

## converting the datatype
repay$Month <- as.Date(repay$Month, format="%d-%b-%y")
str(repay)

spend$Month <- as.Date(spend$Month, format="%d-%b-%y")
str(spend)

class(spend$Month)
class(repay$Month)

## check the missing values 

na_val <- colSums(is.na(cust))
na_val

na_val_1 <- colSums(is.na(spend))
na_val_1

na_val_2 <- colSums(is.na(repay))
na_val_2

## check the duplicates 

dup_row <- cust[duplicated(cust), ]
dim(dup_row)

dup_row_1 <- spend[duplicated(spend), ]
dim(dup_row_1)

dup_row_2 <- repay[duplicated(repay), ]
dim(dup_row_2)

## Omitting the Na values

repay <- na.omit(repay)


  ## (1) In the above dataset
  ## (a) In case age is less than 18, replace it with mean of age values
  
  mean_age <- mean(cust$Age, na.rm = TRUE)
  cust$Age[cust$Age < 18] <- mean_age
  
  
  ## (b) In case spend amount is more than the limit, replace it with 50% of that customer’s limit
  ## (customer’s limit provided in acquisition table is the per transaction limit on his card)
  
  cust_spend <- merge(cust, spend, by = "Customer")
  head(cust_spend)
  
  cust_spend$Amount[cust_spend$Amount > cust_spend$Limit]
  
  cust_spend$Amount[cust_spend$Amount > cust_spend$Limit] <- cust_spend$Limit[cust_spend$Amount > cust_spend$Limit] * 0.5
  
  cust_spend$Amount[cust_spend$Amount > cust_spend$Limit]
  
  
  ## (c) Incase the repayment amount is more than the limit, replace the repayment with the limit
  
  cust_repay <- merge(cust, repay, by = "Customer")
  head(cust_repay)
  
  cust_repay$Amount[cust_repay$Amount > cust_repay$Limit]
  
  cust_repay$Amount[cust_repay$Amount > cust_repay$Limit] <- cust_repay$Limit[cust_repay$Amount > cust_repay$Limit]
  
  cust_repay$Amount[cust_repay$Amount > cust_repay$Limit]
  
  
  ## (2) From the above dataset create the following summaries:
  ## a) How many distinct customers exist?
  
  distinct_count <- n_distinct(cust$Customer)
  print(distinct_count)
  
  
  ## b) How many distinct categories exist?
  
  unique(cust$Segment)
  table(cust$Segment)
  
  ## c) What is the average monthly spend by customers?
  
  spend$Monthly <- format(spend$Month, "%B")
  head(spend)
  
  spend$Yearly <- format(spend$Month, "%Y")
  head(spend)
  
  cust_spend_group <- spend %>%
    group_by(Yearly, Monthly) %>%
    summarise(Mean_Amount = round(mean(Amount, na.rm = TRUE), 2))
  
  View(cust_spend_group)
  
  
  ## d) What is the average monthly repayment by customers?
  
  repay$Monthly <- format(repay$Month, "%B")
  head(repay)
  
  repay$Yearly <- format(repay$Month, "%Y")
  head(repay)
  
  cust_repay_group <- repay %>%
    group_by(Yearly, Monthly) %>%
    summarise(Mean_Amount = round(mean(Amount, na.rm = TRUE), 2))
  
  View(cust_repay_group)
  
  ##install.packages('dplyr')
  ##library(dplyr)
  
  ## e) If the monthly rate of interest is 2.9%, what is the profit for the bank for each month?
  
  cust_spend_repay <- merge(cust_spend, repay, by = "Customer")
  head(cust_spend_repay)
  
  cust_spend_repay <- cust_spend_repay %>%
    rename(Spend_Amount = Amount.x, Repay_Amount = Amount.y)
  head(cust_spend_repay)
  
  interest <- cust_spend_repay %>%
    group_by(Yearly, Monthly) %>%
    summarise(Sum_Spend_Amount = sum(Spend_Amount, na.rm = TRUE), 
              Sum_Repay_Amount = sum(Repay_Amount, na.rm = TRUE))
  
  print(interest)
  
  interest <- interest %>%
    mutate(Monthly_Profit = Sum_Repay_Amount - Sum_Spend_Amount)
  
  print(interest)
  
  interest <- interest %>%
    mutate(Interest_Earned = (2.9 * Monthly_Profit) / 100)
  
  print(interest)
  
  
  ## f) What are the top 5 product types?
  
  top_types <- spend %>%
    count(Type) %>%
    arrange(desc(n)) %>%
    slice_head(n = 5)
  
  View(top_types)
  
  ggplot(top_types, aes(x = reorder(Type, -n), y = n)) +
    geom_bar(stat = "identity", fill = "dodgerblue") +
    labs(title = "Top 5 Types", x = "Type", y = "Count") +
    theme_minimal()
  
  
  ## g) Which city is having maximum spend?
  
  city_spend <- cust_spend %>%
    group_by(City) %>%
    summarise(Total_Amount = sum(Amount, na.rm = TRUE)) %>%
    arrange(desc(Total_Amount))
  
  View(city_spend)
  
  ggplot(city_spend, aes(x = "", y = Total_Amount, fill = City)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y") +
    labs(title = "Amount Spent on Credit Card by Customers from Different Cities") +
    geom_text(aes(label = scales::percent(Total_Amount / sum(Total_Amount))), 
              position = position_stack(vjust = 0.5), color = "black") +
    theme_void() +
    theme(legend.position = "none") +
    scale_fill_brewer(palette = "Set3")
  
  
  ## h) Which age group is spending more money?
  
  cust_spend$Age_Group <- cut(
    cust_spend$Age,
    breaks = seq(18, 88, by = 8), 
    labels = c("18-26", "26-34", "34-42", "42-50", "50-58", "58-66", "66-74", "74-82"),
    include.lowest = TRUE,
    right = FALSE 
  )
  
  View(cust_spend)
  
  age_spend <- cust_spend %>%
    group_by(Age_Group) %>%
    summarise(Total_Amount = sum(Amount, na.rm = TRUE)) %>%
    arrange(desc(Total_Amount))
  
  View(age_spend)
  
  ggplot(age_spend, aes(x = "", y = Total_Amount, fill = Age_Group)) +
    geom_bar(stat = "identity", width = 1, color = "black") +
    coord_polar(theta = "y") +
    labs(title = "Total Amount Spent by Age Group") +
    geom_text(aes(label = scales::percent(Total_Amount / sum(Total_Amount), accuracy = 1)), 
              position = position_stack(vjust = 0.5), color = "white") +
    theme_void() +
    theme(legend.position = "none") +
    scale_fill_brewer(palette = "Set3")
  
  
  ## i) Who are the top 10 customers in terms of repayment?
  
  top_customers <- cust_repay %>%
    group_by(Customer) %>%
    summarise(Total_Amount = sum(Amount, na.rm = TRUE)) %>%
    arrange(desc(Total_Amount)) %>%
    head(10)
  
  View(top_customers)
  
  # install.packages("tidyr")
  # library(tidyr)
  
  # install.packages('devtools')
  # library(devtools)
  # devtools::install_github("tidyverse/tidyr")
  
  
  
  ## 3) Calculate the city wise spend on each product on yearly basis.
  ## Also include a graphical representation for the same
  
  cust_spend$Monthly <- format(cust_spend$Month, "%B")
  head(cust_spend)
  
  cust_spend$Yearly <- format(cust_spend$Month, "%Y")
  head(cust_spend)
  
  customer_spend_pivot <- cust_spend %>%
    group_by(City, Product, Yearly) %>%
    summarise(Total_Amount = sum(Amount, na.rm = TRUE), .groups = 'drop') %>%
    pivot_wider(names_from = Product, values_from = Total_Amount, values_fill = list(Total_Amount = 0))
  
  View(customer_spend_pivot)
  
  customer_spend_long <- customer_spend_pivot %>%
    pivot_longer(cols = -c(City, Yearly), names_to = "Product", values_to = "Total_Amount")
  
  View(customer_spend_long)
  
  ggplot(customer_spend_long, aes(x = City, y = Total_Amount, fill = Product)) +
    geom_bar(stat = "identity", position = "dodge") + 
    theme_minimal() +
    labs(title = "Total Amount Spent on Each Product by City and Year",
         x = "City",
         y = "Total Amount",
         fill = "Product") +
    facet_wrap(~ Yearly, ncol = 1) +  
    scale_fill_brewer(palette = "Pastel1")
  
  
  
  ## (4) Create graphs for
  ## (a) Monthly comparison of total spends, city wise
  
  month_city <- cust_spend %>%
    group_by(Monthly, City) %>%
    summarize(Total_Amount = sum(Amount, na.rm = TRUE)) %>%
    arrange(Monthly, City) %>%
    ungroup()
  
  View(month_city)
  
  month_city <- cust_spend %>%
    group_by(City, Monthly) %>%
    summarize(Total_Amount = sum(Amount, na.rm = TRUE)) %>%
    pivot_wider(names_from = Monthly, values_from = Total_Amount, values_fill = list(Total_Amount = 0))
  
  View(month_city)
  
  month_city_long <- month_city %>% 
    pivot_longer(cols = -City, names_to = "Monthly", values_to = "Total_Amount")
  
  View(month_city_long)
  
  ggplot(month_city_long, aes(x = City, y = Total_Amount, fill = Monthly )) +
    geom_bar(stat = "identity", width = 0.8, position = position_dodge()) +
    theme_minimal() +
    labs(title = "Monthly Spending by City", x = "City", y = "Amount Spent") +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
  ## (b) Comparison of monthly spend for each product (look for any seasonality that exists in terms of spend)
  
  product_wise <- cust_spend %>%
    group_by(Product, Monthly) %>%
    summarize(Total_Amount = sum(Amount, na.rm = TRUE)) %>%
    pivot_wider(names_from = Monthly, values_from = Total_Amount, values_fill = list(Total_Amount = 0))
  
  View(product_wise)
  
  product_wise_long <- product_wise %>%
    pivot_longer(cols = -Product, names_to = "Monthly", values_to = "Total_Amount")
  
  View(product_wise_long)
  
  ggplot(product_wise_long, aes(x = Product, y = Total_Amount, fill = Monthly)) +
    geom_bar(stat = "identity", width = 0.8, position = position_dodge()) +
    theme_minimal() +
    labs(title = "Amount Spent Monthly on Different Products", x = "Product", y = "Amount Spent") +
    theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
  ## (c) Comparison of yearly spend on air tickets
  
  air_tickets <- cust_spend %>%
    group_by(Yearly, Type) %>%
    summarize(Total_Amount = sum(Amount, na.rm = TRUE)) %>%
    ungroup() 
  
  filtered <- air_tickets %>%
    filter(Type == "AIR TICKET")
  
  View(filtered)
  
  ggplot(data = filtered, aes(x = Yearly, y = Total_Amount)) +
    geom_bar(stat = "identity", fill = "orange") +
    labs(x = "Year", y = "Amount Spent", title = "Comparison of Yearly Spend on Air Tickets") +
    theme_minimal()
  
  
  
  ## (5) Write user defined PYTHON function to perform the following analysis: 
  ## You need to find top 10 customers for each city in terms of their repayment amount by different products and by different time periods 
  ## i.e. year or month. The user should be able to specify the product (Gold/Silver/Platinum) and time period (yearly or monthly) 
  ## and the function should automatically take these inputs while identifying the top 10 customers
  
  cust_repay <- cust_repay %>%
    mutate(Monthly = format(Month, "%B")) %>% 
    mutate(Yearly = format(Month, "%Y"))
  
  head(cust_repay)
  
  summary_report <- function(product, timeperiod) {
    cat('Give the product name and timeperiod for which you want the data\n')
    
    product <- tolower(product)
    timeperiod <- tolower(timeperiod)
    
    cities <- c('BANGALORE', 'COCHIN', 'CALCUTTA', 'BOMBAY', 'CHENNAI', 
                'TRIVANDRUM', 'PATNA', 'DELHI')
    
    if (product == 'gold' && timeperiod == 'monthly') {
      result <- cust_repay %>%
        filter(Product == 'Gold') %>%
        group_by(City, Customer) %>%
        summarize(across(Monthly, sum, na.rm = TRUE)) %>%
        pivot_wider(names_from = Monthly, values_from = Amount) %>%
        filter(City %in% cities)
      
    } else if (product == 'gold' && timeperiod == 'yearly') {
      result <- cust_repay %>%
        filter(Product == 'Gold') %>%
        group_by(City, Customer) %>%
        summarize(across(Yearly, sum, na.rm = TRUE)) %>%
        pivot_wider(names_from = Yearly, values_from = Amount) %>%
        filter(City %in% cities)
      
    } else if (product == 'silver' && timeperiod == 'monthly') {
      result <- cust_repay %>%
        filter(Product == 'Silver') %>%
        group_by(City, Customer) %>%
        summarize(across(Monthly, sum, na.rm = TRUE)) %>%
        pivot_wider(names_from = Monthly, values_from = Amount) %>%
        filter(City %in% cities)
      
    } else if (product == 'silver' && timeperiod == 'yearly') {
      result <- cust_repay %>%
        filter(Product == 'Silver') %>%
        group_by(City, Customer) %>%
        summarize(across(Yearly, sum, na.rm = TRUE)) %>%
        pivot_wider(names_from = Yearly, values_from = Amount) %>%
        filter(City %in% cities)
      
    } else if (product == 'platimum' && timeperiod == 'monthly') {
      result <- cust_repay %>%
        filter(Product == 'Platimum') %>%
        group_by(City, Customer) %>%
        summarize(across(Monthly, sum, na.rm = TRUE)) %>%
        pivot_wider(names_from = Monthly, values_from = Amount) %>%
        filter(City %in% cities)
      
    } else if (product == 'platimum' && timeperiod == 'yearly') {
      result <- cust_repay %>%
        filter(Product == 'Platimum') %>%
        group_by(City, Customer) %>%
        summarize(across(Yearly, sum, na.rm = TRUE)) %>%
        pivot_wider(names_from = Yearly, values_from = Amount) %>%
        filter(City %in% cities)
      
    } else {
      return("Invalid product or time period specified.")
    }
    
    return(result)
  }
  
  
  summary_report('gold','monthly')
  
  
  
  
  
  
  
  
  
  
  
  
  
  
 
   