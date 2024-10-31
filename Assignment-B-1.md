Assignment B1
================
Linoja
2024-10-31

#### Exercise 1: Make a Function & Exercise 2: Document your Function

``` r
# Function to extract unique values from a specified column in a data frame
#' Title
#'
#' @param data 
#' @param column 
#'
#' @return
#' @export
#'
#' @examples
extract_unique_values <- function(data, column) {
  # Check if the data is a data frame
  if (!is.data.frame(data)) {
    stop("The input data must be a data frame.")
  }
  
  # Check if the specified column exists in the data frame
  if (!column %in% colnames(data)) {
    stop(paste("Column", column, "does not exist in the data frame."))
  }
  
  # Extract and return unique values from the specified column
  unique_values <- unique(data[[column]])
  return(unique_values)
}
```

#### Exercise 3: Include examples

``` r
# Load necessary libraries of dataset
library(gapminder)
library(palmerpenguins)

# Test the extract_unique_values function with the gapminder dataset
## Example 1: Extract unique continents from the gapminder dataset
unique_continents_gapminder <- extract_unique_values(gapminder, "continent")
print("Unique continents in gapminder dataset:")
```

    ## [1] "Unique continents in gapminder dataset:"

``` r
print(unique_continents_gapminder)
```

    ## [1] Asia     Europe   Africa   Americas Oceania 
    ## Levels: Africa Americas Asia Europe Oceania

``` r
## Example 2: Extract unique countries from the gapminder dataset
unique_countries_gapminder <- extract_unique_values(gapminder, "country")
print("Unique countries in gapminder dataset:")
```

    ## [1] "Unique countries in gapminder dataset:"

``` r
print(unique_countries_gapminder)
```

    ##   [1] Afghanistan              Albania                  Algeria                 
    ##   [4] Angola                   Argentina                Australia               
    ##   [7] Austria                  Bahrain                  Bangladesh              
    ##  [10] Belgium                  Benin                    Bolivia                 
    ##  [13] Bosnia and Herzegovina   Botswana                 Brazil                  
    ##  [16] Bulgaria                 Burkina Faso             Burundi                 
    ##  [19] Cambodia                 Cameroon                 Canada                  
    ##  [22] Central African Republic Chad                     Chile                   
    ##  [25] China                    Colombia                 Comoros                 
    ##  [28] Congo, Dem. Rep.         Congo, Rep.              Costa Rica              
    ##  [31] Cote d'Ivoire            Croatia                  Cuba                    
    ##  [34] Czech Republic           Denmark                  Djibouti                
    ##  [37] Dominican Republic       Ecuador                  Egypt                   
    ##  [40] El Salvador              Equatorial Guinea        Eritrea                 
    ##  [43] Ethiopia                 Finland                  France                  
    ##  [46] Gabon                    Gambia                   Germany                 
    ##  [49] Ghana                    Greece                   Guatemala               
    ##  [52] Guinea                   Guinea-Bissau            Haiti                   
    ##  [55] Honduras                 Hong Kong, China         Hungary                 
    ##  [58] Iceland                  India                    Indonesia               
    ##  [61] Iran                     Iraq                     Ireland                 
    ##  [64] Israel                   Italy                    Jamaica                 
    ##  [67] Japan                    Jordan                   Kenya                   
    ##  [70] Korea, Dem. Rep.         Korea, Rep.              Kuwait                  
    ##  [73] Lebanon                  Lesotho                  Liberia                 
    ##  [76] Libya                    Madagascar               Malawi                  
    ##  [79] Malaysia                 Mali                     Mauritania              
    ##  [82] Mauritius                Mexico                   Mongolia                
    ##  [85] Montenegro               Morocco                  Mozambique              
    ##  [88] Myanmar                  Namibia                  Nepal                   
    ##  [91] Netherlands              New Zealand              Nicaragua               
    ##  [94] Niger                    Nigeria                  Norway                  
    ##  [97] Oman                     Pakistan                 Panama                  
    ## [100] Paraguay                 Peru                     Philippines             
    ## [103] Poland                   Portugal                 Puerto Rico             
    ## [106] Reunion                  Romania                  Rwanda                  
    ## [109] Sao Tome and Principe    Saudi Arabia             Senegal                 
    ## [112] Serbia                   Sierra Leone             Singapore               
    ## [115] Slovak Republic          Slovenia                 Somalia                 
    ## [118] South Africa             Spain                    Sri Lanka               
    ## [121] Sudan                    Swaziland                Sweden                  
    ## [124] Switzerland              Syria                    Taiwan                  
    ## [127] Tanzania                 Thailand                 Togo                    
    ## [130] Trinidad and Tobago      Tunisia                  Turkey                  
    ## [133] Uganda                   United Kingdom           United States           
    ## [136] Uruguay                  Venezuela                Vietnam                 
    ## [139] West Bank and Gaza       Yemen, Rep.              Zambia                  
    ## [142] Zimbabwe                
    ## 142 Levels: Afghanistan Albania Algeria Angola Argentina Australia ... Zimbabwe

``` r
# Test the extract_unique_values function with the penguins dataset
## Example 3: Extract unique species from the penguins dataset
unique_species_penguins <- extract_unique_values(penguins, "species")
print("Unique species in penguins dataset:")
```

    ## [1] "Unique species in penguins dataset:"

``` r
print(unique_species_penguins)
```

    ## [1] Adelie    Gentoo    Chinstrap
    ## Levels: Adelie Chinstrap Gentoo

``` r
## Example 4: Extract unique islands from the penguins dataset
unique_islands_penguins <- extract_unique_values(penguins, "island")
print("Unique islands in penguins dataset:")
```

    ## [1] "Unique islands in penguins dataset:"

``` r
print(unique_islands_penguins)
```

    ## [1] Torgersen Biscoe    Dream    
    ## Levels: Biscoe Dream Torgersen

``` r
## Example 5: Try to extract unique values from a non-existent column
tryCatch({
  unique_errors <- extract_unique_values(gapminder, "non_existent_column")
  print(unique_errors)
}, error = function(e) {
  cat("Caught an error:\n")
  print(e$message)
})
```

    ## Caught an error:
    ## [1] "Column non_existent_column does not exist in the data frame."

#### Exercise 4: Test the Function

``` r
# Load the library
library(testthat)

# Test cases for extract_unique_values() function
test_that("Extract unique values function works as expected", {

  # 1. Test with a simple data frame containing unique values
  df1 <- data.frame(name = c("apple", "banana", "apple", "cherry"))
  expect_equal(extract_unique_values(df1, "name"), c("apple", "banana", "cherry"))

  # 2. Test with a data frame containing NA values
  df2 <- data.frame(category = c("A", "B", "A", NA, "C", NA))
  expect_equal(extract_unique_values(df2, "category"), c("A", "B", NA, "C"))

  # 3. Test with an empty data frame (column exists but is empty)
  df3 <- data.frame(number = numeric(0))
  expect_equal(extract_unique_values(df3, "number"), numeric(0))

  # 4. Test with a numeric column containing repeated values
  df4 <- data.frame(values = c(1, 1, 2, 3, 2, 4, 4, 4))
  expect_equal(extract_unique_values(df4, "values"), c(1, 2, 3, 4))

  # 5. Test with an invalid column name (should throw an error)
  expect_error(extract_unique_values(df4, "nonexistent_column"))

})
```

    ## Test passed 🎊
