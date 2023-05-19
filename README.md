# shiny-regression
Shiny Dashboard application for the regression analysis.
User is asked to insert a .csv file (<5Mb), and choose a targed variable and it's predictos(s).

![image](https://github.com/Taniaosdch/shiny-regression/assets/67751914/2255389b-7600-414f-b2d6-7b3062aa4586)

The application supports linear, logistic, ridge and lasso regression. 
The second step is to chose a % of train data, and a unique set.seed() number for the random sampling. 

![image](https://github.com/Taniaosdch/shiny-regression/assets/67751914/3a2d9498-dce0-4397-9e99-c8ab8f33a7af)

Below is a web capture of a Data Description part, using the AirQuality dataset (available via: <https://archive.ics.uci.edu/ml/datasets/air+quality>)

![Web capture_19-5-2023_191832_127 0 0 1](https://github.com/Taniaosdch/shiny-regression/assets/67751914/533d4490-ee7e-4d64-af50-e8886fd4a29c)

The Linear regression part is presented at the web capture below. The application allows to test linear regession assumptions, reveal outliers and influencial values and provides a feature selection.
Using a train/test datasets it calculates validation metrics.

![Web capture_19-5-2023_192143_127 0 0 1](https://github.com/Taniaosdch/shiny-regression/assets/67751914/bf787368-8458-4478-a69e-62467b0efca5)

Ridge and Lasso part is below. (click on the Menu to choose the regression type)

![Web capture_19-5-2023_192454_127 0 0 1](https://github.com/Taniaosdch/shiny-regression/assets/67751914/d61a4f43-f4c0-4013-a892-262d2edaf58f)

The logistic regression part in R is done using a glm() function. Another dataset (<https://archive-beta.ics.uci.edu/dataset/222/bank+marketing>) with a binary target variable is used for this example.

![Web capture_19-5-2023_19316_127 0 0 1](https://github.com/Taniaosdch/shiny-regression/assets/67751914/c8958653-31c4-4057-b4f2-8fcc3e4b81ac)
