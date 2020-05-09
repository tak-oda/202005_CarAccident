# Prediction of severity in car accidents
	
In this project, I built a classification model to predict serious car accident in the U.S. To do this, I used “US Accidents A Countrywide Traffic Accident Dataset (2016 - 2019)” in Kaggle dataset. (https://www.kaggle.com/sobhanmoosavi/us-accidents) This dataset contains 2974335 observations about car accidents from February 2016 to December 2019 across entire U.S. This data sets give us severity of car accidents along with more than 40 variables such as location, weather, conditions.  
    Using this data set, I attempted to present answer to the question:  
    
How much would be the severity of the car accident when one car accident happens? This dataset provides response variable Severity i.e., a number between 1 and 4, where 1 indicates the least impact on traffic (short delay) and 4 indicates a significant impact on traffic. (long delay) I defined binary classification variable Severity_bin i.e., “Low” for Seveirty 1 and 2, “High” for Severity 3 and 4 and built a predictive model to estimate probability to have “High” severity against specific input of a street such as weather, time and road structure.  

I assume this model is used by public relation sector in a regional government (county government) which is in charge of notifying car accident in their web site or social media. Using the model, public relation agents will be able to provide as accurate estimated time for recovery as possible helping citizens take alternative way to the destination.   
    
To narrow the scope of the model, I extracted accident data of Illinois state in 2019 from original dataset.  To enrich predictive variable, I downloaded population data for each county in Illinois from https://www.illinois-demographics.com/counties_by_population and combined it with car accident data set.
