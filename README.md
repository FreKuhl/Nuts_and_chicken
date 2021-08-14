# Nuts and chicken
by [Lukas Gürtler](https://github.com/LukasGuertler) & [Frederik Kuhl](https://github.com/FreKuhl)

This Decision analysis model for combining a nuttree meadow with chicken, was started in the ['Decision Analysis and Forecasting for Agricultural Development'](https://cory-whitney.shinyapps.io/Decision_Analysis_Course/) course by [Cory Whitney](https://cory-whitney.com/) & [Eike Lüdeling](https://eikeluedeling.com/index.html) at the University of Bonn in the Corona Summer of 2021. 

# The Idea
The idea behind our model, was to create an Agroforestry intervention for a small farmland plot in the Eifel region. The plot that is used for regular crops right now, is going to be used to sequestrate CO2. As the land use plan doesn't allow planting a regular forest we looked for different solutions to combine trees with different animals. The best solution for the owners [Woodify](woodify.de) was planting trees and keeping chicken in a chicken mobile house. So we came up with five different scenarios that have different levels of intensity. The way we calculated the different outcomes can be seen in our conceptual model below.

# Conceptual Model
![](Bilder_präsi/Conceptual_model.png)

# Methods
We used the package [decisionSupport](https://cran.r-project.org/web/packages/decisionSupport/index.html) by Eike Luedeling to implement our model in R. It helps to model decisions that are mainly agriculture related but can also be used for other complex systems, that would be hard to study in real life.
[tidyverse](https://www.tidyverse.org/) & [ggpubr](https://rpkgs.datanovia.com/ggpubr/) were used for formatting data and visualization.

# The Scenarios
1. 70 Hazelnut Trees + 200 Chicken
2. 200 Hazelnut Trees + 200 Chicken
3. 200 Truffle Trees + 200 Chicken
4. 70 Hazelnut Trees with Truffle + 200 Chicken
5. 200 Hazelnut Trees with Truffle + 200 Chicken

The scenarios contain different numbers of trees combined with chicken. For three scenarios we also added truffle as another way of generating revenue. As setting up a hazelnut plantation is cost and time intensive we also wanted to see if it would also be possible to set up this system without the plantation (Scenario 3), which would allow for better biodiversity through use of different tree varieties.

# Using the code
To get a better overview of our code check the __The_R_Markdown.html__ file in the project. It gives detailed overview about what we did and how our different scenarios were calculated. Values are taken from the __input_estimates.xlsx__ file. The values are no exact numbers but distributions with lower and upper bounds which the Monte Carlo Simulation uses to pick random values for each run. These values are then used for calculation and the results are displayed in distribution plots which can be easily compared to each other. 
To run the model open the __model_final.R__ file and load the libraries on the top. To visualize the results run the different functions in the __visualization_analysis.R__ file.
