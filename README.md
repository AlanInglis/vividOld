<img src="https://github.com/AlanInglis/vivid/blob/master/badge/vividLogo.png" width="240" height="276" />


# Variable Interaction and Variable Importance Displays (vivid)

An R-package used to display variable importance and variable interactions. The vivid package contains a suite of plots that enable importance and interaction to be evaluated in a more efficient manor than would traditionally be possible.

An important tool for the use of analysis or for the exploration of data, is that of visualisations. The impact of a carefully chosen visualisation can be significant for a researcher, as using a meaningful visualisation can give emphasis to the relationships that variables have in a model, and thus serves to help the researcher gain a deeper understanding of the behaviour of a model. One such area that can benefit greatly from the use of informative visualisations is that of variable importance and variable interactions.

Traditional methods of displaying variable importance and interaction have relied heavily on the use of line, lollipop or bar charts. However, in variable importance plots there is no emphasis on displaying how interacting variables may also be important in a model. Similarly, in variable interaction plots there is no indication as to which is the most important variable in making a prediction. Both variable importance and variable interaction are clearly important tools in feature selection, yet traditional visualisation techniques deal with them separately. 

Here we present a new way to visualise the importance of variables and the interactions that exist between them, through the use of our R-package 'vivid' (variable importance and variable interaction displays). The vivid package allows a researcher to display a graphic that shows both the variable importance and the interaction strength between variables in an easily interpretable way. This has the added benefit of allowing researchers to identify variables, which may have a low value of importance, but interact strongly with other variables in the model. The package also contains an option to display the variables partial dependence. This can be of benefit to those who are conducting feature selection, or those who wish to gain a deeper insight into their model. 
