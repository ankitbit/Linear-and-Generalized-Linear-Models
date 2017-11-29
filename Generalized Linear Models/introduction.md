

### Transformations
* Binomial Data: Arc Sine Square Root Transformation- Destroys Interpretability of Model Coefficients. How?
* Logarithm Transformation

Basically, we depend on Generalized Linear Models because-
* We don't have to transform data to allow us to assume the conditions for the Linear Models. This lets the model stay on the scale on which the data was recorded; **Also important to remember is that *Natural Logarithm Tranformations* are not applicable to *Negative or Zero Values***
* We do not have to dishonor the real known assumptions about the data;

### About Generalized Linear Models
* Random Component: Exponential family- The response variable has to follow a distribution which should be expressed in the form of exponential family distributions
* Systematic Component: Linear Predictor- It is the part that we are trying to model. These are the same regression variables that we used for modelling in Linear Regression
* Link function: The role of the link function is to connect the means of the **Response Variable** to the **Linear Predictor**

about binomial and quasi binomial
