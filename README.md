# conversion-rate-prediction

I. Data:
- Given data is more than 1M interactions (eg.clicks, opened pages and searched keywords)  of customers to a website within a session
- The data is a nested table
- For modeling, 100,000 data points are extracted randomly.

II. Objectives
To study customer behavior from their entry points to a website
To study how differently keyword search influences customer’s purchase pattern
To compare the performance of Google paid campaign and Google organic search on traffic, revenue and conversion
To identify the most important variables contributing to session conversion
To provide the actionable insights and recommendations from our analysis

III. My appoach
1. EDA
- Understand data nature and factors related to conversion rate
2. Data cleaning
- Aggregate data
- Deal with data missing and wrong data type
3. Feature engineer and feature selection
- Address the problem of multicolinear, data skewness and reverse causality
- Create new features based on hints of EDA
4. Modeling
- The idea is that using neural network's performance as baseline for logistic's performance. 
If logistic's performance is equal to nn's, I would select logistic model. If it is not, try to train logistic model to match
nn's performance by adding interaction. Then if logistic's performance is still lower than nn's, I would choose nn.
5. Model evaluation
- Evaluate overfitting and model performance in train and test set
- Metrics: AUC, gain chart and confusion matrix
6. Interpretation and application

IV. Key takeaways
- Neural network easily overfits therefore you should carefully check overfitting and choose smaller note if it overfits 
(use ... chart to see optimal note)
- Be careful when selecting features. 
In my case, there is one feature that is a strong predictor but also a bad variable because of multicolinear. However,
if I remove that feature, its effect would be missing or absord in another features (for prediction, this problem would be fine. 
But for interpretation, it would be huge mistake since you could interpret wrongly other variables). My solution is that keeping that 
feature but modify it to avoid in the way that avoiding multicolinear.
- Watch out reverse casuality problem in your model.
- Yes, real data is messy and EDA would save your life. Never skip this step!
