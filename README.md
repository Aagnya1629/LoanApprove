# LoanApprove
This project aims to predict whether a loan applicant will be approved or denied, using a classification-based machine learning approach. We worked with a real-world dataset sourced from Kaggle to model loan approval outcomes based on applicant financial history, credit profile, and demographic features.

📊 Dataset:
Source: Kaggle - Financial Risk for Loan Approval Dataset

Records: 20,000
Attributes: 36
Target Variable: LoanApproved
1 = Approved
0 = Denied

💡 Why We Did It:
To explore how structured financial and demographic data can be used to improve credit risk modeling.
To evaluate the performance of multiple classification algorithms in the loan approval context.
To gain practical experience in data preprocessing, feature engineering, and model evaluation.

⚙️ What We Did:
Preprocessing:
Removed missing values, outliers (IQR method), and high-correlation features.
Cleaned categorical variables and used one-hot encoding.
Models Implemented:
Logistic Regression (with forward, backward, and stepwise selection)
k-Nearest Neighbors (kNN) with cross-validation and cutoff comparisons
CART (Classification and Regression Trees)
Random Forest, including tuned and default versions

Evaluation:
Accuracy, Sensitivity, Specificity at thresholds: 0.5, 0.3, 0.1
Variable importance and performance analysis
Comparison with existing literature

📈 Key Results:
Model	Accuracy (0.5 Cutoff)	Sensitivity	Specificity
Logistic Reg.	95.3%	90.8%	96.6%
kNN (Screened)	89.9%	73.4%	94.7%
CART	88.0%	67.5%	93.8%
Random Forest	91.6%	74.4%	96.4%

📁 Files Included:
File Name	Description
loan_prediction.R	Full R code for data preprocessing, modeling, and evaluation
loan_data.csv	Cleaned dataset used in the analysis
model_summary.txt	Text output of model evaluation metrics
correlation_matrix.png	Heatmap showing pairwise correlation for numeric variables
regression_tree.png	CART model visualization
Loan_Approval_Presentation.pdf	Final presentation summarizing the full workflow and results
README.md	Project overview and instructions
Presentation
