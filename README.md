# 🏆 Data Science & Machine Learning Competition Solutions

[![Python](https://img.shields.io/badge/Python-3.8+-blue.svg)](https://www.python.org/)
[![R](https://img.shields.io/badge/R-4.0+-blue.svg)](https://www.r-project.org/)
[![Jupyter](https://img.shields.io/badge/Jupyter-Notebook-orange.svg)](https://jupyter.org/)
[![Status](https://img.shields.io/badge/Status-Active-success.svg)]()

📬 Contact: Aleksey Shipitsyn [LinkedIn](https://www.linkedin.com/in/aleksey-shipitsyn-53417431)

Feel free to reach out if you have any questions about my methodologies or if you want to discuss potential collaborations!

---
Welcome to my repository of Data Science and Machine Learning competition solutions. This repository contains my code, exploratory data analysis (EDA), and modeling pipelines for various public competitions (e.g., Kaggle, DrivenData, Zindi) where I have consistently achieved **top-10% leaderboard placements**.

The solutions provided here are implemented using **Python**, **R**, and **Jupyter Notebooks**, focusing on clean code, reproducibility, and advanced feature engineering.



## 🗂️ Table of Contents / Solutions List

Below is a breakdown of the competitions, the problem statements, and the specific applied methods and ML models used to achieve the results.

### 1. Analytics Vidhya: Age Prediction
* **Path:** `/age.R`
* **Language:** R
* **Problem Statement:** Multi-class image classification to predict the age group (YOUNG, MIDDLE, OLD) of individuals based on image data.
* **Applied Methods:**
  * Handled class imbalance using a custom random undersampling function for majority classes.
  * Image preprocessing: loading, resizing to 28x28 pixels, and converting to grayscale using the `imager` library.
  * Image enhancement utilizing histogram equalization (ECDF).
  * Feature engineering: flattening 2D image matrices into 1D numerical feature vectors (784 features per image) for traditional ML algorithms.
* **ML Models:** Naive Bayes and Random Forest.

### 2. Analytics Vidhya: BigMart Sales Prediction
* **Path:** `/bigmart.R`
* **Language:** R
* **Problem Statement:** Predicting store item sales (Item_Outlet_Sales) based on specific product attributes and store (outlet) characteristics.
* **Applied Methods:**
  * Data cleaning and standardization of categorical levels (e.g., mapping inconsistent "low fat" labels to a standard format).
  * Missing value imputation utilizing grouping (imputing missing Item_Weight with the mean weight of the same Item_Identifier).
  * Feature engineering: Custom ordinal encoding for size categories and a custom function for creating one-hot encoded dummy variables.
  * Feature scaling and centering to prepare data for regularized regression models.
  * Model ensembling via simple averaging of predictions from multiple linear models to improve robustness.
* **ML Models:** Multiple Linear Regression, Ridge Regression, Lasso Regression, and Elastic Net (using the `glmnet` library).

### 3. Analytics Vidhya: Black Friday Sales Prediction
* **Path:** `/blackfriday.R`
* **Language:** R
* **Problem Statement:** Predicting the purchase amount of customers for various products during Black Friday sales based on customer demographics and product features.
* **Applied Methods:**
  * Data cleaning and missing value imputation (substituting NA values in secondary product categories with zeros).
  * Feature transformation: converting binary text variables (Gender) and ordinal strings (Age, Stay_In_Current_City_Years) into integer formats.
  * Explicit factor level alignment between training and testing datasets to handle unseen categories.
  * Feature engineering: One-hot encoding for multi-level categorical features (Occupation, City Category, Product Categories) using a custom `factor2dummy` function.
* **ML Models:** Multiple Linear Regression.

### 4. Analytics Vidhya: Digits Recognition
* **Path:** `/digits.R`
* **Language:** R
* **Problem Statement:** Multi-class image classification to recognize handwritten digits from 28x28 pixel images.
* **Applied Methods:**
  * Image preprocessing and channel splitting using the `imager` package.
  * Flattening 2D image matrices into 1D numerical feature vectors (784 features per image).
  * Dimensionality reduction by identifying and removing "empty" or near-zero variance background pixels across the training dataset.
  * Developed a custom pixel intensity thresholding function for optional image binarization.
* **ML Models:** Naive Bayes and Random Forest.

### 5. Analytics Vidhya: Recommender Engine
* **Path:** `/RecEng.R`
* **Language:** R
* **Problem Statement:** Predicting the number of attempts (attempts_range, a multi-class target) a user will need to successfully solve a given coding problem based on user demographics and problem metadata.
* **Applied Methods:**
  * Merged multiple relational datasets (user profiles, problem characteristics, and submission records).
  * Missing value imputation using median values for continuous variables and placeholder categories ('Unknown', 'U') for categorical ones.
  * Feature engineering: Converted timestamps into elapsed time features (time since registration and last login).
  * Text processing: Split comma-separated problem tags and transformed them into a binary document-term matrix (one-hot encoding).
  * Categorical encoding utilizing the `fastDummies` library.
  * Implemented a custom evaluation metric to calculate the Weighted F1-score, accounting for the highly skewed/imbalanced target classes.
* **ML Models:** Poisson Regression (GLM) and Random Forest.

### 6. SentiRuEval-2016
* **Path:** `/Senti.R`
* **Language:** R
* **Problem Statement:** Multiclass sentiment analysis of Russian tweets to classify customer feedback regarding banks and telecommunication companies into positive, negative, or neutral categories.
* **Applied Methods:**
  * Raw XML data parsing and extraction.
  * Natural Language Processing (NLP) pipeline using the `tm` package: text cleaning, Russian stopword removal, stemming, regex-based URL/English word filtering, and Bag-of-Words (Term-Document Matrix) creation.
  * Dimensionality reduction using Principal Component Analysis (PCA) to compress the sparse matrix, retaining 85-90% of the variance for faster model training.
  * Addressed severe class imbalance by implementing custom oversampling and undersampling functions.
  * Developed custom evaluation functions to compute multiclass Macro/Micro F1-scores and misclassification rates.
* **ML Models:** Support Vector Machine (SVM), Neural Networks, Linear Discriminant Analysis (LDA), Quadratic Discriminant Analysis (QDA), Logistic Regression, Decision Trees, k-Nearest Neighbors (kNN), Naive Bayes, Multiple Linear Regression, and a custom majority-voting Ensemble.

### 7. Analytics Vidhya: Urban Sound Classification
* **Path:** `/sound.R`
* **Language:** R
* **Problem Statement:** Multiclass classification of urban audio recordings (e.g., jackhammers, engine idling, sirens, dog barks) into distinct sound categories.
* **Applied Methods:**
  * Addressed class imbalance by implementing a custom random undersampling strategy for majority sound classes.
  * Audio file preprocessing utilizing `tuneR` and `seewave` packages: corrupted file handling, converting stereo tracks to mono, and uniform resampling to 44.1 kHz.
  * Audio length equalization: standardized all audio clips to exactly 2 seconds by truncating longer files and padding shorter files with silence.
  * Extracted Mel-Frequency Cepstral Coefficients (MFCCs) to represent acoustic features, flattening them into numerical feature matrices.
* **ML Models:** Naive Bayes and Random Forest.

### 8. Analytics Vidhya: Time Series Analysis
* **Path:** `/TScompetition.R`
* **Language:** R
* **Problem Statement:** Forecasting target metric counts (e.g., passenger traffic or event counts) over time using historical datetime data.
* **Applied Methods:**
  * Custom time-series feature engineering: developed a flexible function (`expand.dates`) to extract granular temporal components (year, month, day of week, day of year, week of year, hour, day time, and day parts) from raw POSIXlt datetime objects.
  * Categorical encoding: automated the creation of one-hot encoded variables (dummy matrices) for the newly extracted categorical time features using a custom `factor2dummy` function.
  * Trend and seasonality capturing through extensive temporal feature sets.
* **ML Models:** Poisson Regression (GLM with a log link function).

### 9. Analytics Vidhya: Footfall Parks Competition
* **Path:** `/StudentHunt/parks.R`
* **Language:** R
* **Problem Statement:** Predicting daily footfall (visitor traffic) in various park locations based on temporal features, local weather conditions, and environmental pollution metrics.
* **Applied Methods:**
  * Temporal feature engineering: Extracted granular features (year, month, day of week, day of year) from raw date strings.
  * Developed a sophisticated, two-step missing value imputation strategy:
    * *Conditional Imputation:* Filled missing values by randomly sampling from historical observations that matched the specific Location_Type within a rolling 20-day time window.
    * *Sequential Regression Imputation:* Iteratively trained multiple linear regression models to impute missing weather variables (e.g., Wind Direction, Breeze Speed, Atmospheric Pressure, Ambient Pollution) using previously complete or imputed variables as predictors.
  * Data standardization (scaling and centering) to prepare features for regularized regression models.
* **ML Models:** Multiple Linear Regression, Ridge Regression, Lasso Regression, and Random Forest.

### 10. Analytics Vidhya: Footfall Parks Competition (Advanced Feature Engineering)
* **Path:** `/StudentHunt/park2.R`
* **Language:** R
* **Problem Statement:** Predicting daily footfall (visitor traffic) in various park locations based on temporal, weather, and pollution data, leveraging advanced time-series lags and non-linear interactions.
* **Applied Methods:**
  * Temporal feature extraction (year, month, day of week, day of year).
  * Two-step missing value imputation: Conditional sampling based on a tighter 5-day rolling window, followed by sequential regression modeling.
  * Advanced time-series feature engineering: Developed a `features.past` function to generate lagged variables capturing historical weather and footfall data from the previous 5 days.
  * Polynomial & Interaction feature engineering: Created a `features.interact2` function to compute pairwise products and non-linear interactions across numeric features to capture complex environmental relationships.
  * Extensive feature standardization and scaling across the combined train and test sets to stabilize the expanded feature space.
* **ML Models:** Multiple Linear Regression, Ridge Regression, Lasso Regression (using cross-validation), and Random Forest.

### 11. Analytics Vidhya: Loan Prediction III
* **Path:** `/LoanPrediction/LoanPrediction.R`
* **Language:** R
* **Problem Statement:** Predicting whether a loan application will be approved (binary classification) based on applicant demographics, income details, and credit history.
* **Applied Methods:**
  * Handling missing values through a custom stochastic regression imputation method for continuous variables (Loan Amount, Loan Term) and sampling/median imputation for categorical variables.
  * Extensive domain-specific feature engineering: calculating Total Income, Net Income, Annuity payments, Income-to-Loan ratios, Annuity Coverage, and features normalized against sample averages/modes.
  * Data normalization via log transformations and magnitude scaling (e.g., dividing financial figures by 1,000 or 1,000,000).
  * Dimensionality reduction using Principal Component Analysis (PCA), retaining 13 principal components that explain 96% of the total variance.
  * 2D Data visualization using metric multidimensional scaling (MDS).
  * Addressed target class imbalance by developing custom `undersample`, `oversample`, and `equalsample` functions.
  * Built a custom evaluation pipeline to compute misclassification rates and compare multiple models alongside a majority-voting ensemble.
* **ML Models:** Logistic Regression, Neural Networks, Classification Trees, Random Forest, Support Vector Machine (SVM), k-Nearest Neighbors (kNN), Naive Bayes, Bayesian Additive Regression Trees (BART), Gaussian Process, and a Custom Voting Ensemble.

### 12. Analytics Vidhya: Loan Prediction III (Ensemble Submission)
* **Path:** `/LoanPrediction/LoanPredictionCompetition.R`
* **Language:** R
* **Problem Statement:** Predicting the final loan approval status (Yes/No) for applicants based on financial, demographic, and credit history data.
* **Applied Methods:**
  * Modularized data preprocessing pipeline: implemented stochastic regression imputation for continuous missing values and sampling/median strategies for categoricals.
  * Extensive domain-specific feature engineering: derived financial metrics such as Total Income, Net Income, Annuity payments, Annuity Coverage, and Income-to-Loan ratios, followed by log transformations and magnitude scaling.
  * Dimensionality reduction using Principal Component Analysis (PCA) to compress the feature space into 13 orthogonal components.
  * Implemented a robust repeated random sub-sampling strategy: looped the preprocessing and modeling pipeline 10 times, training on random 80% data slices in each iteration.
  * Grand Majority Voting Ensemble: Aggregated predictions from 7 distinct algorithm architectures across all 10 training iterations to compute highly robust final test set predictions.
* **ML Models:** Logistic Regression, Classification Trees, Random Forest, Support Vector Machine (SVM), Naive Bayes, Bayesian Additive Regression Trees (BART), Gaussian Process, and a Grand Majority Voting Ensemble.

### 13. CelebA Gender Prediction
* **Path:** `/celeba.py`
* **Language:** Python
* **Problem Statement:** Binary image classification to predict the gender (male/female) of individuals based on facial images from the CelebA dataset.
* **Applied Methods:**
  * Automated dataset downloading and extraction directly from Google Drive using `gdown` and `zipfile`.
  * Built an end-to-end data pipeline using `tf.data.Dataset` for efficient loading, batching, and shuffling of images.
  * Image preprocessing and dynamic data augmentation: applied random cropping, bounding-box cropping (for evaluation sets), random horizontal flipping, image resizing (to 64x64 pixels), and pixel intensity scaling.
  * Training evaluation and visualization: created custom plotting functions to monitor training versus validation loss and accuracy across epochs, as well as visualizing model predictions with probabilities on test images.
* **ML Models:** Convolutional Neural Network (CNN) built with TensorFlow and Keras (utilizing Conv2D, MaxPooling2D, Dropout, GlobalAveragePooling2D, and Dense layers).

### 14. Analytics Vidhya: WNS Analytics Hackathon 2018 (HR Analytics)
* **Path:** `/HR Analytics.ipynb`
* **Language:** Python (Jupyter Notebook)
* **Problem Statement:** Predicting employee promotion status (binary classification) based on employee demographics, past performance metrics, and general HR records.
* **Applied Methods:**
  * Handled missing data by introducing an 'Unknown' category for categorical features (education) and setting default median values for numerical ratings (previous_year_rating).
  * Combined training and test sets temporarily to ensure consistent categorical one-hot encoding (`pd.get_dummies`).
  * Feature standardization using Scikit-Learn's `StandardScaler`.
  * Addressed severe target class imbalance (~91% negative vs. 9% positive) using the `scale_pos_weight` parameter within the XGBoost algorithm.
  * Extensive hyperparameter tuning via `GridSearchCV` cross-validation, specifically optimizing for the F1-score metric to account for the imbalanced dataset.
  * *Result:* Achieved a top 1.6% rank (110th place out of 6,834 participants).
* **ML Models:** Logistic Regression and XGBoost.

### 15. Analytics Vidhya: Identify the Apparels
* **Path:** `/Identify apparels.ipynb`
* **Language:** Python (Jupyter Notebook)
* **Problem Statement:** Multiclass image classification to identify the type of apparel (10 classes: T-shirt, trousers, bag, etc.) from 28x28 grayscale images (Fashion MNIST dataset).
* **Applied Methods:**
  * Handled raw image reading and visual exploration using `imageio` and `matplotlib`.
  * Traditional Computer Vision feature engineering:
    * Extracted Histogram of Oriented Gradients (HOG) features to capture edge directions and shape structures.
    * Extracted Sobel filter features to detect image edges.
  * Extracted and flattened the HOG feature matrices into 1D tabular formats (81 numerical features per image) compatible with classical ML algorithms, rather than using Deep Learning/CNNs.
  * Hyperparameter tuning using Grid Search (`GridSearchCV`).
  * *Result:* Achieved an accuracy score yielding a top 8.8% rank (203rd out of 2,310 participants).
* **ML Models:** Gaussian Naive Bayes, Random Forest, and XGBoost (Random Forest variation `XGBRFClassifier`).

### 16. Analytics Vidhya: Identify the Sentiments
* **Path:** `/Identify the Sentiments.ipynb`
* **Language:** Python (Jupyter Notebook)
* **Problem Statement:** Binary sentiment classification of tweets, categorizing user feedback and posts into positive or negative sentiments (Linguipedia Codefest).
* **Applied Methods:**
  * Custom text feature engineering: Extracted meta-features including word/character counts, average word lengths, hashtag/numeric counts, and uppercase word counts.
  * Sentiment feature extraction: Leveraged TextBlob to calculate and append an out-of-the-box sentiment polarity score for each tweet.
  * Text preprocessing and cleaning: Regex-based extraction of English tokens (lowercasing), removal of `nltk` stopwords, text stemming via `PorterStemmer`, and the elimination of the top 10 most frequent domain-specific words.
  * Text vectorization: Converted the cleaned tweets into a sparse TF-IDF matrix using `TfidfVectorizer` spanning 1-to-3 n-grams.
  * Model stacking architecture: Generated base predictions using a TF-IDF text model and stacked them with the engineered meta-features (counts, TextBlob scores) to train a final meta-classifier.
  * Handled class imbalance organically using the `class_weight='balanced'` parameter.
  * *Result:* Achieved a top 9.3% rank (225th out of 2,424 participants) on the leaderboard.
* **ML Models:** Logistic Regression (utilized as both the base TF-IDF text classifier and the final meta-learner).

### 17. Analytics Vidhya: Twitter Sentiment Analysis
* **Path:** `/Twitter Sentiment Analysis.ipynb`
* **Language:** Python (Jupyter Notebook)
* **Problem Statement:** Binary text classification to predict the sentiment of tweets, specifically identifying negative sentiments/hate speech in a heavily imbalanced dataset (93% majority class).
* **Applied Methods:**
  * Engineered structural meta-features from raw text: word/character counts, average word length, and counts of stopwords, hashtags, numerics, and uppercase letters.
  * Extracted initial sentiment polarity scores using the TextBlob library.
  * Text preprocessing pipeline: Regex-based filtering (lowercasing), `nltk` stopword removal, stemming via `PorterStemmer`, and the explicit removal of the top 10 most frequent words in the corpus to reduce noise.
  * Text vectorization using `TfidfVectorizer` (unigrams) to transform cleaned text into a sparse feature matrix.
  * Model stacking architecture: Generated base sentiment predictions using a TF-IDF text model, and concatenated these predictions with the engineered meta-features to train a final meta-classifier.
  * Handled the severe target class imbalance organically by utilizing the `class_weight='balanced'` parameter.
  * *Result:* Achieved a top 3.95% rank (408th out of 10,321 participants) on the leaderboard.
* **ML Models:** Logistic Regression (utilized as both the base TF-IDF text classifier and the final stacked meta-learner).

### 18. Analytics Vidhya: Enigma Codefest - Predict Number of Upvotes
* **Path:** `/Number of upvotes.ipynb`
* **Language:** Python (Jupyter Notebook)
* **Problem Statement:** Predicting the number of upvotes a question will receive on a Q&A platform based on the author's reputation, number of answers, view counts, and question category tags.
* **Applied Methods:**
  * Custom log-normalization to handle highly skewed numerical distributions in both the target variable (Upvotes) and the predictors (Reputation, Answers, Views).
  * User-based feature engineering: Calculated the total number of historical posts per Username (N_posts) to capture individual user activity levels.
  * Generated high-degree Polynomial features (degree=5) using `PolynomialFeatures` to capture complex non-linear interactions among numerical variables.
  * Categorical one-hot encoding for anonymized question Tags.
  * Feature standardization using Scikit-Learn's `StandardScaler`.
  * *Result:* Achieved a top 17.1% rank (396th out of 2,320 participants).
* **ML Models:** Multiple Linear Regression, Lasso Regression, Ridge Regression (selected as the final model), and XGBoost (Random Forest Regressor variant `XGBRFRegressor`).
