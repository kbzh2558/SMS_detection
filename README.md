# 📱🛡️ Smishing Detection via Tree-Based NLP and Metadata Modeling  
![Python](https://img.shields.io/badge/Python-3.9+-blue?logo=python)  
![XGBoost](https://img.shields.io/badge/Model-XGBoost-orange)  
![NLP](https://img.shields.io/badge/NLP-Bag%20of%20Words-green)  

📊 Tree-Based Classification of Malicious SMS with Textual and Structural Cues  

> **Author**  
> Kaibo Zhang  
> **Affiliation**: McGill University  
> **Course**: MGSC401 - Final Project  

---

## 📌 Overview  

This project develops a binary classification model to detect **malicious SMS messages (smishing)** using natural language processing (NLP) and metadata. By combining **linguistic features** with behavioral and domain-based metadata, we show that a tuned **XGBoost** classifier can significantly outperform traditional models in predicting mobile phishing attempts.

**Key contributions include:**  
- Custom text cleaning and BoW token encoding  
- Timestamp and domain-based metadata engineering  
- Logistic regression baseline with diagnostic limitations  
- XGBoost modeling with extensive grid search and recall prioritization  
- Model interpretation and real-world threat detection insights  

---

## 🧾 Dataset Description  

The **Smishtank SMS Phishing Dataset** contains:
- 930 labeled SMS messages  
- Features include text content, sender type, URLs, domain metadata, and message timestamps  
- Labels: 0 (benign) vs. 1 (malicious)  

All messages were normalized, tokenized, and enriched with metadata such as WHOIS-based domain age and message timing.

---

## 🔍 Modeling Pipeline  

1. **Preprocessing**
   - Standard token replacements (`<url>`, `<email>`, etc.)
   - Hunspell-based spelling correction, stemming, stopword removal
   - Bag-of-Words representation chosen for interpretability

2. **Feature Engineering**
   - Domain creation/update times, sender types, and categorical brand grouping  
   - Timestamp decomposition (hour, minute, month)  
   - Handling missing metadata with structural padding (`-1`)

3. **Baseline Model**
   - Logistic regression showed poor recall and instability under sparsity

4. **Advanced Model: XGBoost**
   - Version 1: Balanced recall and precision with shallow trees (depth=3)  
   - Version 2: Prioritized recall but reduced precision  
   - Tuned using grid search on tree depth, learning rate, and column sampling  

5. **Evaluation Metrics**
   - Accuracy, Precision, Recall, F1, and AUC  
   - 5-fold cross-validation and test set comparison  

---

## 📊 Model Performance Summary  

| Model                    | Accuracy | Precision | Recall | F1 Score | AUC   |
|-------------------------|----------|-----------|--------|----------|-------|
| Logistic Regression     | 0.497    | 0.557     | 0.431  | 0.486    | ~0.56 |
| XGBoost (V1 - balanced) | 0.773    | 0.557     | 0.863  | 0.677    | 0.846 |
| XGBoost (V2 - recall)   | 0.751    | 0.519     | 0.837  | 0.641    | 0.824 |

---

## 📌 Key Insights and Managerial Implications  

- **New Domains = High Risk**: Recently registered domains are strong smishing indicators.  
- **Urgency Sells**: Words like “please,” “link,” and “confirm” dominate malicious texts.  
- **Sender Types Matter**: Phone numbers and email-to-text gateways are overused in smishing.  
- **False Positives Are Safer**: Models may overflag financial language but better than underflagging threats.  
- **Smishing is Polite**: Subtle smishing often uses kind tone and onboarding language — a blindspot for most models.  

---

## 📚 References  

[1] Timko, D., & Rahman, M. L. (2024). Smishing Dataset I: Smishtank.com Phishing Dataset. _ACM CODASPY_.  
https://www.smishtank.com  

---
