# Stock Price Movement Prediction from Financial News

A deep learning model that predicts whether a stock price will increase or decrease based on a financial news headline, using ALBERT, a lightweight transformer architecture developed by Google.

The more interesting result is not the 100% accuracy. It is what happens when you feed the model plain English it was never trained on. That is where things get revealing.

## Project Structure
```
project/
├── data/                        # dataset
├── notebooks/exploration.ipynb  # exploratory analysis
├── src/main.py                  # training script
├── report/                      # LaTeX report and figures
└── requirements.txt
```

## Model
- **Architecture:** ALBERT-base-v2 (11.6M parameters)
- **Task:** Binary classification - stock UP (1) or DOWN (0)
- **Framework:** PyTorch + HuggingFace Transformers

## Results
| Metric | Score |
|---|---|
| Accuracy | 100% |
| F1 Score | 100% |

The dataset is synthetic and perfectly balanced (800 UP / 800 DOWN), which explains the perfect scores. The model learned the templated language patterns in the data rather than genuine financial reasoning. Behavioral probing confirms this, feed it plain English like "Apple goes down" and it predicts UP with 99.8% confidence.

## How to Run
```bash
pip install -r requirements.txt
python src/main.py
```

## Author
Santiago Freile · April 2026