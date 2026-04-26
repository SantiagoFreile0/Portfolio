"""
Stock Price Movement Prediction from Financial News Headlines
=============================================================
Model:   ALBERT (A Lite BERT) - albert-base-v2
Task:    Binary Classification (UP = 1, DOWN = 0)
Dataset: financial_news_albert.csv

Author:  Santiago Freile
Course:  DATA 445 - Machine Learning
Date:    April 2026

Required packages:
    pip install pandas numpy scikit-learn torch transformers matplotlib seaborn
"""

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import warnings
warnings.filterwarnings("ignore")

from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score, f1_score, classification_report

import torch
from torch.utils.data import Dataset, DataLoader
from torch.optim import AdamW

from transformers import (
    AlbertTokenizer,
    AlbertForSequenceClassification,
)

# ============================================================
# 1. CONFIG
# ============================================================

DATA_PATH    = "../data/financial_news_albert.csv"
MODEL_NAME   = "albert-base-v2"
RANDOM_STATE = 42
MAX_LEN      = 32
BATCH_SIZE   = 16
EPOCHS       = 3
LR           = 2e-5

DEVICE = torch.device("cuda" if torch.cuda.is_available() else "cpu")
print("Using device:", DEVICE)

# ============================================================
# 2. LOAD DATA
# ============================================================

df = pd.read_csv(DATA_PATH)
df = df.dropna(subset=["title", "label"]).copy()

print("\nFirst 5 rows:")
print(df.head())

print("\nClass distribution:")
print(df["label"].value_counts())

print(f"\nTotal samples: {len(df)}")
print(f"Balance check:\n{df['label'].value_counts(normalize=True).round(3)}")

# ============================================================
# 3. DATA SPLITTING
# ============================================================

# Step 1: split off test set (15%)
train_val_df, test_df = train_test_split(
    df,
    test_size=0.15,
    stratify=df["label"],
    random_state=RANDOM_STATE
)

# Step 2: split remaining 85% into train (70%) and val (15%)
train_df, val_df = train_test_split(
    train_val_df,
    test_size=0.15 / 0.85,   # 15% of total = 17.6% of remaining 85%
    stratify=train_val_df["label"],
    random_state=RANDOM_STATE
)

print(f"\nTraining set:   {len(train_df)} samples ({len(train_df)/len(df)*100:.1f}%)")
print(f"Validation set: {len(val_df)} samples ({len(val_df)/len(df)*100:.1f}%)")
print(f"Test set:       {len(test_df)} samples ({len(test_df)/len(df)*100:.1f}%)")

# ============================================================
# 4. TOKENIZATION
# ============================================================

tokenizer = AlbertTokenizer.from_pretrained(MODEL_NAME)

def tokenize_data(texts, tokenizer, max_length=MAX_LEN):
    return tokenizer(
        texts,
        max_length=max_length,
        padding="max_length",
        truncation=True,
        return_tensors="pt"
    )

train_encodings = tokenize_data(train_df["title"].tolist(), tokenizer)
val_encodings   = tokenize_data(val_df["title"].tolist(),   tokenizer)
test_encodings  = tokenize_data(test_df["title"].tolist(),  tokenizer)

print(f"\nTrain encodings shape: {train_encodings['input_ids'].shape}")
print(f"Val encodings shape:   {val_encodings['input_ids'].shape}")
print(f"Test encodings shape:  {test_encodings['input_ids'].shape}")

# ============================================================
# 5. DATASET CLASS
# ============================================================

class StockDataset(Dataset):
    def __init__(self, encodings, labels):
        self.encodings = encodings
        self.labels    = labels

    def __len__(self):
        return len(self.labels)

    def __getitem__(self, idx):
        return {
            "input_ids":      self.encodings["input_ids"][idx],
            "attention_mask": self.encodings["attention_mask"][idx],
            "label":          torch.tensor(self.labels[idx], dtype=torch.long)
        }

train_dataset = StockDataset(train_encodings, train_df["label"].tolist())
val_dataset   = StockDataset(val_encodings,   val_df["label"].tolist())
test_dataset  = StockDataset(test_encodings,  test_df["label"].tolist())

train_loader = DataLoader(train_dataset, batch_size=BATCH_SIZE, shuffle=True)
val_loader   = DataLoader(val_dataset,   batch_size=BATCH_SIZE, shuffle=False)
test_loader  = DataLoader(test_dataset,  batch_size=BATCH_SIZE, shuffle=False)

print(f"\nTrain dataset: {len(train_dataset)} samples")
print(f"Val dataset:   {len(val_dataset)} samples")
print(f"Test dataset:  {len(test_dataset)} samples")

# ============================================================
# 6. MODEL
# ============================================================

model = AlbertForSequenceClassification.from_pretrained(
    MODEL_NAME,
    num_labels=2   # binary: UP (1) or DOWN (0)
)
model = model.to(DEVICE)

total_params     = sum(p.numel() for p in model.parameters())
trainable_params = sum(p.numel() for p in model.parameters() if p.requires_grad)

print(f"\nTotal parameters:     {total_params:,}")
print(f"Trainable parameters: {trainable_params:,}")

# ============================================================
# 7. TRAINING
# ============================================================

optimizer = AdamW(model.parameters(), lr=LR)
criterion = torch.nn.CrossEntropyLoss()

history = {
    "train_loss": [], "val_loss": [],
    "train_acc":  [], "val_acc":  [],
    "train_f1":   [], "val_f1":   []
}

print("\nTraining ALBERT...")
print(f"Epochs: {EPOCHS}  |  LR: {LR}  |  Batch size: {BATCH_SIZE}\n")

for epoch in range(EPOCHS):

    # ── TRAINING PHASE ────────────────────────────────────────
    model.train()
    total_train_loss = 0
    all_preds, all_labels = [], []

    for batch in train_loader:
        input_ids      = batch["input_ids"].to(DEVICE)
        attention_mask = batch["attention_mask"].to(DEVICE)
        labels         = batch["label"].to(DEVICE)

        optimizer.zero_grad()
        outputs = model(input_ids=input_ids, attention_mask=attention_mask)
        loss    = criterion(outputs.logits, labels)
        loss.backward()
        optimizer.step()

        total_train_loss += loss.item()
        preds = torch.argmax(outputs.logits, dim=1)
        all_preds.extend(preds.cpu().numpy())
        all_labels.extend(labels.cpu().numpy())

    train_loss = total_train_loss / len(train_loader)
    train_acc  = accuracy_score(all_labels, all_preds)
    train_f1   = f1_score(all_labels, all_preds, average="weighted")

    # ── VALIDATION PHASE ──────────────────────────────────────
    model.eval()
    total_val_loss = 0
    val_preds, val_labels_list = [], []

    with torch.no_grad():
        for batch in val_loader:
            input_ids      = batch["input_ids"].to(DEVICE)
            attention_mask = batch["attention_mask"].to(DEVICE)
            labels         = batch["label"].to(DEVICE)

            outputs = model(input_ids=input_ids, attention_mask=attention_mask)
            loss    = criterion(outputs.logits, labels)

            total_val_loss += loss.item()
            preds = torch.argmax(outputs.logits, dim=1)
            val_preds.extend(preds.cpu().numpy())
            val_labels_list.extend(labels.cpu().numpy())

    val_loss = total_val_loss / len(val_loader)
    val_acc  = accuracy_score(val_labels_list, val_preds)
    val_f1   = f1_score(val_labels_list, val_preds, average="weighted")

    history["train_loss"].append(train_loss)
    history["val_loss"].append(val_loss)
    history["train_acc"].append(train_acc)
    history["val_acc"].append(val_acc)
    history["train_f1"].append(train_f1)
    history["val_f1"].append(val_f1)

    print(f"Epoch {epoch+1}/{EPOCHS}")
    print(f"  Train  →  Loss: {train_loss:.4f}  Acc: {train_acc:.4f}  F1: {train_f1:.4f}")
    print(f"  Val    →  Loss: {val_loss:.4f}  Acc: {val_acc:.4f}  F1: {val_f1:.4f}\n")

print("Training complete.")

# ============================================================
# 8. EVALUATION
# ============================================================

model.eval()
test_preds, test_labels_list = [], []

with torch.no_grad():
    for batch in test_loader:
        input_ids      = batch["input_ids"].to(DEVICE)
        attention_mask = batch["attention_mask"].to(DEVICE)
        labels         = batch["label"].to(DEVICE)

        outputs = model(input_ids=input_ids, attention_mask=attention_mask)
        preds   = torch.argmax(outputs.logits, dim=1)
        test_preds.extend(preds.cpu().numpy())
        test_labels_list.extend(labels.cpu().numpy())

y_pred = np.array(test_preds)
y_true = np.array(test_labels_list)

print("\nALBERT Classification Report:")
print(classification_report(
    y_true, y_pred,
    target_names=["Stock DOWN", "Stock UP"],
    digits=4
))

test_acc = accuracy_score(y_true, y_pred)
test_f1  = f1_score(y_true, y_pred, average="weighted")

print("================ FINAL RESULTS ================")
print(f"Accuracy: {test_acc:.4f}")
print(f"F1 Score: {test_f1:.4f}")

# ============================================================
# 9. PREDICT NEW HEADLINES
# ============================================================

def predict(headline):
    model.eval()

    encoding = tokenizer(
        headline,
        max_length=MAX_LEN,
        padding="max_length",
        truncation=True,
        return_tensors="pt"
    )

    input_ids      = encoding["input_ids"].to(DEVICE)
    attention_mask = encoding["attention_mask"].to(DEVICE)

    with torch.no_grad():
        outputs = model(input_ids=input_ids, attention_mask=attention_mask)
        probs   = torch.softmax(outputs.logits, dim=1)[0]
        pred    = torch.argmax(probs).item()

    label      = "UP ↑" if pred == 1 else "DOWN ↓"
    confidence = probs[pred].item()

    print(f"Headline:   {headline}")
    print(f"Prediction: {label}  (confidence: {confidence:.1%})")
    print()

sample_headlines = [
    "Google improves in the last years",
    "Pfizer decrease their performance",
    "AMD starts asking what is happening",
    "Microsoft increase their performance",
    "Apple's earnings go down",
    "Meta has a massive layoff"
]

print("\nSample predictions:")
for headline in sample_headlines:
    predict(headline)