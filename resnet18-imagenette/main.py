"""
Image Classification with ResNet-18 on an Imbalanced Dataset
=============================================================
Model:   ResNet-18 (pretrained on ImageNet, feature extraction)
Task:    10-class image classification on imbalanced Imagenette
Dataset: Imagenette-10 (2,000 images, intentionally imbalanced)

Author:  Santiago Freile
Date:    April 2026

Required packages:
    pip install torch torchvision matplotlib scikit-learn pillow
"""

import torch
import torch.nn as nn
import torch.optim as optim
from torchvision import datasets, transforms, models
from torch.utils.data import DataLoader
import matplotlib.pyplot as plt
from sklearn.metrics import f1_score

# ============================================================
# 1. DEVICE
# ============================================================

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
print("Using device:", device)

# ============================================================
# 2. TRANSFORMS
# ============================================================

# Training set: resize, random horizontal flip, normalize
train_transform = transforms.Compose([
    transforms.Resize((224, 224)),
    transforms.RandomHorizontalFlip(),
    transforms.ToTensor(),
    transforms.Normalize(
        mean=[0.485, 0.456, 0.406],
        std=[0.229, 0.224, 0.225]
    )
])

# Validation and test sets: no augmentation
val_test_transform = transforms.Compose([
    transforms.Resize((224, 224)),
    transforms.ToTensor(),
    transforms.Normalize(
        mean=[0.485, 0.456, 0.406],
        std=[0.229, 0.224, 0.225]
    )
])

# ============================================================
# 3. DATASETS AND DATALOADERS
# ============================================================

train_dataset = datasets.ImageFolder("../data/imagenette10_imbalanced/train", transform=train_transform)
val_dataset   = datasets.ImageFolder("../data/imagenette10_imbalanced/val",   transform=val_test_transform)
test_dataset  = datasets.ImageFolder("../data/imagenette10_imbalanced/test",  transform=val_test_transform)

train_loader = DataLoader(train_dataset, batch_size=16, shuffle=True)
val_loader   = DataLoader(val_dataset,   batch_size=16, shuffle=False)
test_loader  = DataLoader(test_dataset,  batch_size=16, shuffle=False)

images, labels = next(iter(train_loader))
print(f"Image batch shape: {images.shape}")
print(f"Label batch shape: {labels.shape}")

# ============================================================
# 4. DATASET EXPLORATION
# ============================================================

class_names = train_dataset.classes
print("Classes:", class_names)

print("\nImage count per class:")
print(f"{'Class':20s} {'Train':>8s} {'Val':>8s} {'Test':>8s}")
print("-" * 50)
for i, class_name in enumerate(class_names):
    train_count = sum(1 for _, label in train_dataset.samples if label == i)
    val_count   = sum(1 for _, label in val_dataset.samples   if label == i)
    test_count  = sum(1 for _, label in test_dataset.samples  if label == i)
    print(f"{class_name:20s} {train_count:>8d} {val_count:>8d} {test_count:>8d}")

print("-" * 50)
print(f"{'TOTAL':20s} {len(train_dataset):>8d} {len(val_dataset):>8d} {len(test_dataset):>8d}")

train_counts    = [sum(1 for _, label in train_dataset.samples if label == i) for i in range(len(class_names))]
imbalance_ratio = max(train_counts) / min(train_counts)
print(f"\nImbalance ratio: {imbalance_ratio:.1f}x")

# Sample image per class
fig, axes = plt.subplots(2, 5, figsize=(15, 6))
axes = axes.flatten()

for class_idx, class_name in enumerate(class_names):
    for img_path, label in train_dataset.samples:
        if label == class_idx:
            img = plt.imread(img_path)
            axes[class_idx].imshow(img)
            axes[class_idx].set_title(class_name, fontsize=9)
            axes[class_idx].axis("off")
            break

plt.suptitle("Sample image from each class", fontsize=13)
plt.tight_layout()
plt.savefig("sample_images.png", dpi=150, bbox_inches="tight")
plt.show()

# ============================================================
# 5. MODEL
# ============================================================

model = models.resnet18(pretrained=True)

# Freeze all pretrained layers
for param in model.parameters():
    param.requires_grad = False

# Replace the final layer for 10-class classification
model.fc = nn.Linear(model.fc.in_features, 10)

model = model.to(device)

print("Final layer:", model.fc)

trainable_params = sum(p.numel() for p in model.parameters() if p.requires_grad)
total_params     = sum(p.numel() for p in model.parameters())
print(f"Trainable parameters: {trainable_params:,}")
print(f"Total parameters:     {total_params:,}")

# ============================================================
# 6. LOSS AND OPTIMIZER
# ============================================================

criterion = nn.CrossEntropyLoss()
optimizer = optim.Adam(model.parameters(), lr=0.001)

# ============================================================
# 7. TRAINING
# ============================================================

epochs = 5

train_losses     = []
train_accuracies = []
train_f1s        = []
val_losses       = []
val_accuracies   = []
val_f1s          = []

for epoch in range(epochs):

    # Training phase
    model.train()
    epoch_loss = 0.0
    correct    = 0
    total      = 0
    all_preds  = []
    all_labels = []

    for images, labels in train_loader:
        images = images.to(device)
        labels = labels.to(device)

        outputs = model(images)
        loss    = criterion(outputs, labels)

        optimizer.zero_grad()
        loss.backward()
        optimizer.step()

        epoch_loss += loss.item() * images.size(0)

        _, predicted = torch.max(outputs, 1)
        correct      += (predicted == labels).sum().item()
        total        += labels.size(0)

        all_preds.extend(predicted.cpu().numpy())
        all_labels.extend(labels.cpu().numpy())

    train_loss = epoch_loss / total
    train_acc  = correct / total * 100
    train_f1   = f1_score(all_labels, all_preds, average="weighted")

    train_losses.append(train_loss)
    train_accuracies.append(train_acc)
    train_f1s.append(train_f1)

    # Validation phase
    model.eval()
    val_loss   = 0.0
    correct    = 0
    total      = 0
    all_preds  = []
    all_labels = []

    with torch.no_grad():
        for images, labels in val_loader:
            images = images.to(device)
            labels = labels.to(device)

            outputs = model(images)
            loss    = criterion(outputs, labels)

            val_loss += loss.item() * images.size(0)

            _, predicted = torch.max(outputs, 1)
            correct      += (predicted == labels).sum().item()
            total        += labels.size(0)

            all_preds.extend(predicted.cpu().numpy())
            all_labels.extend(labels.cpu().numpy())

    val_loss_avg = val_loss / total
    val_acc      = correct / total * 100
    val_f1       = f1_score(all_labels, all_preds, average="weighted")

    val_losses.append(val_loss_avg)
    val_accuracies.append(val_acc)
    val_f1s.append(val_f1)

    print(f"Epoch [{epoch+1}/{epochs}]  "
          f"Train Loss: {train_loss:.4f}  Train Acc: {train_acc:.2f}%  Train F1: {train_f1:.4f}  |  "
          f"Val Loss: {val_loss_avg:.4f}  Val Acc: {val_acc:.2f}%  Val F1: {val_f1:.4f}")

# Learning curves
fig, axes = plt.subplots(1, 3, figsize=(15, 4))
epoch_range = range(1, epochs + 1)

axes[0].plot(epoch_range, train_losses,     label="Train")
axes[0].plot(epoch_range, val_losses,       label="Validation")
axes[0].set_title("Loss over epochs")
axes[0].set_xlabel("Epoch")
axes[0].set_ylabel("Loss")
axes[0].legend()

axes[1].plot(epoch_range, train_accuracies, label="Train")
axes[1].plot(epoch_range, val_accuracies,   label="Validation")
axes[1].set_title("Accuracy over epochs")
axes[1].set_xlabel("Epoch")
axes[1].set_ylabel("Accuracy (%)")
axes[1].legend()

axes[2].plot(epoch_range, train_f1s,        label="Train")
axes[2].plot(epoch_range, val_f1s,          label="Validation")
axes[2].set_title("F1 Score over epochs")
axes[2].set_xlabel("Epoch")
axes[2].set_ylabel("F1 Score")
axes[2].legend()

plt.suptitle("Learning curves", fontsize=13)
plt.tight_layout()
plt.savefig("learning_curves.png", dpi=150, bbox_inches="tight")
plt.show()

# ============================================================
# 8. EVALUATION
# ============================================================

model.eval()

all_preds  = []
all_labels = []
all_images = []

with torch.no_grad():
    for images, labels in test_loader:
        images = images.to(device)
        labels = labels.to(device)

        outputs = model(images)
        _, predicted = torch.max(outputs, 1)

        all_preds.extend(predicted.cpu().numpy())
        all_labels.extend(labels.cpu().numpy())
        all_images.extend(images.cpu())

test_acc = sum(p == l for p, l in zip(all_preds, all_labels)) / len(all_labels) * 100
test_f1  = f1_score(all_labels, all_preds, average="weighted")
print(f"Overall Test Accuracy: {test_acc:.2f}%")
print(f"Overall Test F1 Score: {test_f1:.4f}")

print("\nPer-class results:")
print(f"{'Class':20s} {'Accuracy':>10s} {'F1 Score':>10s}")
print("-" * 45)

per_class_f1 = f1_score(all_labels, all_preds, average=None)

for i, class_name in enumerate(class_names):
    class_correct = sum(p == l for p, l in zip(all_preds, all_labels) if l == i)
    class_total   = sum(1 for l in all_labels if l == i)
    class_acc     = class_correct / class_total * 100
    print(f"{class_name:20s} {class_acc:>9.2f}%  {per_class_f1[i]:>10.4f}")

# Correctly classified examples
correct_indices = [i for i, (p, l) in enumerate(zip(all_preds, all_labels)) if p == l][:5]

fig, axes = plt.subplots(1, 5, figsize=(15, 3))
for ax, idx in zip(axes, correct_indices):
    img = all_images[idx].permute(1, 2, 0).numpy()
    img = img * [0.229, 0.224, 0.225] + [0.485, 0.456, 0.406]
    img = img.clip(0, 1)
    ax.imshow(img)
    ax.set_title(f"T: {class_names[all_labels[idx]]}\nP: {class_names[all_preds[idx]]}", fontsize=8)
    ax.axis("off")
plt.suptitle("Correctly classified", fontsize=13)
plt.tight_layout()
plt.savefig("correct_examples.png", dpi=150, bbox_inches="tight")
plt.show()

# Incorrectly classified examples
wrong_indices = [i for i, (p, l) in enumerate(zip(all_preds, all_labels)) if p != l][:5]

fig, axes = plt.subplots(1, 5, figsize=(15, 3))
for ax, idx in zip(axes, wrong_indices):
    img = all_images[idx].permute(1, 2, 0).numpy()
    img = img * [0.229, 0.224, 0.225] + [0.485, 0.456, 0.406]
    img = img.clip(0, 1)
    ax.imshow(img)
    ax.set_title(f"T: {class_names[all_labels[idx]]}\nP: {class_names[all_preds[idx]]}", fontsize=8)
    ax.axis("off")
plt.suptitle("Incorrectly classified", fontsize=13)
plt.tight_layout()
plt.savefig("incorrect_examples.png", dpi=150, bbox_inches="tight")
plt.show()