# Image Classification with ResNet-18

A 10-class image classifier built with transfer learning on an intentionally imbalanced dataset. The project uses a pretrained ResNet-18 model with all layers frozen except the final classification layer, making training feasible on CPU in approximately 30 minutes.

The more interesting result is not the 97.35% overall accuracy. It is what the per-class analysis reveals: class imbalance does not hurt performance uniformly. Minority classes with visually distinctive features still reach 100% accuracy, while Gas Pump drops to 75% despite being larger than the smallest classes.

## Dataset

Imagenette-10, a 10-class subset of ImageNet. Modified to be intentionally imbalanced with 2,000 total images and an imbalance ratio of 25 (Tench: 500 images, Parachute: 20 images).

## Model

- Architecture: ResNet-18 pretrained on ImageNet
- Strategy: Feature extraction, all layers frozen except final
- Trainable parameters: 5,130 out of 11,181,642
- Framework: PyTorch + torchvision

## Results

| Metric | Score |
|---|---|
| Overall Accuracy | 97.35% |
| Overall F1 Score | 0.9733 |

## How to Run

```bash
python imagenette.py   # download and prepare dataset
python main.py         # train and evaluate
```

## Files

- `imagenette.py` — dataset download and preparation
- `src/main.py` — training and evaluation
- `report/report.pdf` — full technical report
- `report/report.tex` — LaTeX source

## Technologies

Python, PyTorch, torchvision, scikit-learn

*Santiago Freile · 2026*
