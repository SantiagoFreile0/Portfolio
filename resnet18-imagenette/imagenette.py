"""
download_prepare_imagenette_project.py

What this script does:
1. Downloads Imagenette 160px.
2. Extracts the dataset.
3. Creates an imbalanced 10-class dataset with 2,000 total images.
4. Splits images into train, validation, and test folders.
5. Resizes all images to 224 x 224.
6. Saves the final dataset in PyTorch ImageFolder format.

Final output folder:

imagenette10_imbalanced/
    train/
        tench/
        English_springer/
        cassette_player/
        chain_saw/
        church/
        French_horn/
        garbage_truck/
        gas_pump/
        golf_ball/
        parachute/
    val/
        ...
    test/
        ...

After running this script, you can use ImageFolder to load the dataset.
"""

import random
import tarfile
import shutil
import urllib.request
from pathlib import Path
from PIL import Image


# ---------------------------------------------------------
# 1. Settings
# ---------------------------------------------------------

SEED = 42
random.seed(SEED)

# Imagenette 160px public download URL.
# This smaller version is recommended for CPU-only student projects.
DATA_URL = "https://s3.amazonaws.com/fast-ai-imageclas/imagenette2-160.tgz"

BASE_DIR = Path("data")
RAW_TGZ = BASE_DIR / "imagenette2-160.tgz"
RAW_DIR = BASE_DIR / "imagenette2-160"

OUTPUT_DIR = Path("imagenette10_imbalanced")

IMAGE_SIZE = (224, 224)

# Original Imagenette folder names are WordNet IDs.
# We rename them to readable class names.
CLASS_MAP = {
    "n01440764": "tench",
    "n02102040": "English_springer",
    "n02979186": "cassette_player",
    "n03000684": "chain_saw",
    "n03028079": "church",
    "n03394916": "French_horn",
    "n03417042": "garbage_truck",
    "n03425413": "gas_pump",
    "n03445777": "golf_ball",
    "n03888257": "parachute",
}

# Imbalanced distribution.
# Total number of images = 2,000.
CLASS_COUNTS = {
    "tench": 500,
    "English_springer": 400,
    "cassette_player": 300,
    "chain_saw": 250,
    "church": 200,
    "French_horn": 150,
    "garbage_truck": 100,
    "gas_pump": 50,
    "golf_ball": 30,
    "parachute": 20,
}

TRAIN_RATIO = 0.70
VAL_RATIO = 0.15


# ---------------------------------------------------------
# 2. Helper functions
# ---------------------------------------------------------

def download_file(url: str, output_path: Path):
    """
    Download the Imagenette archive if it does not already exist.
    """
    output_path.parent.mkdir(parents=True, exist_ok=True)

    if output_path.exists():
        print(f"Dataset archive already exists: {output_path}")
        return

    print("Downloading Imagenette dataset...")
    print("This may take a few minutes depending on your internet speed.")
    print(f"URL: {url}")

    urllib.request.urlretrieve(url, output_path)

    print(f"Download complete: {output_path}")


def extract_tgz(tgz_path: Path, extract_to: Path):
    """
    Extract the .tgz file if it has not already been extracted.
    """
    if extract_to.exists():
        print(f"Raw dataset already extracted: {extract_to}")
        return

    print(f"Extracting {tgz_path}...")

    with tarfile.open(tgz_path, "r:gz") as tar:
        tar.extractall(path=extract_to.parent)

    print(f"Extraction complete: {extract_to}")


def collect_images_for_class(raw_dir: Path, class_id: str):
    """
    Collect image paths from both original train and val folders.

    Original Imagenette structure:
        imagenette2-160/
            train/
                n01440764/
                ...
            val/
                n01440764/
                ...
    """
    image_paths = []

    for split in ["train", "val"]:
        class_folder = raw_dir / split / class_id

        if not class_folder.exists():
            raise FileNotFoundError(f"Missing class folder: {class_folder}")

        for ext in ["*.jpg", "*.jpeg", "*.png", "*.JPEG", "*.JPG"]:
            image_paths.extend(class_folder.glob(ext))

    return image_paths


def split_images(image_paths):
    """
    Shuffle and split images into train, validation, and test sets.
    """
    image_paths = list(image_paths)
    random.shuffle(image_paths)

    n = len(image_paths)

    n_train = int(n * TRAIN_RATIO)
    n_val = int(n * VAL_RATIO)

    train_images = image_paths[:n_train]
    val_images = image_paths[n_train:n_train + n_val]
    test_images = image_paths[n_train + n_val:]

    return train_images, val_images, test_images


def resize_and_save_image(src_path: Path, dst_path: Path):
    """
    Open image, convert to RGB, resize to 224 x 224, and save.
    """
    dst_path.parent.mkdir(parents=True, exist_ok=True)

    try:
        with Image.open(src_path) as img:
            img = img.convert("RGB")
            img = img.resize(IMAGE_SIZE)
            img.save(dst_path, quality=95)
    except Exception as error:
        print(f"Skipping image because it could not be processed: {src_path}")
        print(error)


def prepare_output_folder(output_dir: Path):
    """
    Create a clean output folder.
    If the output folder already exists, ask students to remove it manually.
    This prevents accidental overwriting.
    """
    if output_dir.exists():
        print()
        print(f"The output folder already exists: {output_dir}")
        print("To recreate the dataset, delete this folder first and run the script again.")
        print("The script will stop now to avoid overwriting your files.")
        raise SystemExit

    for split in ["train", "val", "test"]:
        for class_name in CLASS_COUNTS:
            (output_dir / split / class_name).mkdir(parents=True, exist_ok=True)


def count_images(folder: Path):
    """
    Count image files in a folder.
    """
    total = 0
    for ext in ["*.jpg", "*.jpeg", "*.png", "*.JPEG", "*.JPG"]:
        total += len(list(folder.glob(ext)))
    return total


# ---------------------------------------------------------
# 3. Main function
# ---------------------------------------------------------

def main():
    print("=" * 72)
    print("Download and Prepare Imbalanced Imagenette-10 Dataset")
    print("=" * 72)

    # Step 1: Download Imagenette.
    download_file(DATA_URL, RAW_TGZ)

    # Step 2: Extract Imagenette.
    extract_tgz(RAW_TGZ, RAW_DIR)

    # Step 3: Prepare output folder.
    prepare_output_folder(OUTPUT_DIR)

    # Step 4: Build imbalanced dataset.
    summary = []

    for class_id, class_name in CLASS_MAP.items():
        required_count = CLASS_COUNTS[class_name]

        print()
        print(f"Preparing class: {class_name}")
        print(f"Required number of images: {required_count}")

        all_images = collect_images_for_class(RAW_DIR, class_id)

        if len(all_images) < required_count:
            raise ValueError(
                f"Not enough images for {class_name}. "
                f"Required {required_count}, but found {len(all_images)}."
            )

        selected_images = random.sample(all_images, required_count)

        train_images, val_images, test_images = split_images(selected_images)

        split_dict = {
            "train": train_images,
            "val": val_images,
            "test": test_images,
        }

        for split_name, image_paths in split_dict.items():
            for i, src_path in enumerate(image_paths):
                dst_name = f"{class_name}_{i:04d}.jpg"
                dst_path = OUTPUT_DIR / split_name / class_name / dst_name
                resize_and_save_image(src_path, dst_path)

        summary.append(
            [
                class_name,
                required_count,
                len(train_images),
                len(val_images),
                len(test_images),
            ]
        )

    # Step 5: Print final summary.
    print()
    print("=" * 72)
    print("Dataset preparation complete!")
    print("=" * 72)
    print(f"Final dataset folder: {OUTPUT_DIR.resolve()}")

    print()
    print("Class distribution:")
    print(f"{'Class':20s} {'Total':>8s} {'Train':>8s} {'Val':>8s} {'Test':>8s}")
    print("-" * 60)

    total_all = 0
    total_train = 0
    total_val = 0
    total_test = 0

    for class_name, total, train_count, val_count, test_count in summary:
        total_all += total
        total_train += train_count
        total_val += val_count
        total_test += test_count

        print(
            f"{class_name:20s} "
            f"{total:8d} "
            f"{train_count:8d} "
            f"{val_count:8d} "
            f"{test_count:8d}"
        )

    print("-" * 60)
    print(
        f"{'TOTAL':20s} "
        f"{total_all:8d} "
        f"{total_train:8d} "
        f"{total_val:8d} "
        f"{total_test:8d}"
    )

    print()
    print("Folder check:")
    for split in ["train", "val", "test"]:
        split_total = 0

        for class_name in CLASS_COUNTS:
            folder = OUTPUT_DIR / split / class_name
            split_total += count_images(folder)

        print(f"{split}: {split_total} images")

    print()
    print("You can now use this folder with torchvision.datasets.ImageFolder.")
    print()
    print("Example:")
    print('    train_dataset = ImageFolder("imagenette10_imbalanced/train", transform=train_transform)')
    print('    val_dataset = ImageFolder("imagenette10_imbalanced/val", transform=val_transform)')
    print('    test_dataset = ImageFolder("imagenette10_imbalanced/test", transform=val_transform)')


if __name__ == "__main__":
    main()
