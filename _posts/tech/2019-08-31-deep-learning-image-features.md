---
layout: post
title: Simple Python Package to Extract Deep Learning Features
---

Ever wanted to do a hacky computer vision project? But you don't want to invest time on learning/using complicated deep learning libraries like PyTorch or TensorFlow? Enter [`image_features`](https://github.com/chsasank/image_features):

```bash
$ pip install -U git+https://github.com/chsasank/image_features.git
```

```python
from image_features import image_features
features = image_features(['your_image_1.png', 'your_image_2.jpg'])
```

I have been in the above situation a lot of times. Even if you're familiar with these deep learning libraries, there's no way to escape boilerplate code. For example, [following code](https://github.com/Cadene/pretrained-models.pytorch/blob/master/README.md) extracts features using PyTorch:

```python
import torch
import pretrainedmodels.utils as utils

# Load model
model_name = 'nasnetalarge'
model = pretrainedmodels.__dict__[model_name](
    num_classes=1000, pretrained='imagenet')
model.eval()

load_img = utils.LoadImage()

# transformations depending on the model
# rescale, center crop, normalize, and others (ex: ToBGR, ToRange255)
tf_img = utils.TransformImage(model)

# Load and transform image
path_img = 'data/cat.jpg'
input_img = load_img(path_img)           # 3x400x225
input_tensor = tf_img(input_img)         # 3x299x299
input_tensor = input_tensor.unsqueeze(0) # 1x3x299x299

with torch.no_grad():
    image_features = model.get_features(input) # 1x1000
```

I figured that I'd have the boilerplate code in a python package which has super simple interface. This package can support useful features like loading different deep learning models, running them on gpu if available, loading/transforming images with multiprocessing and so on.

`image_features` package extracts features using [imagenet](http://www.image-net.org) trained deep learning models. Since these models have seen upwards of million images during their training, their features can generalize to most imaging tasks. This technique is called [transfer learning](http://cs231n.github.io/transfer-learning/).

### Classifying images with `image_features`

On a leisurely Saturday afternoon, I was browsing fashion photography blog [The Sartorialist](https://www.thesartorialist.com) and wanted to look at outfits without any layering. 

Layering            |  No Layering
:-------------------------:|:-------------------------:
![](/assets/images/sartorialist/layered.jpg)  |  ![](/assets/images/sartorialist/no_layer.jpg)


But there are no tags or text to allow me to search for this in the website. So I ended up doing what any hacker would do: I scraped the blog, annotated some 400 images and trained a classifier on the images.

I've put up a sample of the scraped images on a bucket. Download the data from [here](https://storage.googleapis.com/public-sasank/sartorialist_images.zip) and extract the zip file. Load the annotations:

```python
import pandas as pd
df = pd.read_csv('archive/annotations.csv')
img_paths = list(df['path'])
no_layer = list(df['no_layer'])
```

Extract features:

```python
from image_features import image_features

n = round(0.8 * len(img_paths))
X_train = image_features(img_paths[:n], progress=True)
y_train = no_layer[:n]

X_val = image_features(img_paths[n:], progress=True)
y_val = no_layer[n:]
```

Train scikit-learn classifier:

```python
from sklearn import linear_model
import numpy as np
clf = linear_model.LogisticRegressionCV(
    max_iter=1000,
    Cs=np.geomspace(1e-1, 1e-7, 15),
    class_weight='balanced'
)
clf.fit(X_train, y_train)
print('train score:', clf.score(X_train, y_train))
print('val score:', clf.score(X_val, y_val))
```

That's all, your model is trained! This simple model achieves accuracy of 92% on validation set!

I hope this package lowers the entry barrier to computer vision. 
