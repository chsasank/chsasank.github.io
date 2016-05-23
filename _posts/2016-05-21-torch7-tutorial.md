---
layout: post
title: Torch Tutorial for PlantVillage Challenge
---

There is this interesting challenge called [PlantVillage challenge](https://www.crowdai.org/challenges/1) hosted on a newly built platform, [crowdai](https://www.crowdai.org).
In this challenge, you are required to identify the disease of a plant from an image of its leaf.

Dataset include both healthy and diseased leaves. Training dataset has 21917 images.
There are 38 classes of crop-disease pairs in the dataset:

{% image /assets/images/plantvillage/classes.jpg 600 500 %}

We'll use popular deep learning platform [torch](http://torch.ch) to solve this problem.
This will be a hands-on tutorial covering training of 
Alexnet
{% sidenote alexnet [ImageNet Classification with Deep Convolutional Neural Networks](https://papers.nips.cc/paper/4824-imagenet-classification-with-deep-convolutional-neural-networks.pdf) %}.
Tutorial will be accompanied by a [repo containing complete working code](https://github.com/chsasank/plantvillage-challenge).
It will include VGGNet 
{% sidenote vggnet [Very Deep Convolutional Networks for Large-Scale Image Recognition](http://arxiv.org/abs/1409.1556) %}
and ResNet 
{% sidenote resnet [Deep Residual Learning for Image Recognition](https://arxiv.org/abs/1512.03385) %}. 

This tutorial assumes familiarity with convolutional neural networks. If you are not, you can go through very readable *Neural Networks and Deep Learning* book by Michael Nielsen.
[Chapter 6](http://neuralnetworksanddeeplearning.com/chap6.html) is the essential reading. 
Just read the [first section](http://neuralnetworksanddeeplearning.com/chap6.html#introducing_convolutional_networks) in this chapter if you are in a hurry. Go on, it's a easy read. I'll wait for you :).

Ok, Done? Let's first start with preprocessing and augmentation of the images.

{% comment %} 
    TODO: talk about github source
{% endcomment %}


## Preprocessing
Firstly, let's download the dataset and extract it into a directory. 
We'll divide the images into two directories, `train` and `val` for training and validation sets respectively.
I used a simple bash script to do this:

```bash
cd directory/contaning/c_0c_1...etcdirectories
mkdir -p train val
for i in {0..37}; do mkdir val/c_$i; done
mv c_* train

cd train
find . -iname *.jpg | shuf | head -n 2100| xargs -I{} mv {} ../val/{}
```

This will move random 2100 images (about 10% of the dataset) in to `val` directory and rest into `train` directory.
Directory structure should now look like:

```
.
├── train
│   ├── c_0
│   │   ├── img_name.JPG
│   │   ├── ...
│   │   └── img_name.JPG
│   ├── c_1
│   ├── ...
│   ├── c_36
│   └── c_37
│  
└── val
    ├── c_0
    ├── c_1
    ├── ...
    ├── c_36
    └── c_37
        ├── img_name.JPG
        ├── ...
        └── img_name.JPG
```

Before feeding images into neural networks we'll normalize the images with mean and standard deviation of RGB channels computed from a random subset of ImageNet. 

In the world of deep learning, dataset of 20,000 images is a relatively small dataset. 
We'll therefore augment the data with randomly sized crops, color jittering and horizontal flips. 
Code to do these transformations is in `datasets/transforms.lua`. Most of it is borrowed from [fb.resnet.torch](http://github.com/facebook/fb.resnet.torch) repo.

We will load the images in batches and do all these processing on the fly. This is done by writing a class named `DataGen` which does this. 
Essentially, code{% sidenote side1 Understanding how iterators work in lua can be a little tricky. Read the following [ documentation](https://www.lua.org/pil/7.1.html) for details. %} can be summarized as :

`datasets/plantvillage.lua`:

```
require 'paths'
t = require 'datasets/transforms.lua'

function DataGen:__init(path)
    -- path is path of directory containing 'train' and 'val' folders
    self.rootPath = path
    self.trainImgPaths = self.findImages(paths.concat(self.rootPath, 'train'))
    self.valImgPaths = self.findImages(paths.concat(self.rootPath, 'val'))
    self.nbTrainExamples = #self.trainImgPaths
    self.nbValExamples = #self.valImgPaths 
end

-- Some utility functions
function DataGen.findImages(dir)
    -- Returns a table with all the image paths found in dir using 'find'
    ...
end

local function getClass(path)
    -- gets class from the name of the parent directory
    local className = paths.basename(paths.dirname(path))
    return tonumber(className:sub(3)) + 1
end

--- Iterator 
function DataGen:generator(pathsList, batchSize, preprocess) 
    -- pathsList is table with paths of images to be iterated over
    -- batchSize is number of images to be loaded in one iteration
    -- preprocess is function which will be applied to image after it's loaded

    -- Split all the paths into random batches
    local pathIndices = torch.randperm(#pathsList)
    local batches = pathIndices:split(batchSize)
    local i = 1
   
    return function ()
        if i <= #batches then
            local currentBatch = batches[i]         

            local X = torch.Tensor(currentBatch:size(1), 3, 224, 224)
            local Y = torch.Tensor(currentBatch:size(1))

            for j = 1, currentBatch:size(1) do
                local currentPath = pathsList[currentBatch[j]]
                X[j] = preprocess(t.loadImage(currentPath))
                Y[j] = getClass(currentPath)
            end

            i = i + 1
            return X, Y
        end
   end
end

function DataGen:trainGenerator(batchSize)
    local trainPreprocess = t.Compose{
        t.RandomSizedCrop(224),
        t.ColorJitter({
            brightness = 0.4,
            contrast = 0.4,
            saturation = 0.4,
        }),
        t.Lighting(0.1, t.pca.eigval, t.pca.eigvec),
        t.ColorNormalize(t.meanstd),
        t.HorizontalFlip(0.5),}

   return self:generator(self.trainImgPaths, batchSize, trainPreprocess)
end


function DataGen:valGenerator(batchSize)
    local valPreprocess = t.Compose{
         t.Scale(256),
         t.ColorNormalize(t.meanstd),
         t.CenterCrop(224),}
   return self:generator(self.valImgPaths, batchSize, valPreprocess)
end

```

Complete code for this class with some error catching is at `datasets/plantvillage.lua`. 
We can now simply use a `DataGen` object to write a `for` loop to iterate over all the images:

```
for input, target in dataGen:trainGenerator(batchSize) do
    -- code to train your model
end
```

Neat, isn't it?

## Models
Now that we're done with loading and preprocessing the image files, let's start writing model descriptions.
These are the actual powerhouses that will be trained to classify. 
It's quite easy to code up a model.

Let's code up alexnet as an example:

`models/alexnet.lua`:

```
require 'nn'

function createModel(opt)
    opt = opt or {}
    nbClasses = opt.nbClasses or 38
    nbChannels = opt.nbChannels or 3

    local features = nn.Sequential()
    
    features:add(nn.SpatialConvolution(nbChannels,64,11,11,4,4,2,2))    -- 224 -> 55
    features:add(nn.SpatialMaxPooling(3,3,2,2))                   --  55 ->  27
    features:add(nn.ReLU(true))
    features:add(nn.SpatialBatchNormalization(64))
    
    features:add(nn.SpatialConvolution(64,192,5,5,1,1,2,2))       --  27 -> 27
    features:add(nn.SpatialMaxPooling(3,3,2,2))                   --  27 ->  13
    features:add(nn.ReLU(true))
    features:add(nn.SpatialBatchNormalization(192))
    
    features:add(nn.SpatialConvolution(192,384,3,3,1,1,1,1))      --  13 ->  13
    features:add(nn.ReLU(true))
    features:add(nn.SpatialBatchNormalization(384))
    
    features:add(nn.SpatialConvolution(384,256,3,3,1,1,1,1))      --  13 ->  13
    features:add(nn.ReLU(true))
    features:add(nn.SpatialBatchNormalization(256))
    
    features:add(nn.SpatialConvolution(256,256,3,3,1,1,1,1))      --  13 ->  13
    features:add(nn.SpatialMaxPooling(3,3,2,2))                   --  13 -> 6
    features:add(nn.ReLU(true))
    features:add(nn.SpatialBatchNormalization(256))

    local classifier = nn.Sequential()
    classifier:add(nn.View(256*6*6))
    
    classifier:add(nn.Dropout(0.5))
    classifier:add(nn.Linear(256*6*6, 4096))
    classifier:add(nn.ReLU(true))
    
    classifier:add(nn.Dropout(0.5))
    classifier:add(nn.Linear(4096, 4096))
    classifier:add(nn.ReLU(true))
    
    classifier:add(nn.Linear(4096, nbClasses))

    model:add(features):add(classifier)

    return model
end
```

As you can see, model is just a stack of convolutional layers, max pooling and fully connected layers.

We will use `nn.CrossEntropyCriterion` as our criterion.


## Training

In torch, `net:forward(input)` computes the `output` of neural network. 
This is the forward pass of the backpropagation algorithm while `net:backward(input,gradOutput)` is the backward pass of the backpropagation. 
Forward and backward passes for `criterion` are also very similar.

We'll use [adam](https://github.com/torch/optim/blob/master/doc/index.md#optim.adam) optimizer from `optim` package to make the updates to the network. A minimal training script will look like:

```
require 'nn'
require 'datasets/plantvillage.lua'
require 'models/alexnet.lua'

net = createModel()
criterion = nn.CrossEntropyCriterion()
dataGen = DataGen('path/to/folder/with/train-val-directories/')

-- adam initial learning rate and momentum parameters
optimState = { 
        learningRate = 0.01,
        beta1 = 0.9,
    }

-- Parameters for network that need to be optimized
params, gradParams = net:getParameters()

local function feval()
    return criterion.output, gradParams
end

function train()
    for input, target in dataGen:trainGenerator(batchSize) do
        -- Forward pass
        output = net:forward(input)
        criterion:forward(output, target)

        -- Backward pass
        net:zeroGradParameters()
        critGrad = criterion:backward(output, target)
        net:backward(input, critGrad)
        
        -- Make updates using adam
        optim.adam(feval, params, optimState)
    end
end

for i = 1, nbEpochs do
    train()
end
```

If you look at the code in the repo, you'll find that I have divided training into `main.py` and `train.py` scripts.
In `main.lua`, we manage the configuration of the neural network and criterion.
In `train.lua`, I wrote a `Trainer` class with `Trainer:train()` and `Trainer:validate()` methods very similar to `train()` function above except with some logging.
This class will allow us to switch models and criterions easily in `main.lua`.