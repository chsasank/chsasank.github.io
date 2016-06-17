---
layout: post
title: Deep Learning With Torch
author: Preetham Sreenivas
---

[Torch](http://torch.ch/) is a scientific computing framework built on top of Lua[JIT]. The `nn` package and the ecosystem around it provide a very powerful framework for building deep learning models striking a perfect balance between speed and flexibility. 
It is used at [Facebook AI Research(FAIR)](https://research.facebook.com/ai), [Twitter Cortex](https://engineering.twitter.com/cortex), DeepMind, Yann LeCun's group at NYU, Fei-Fei Li's at Stanford and many more industrial and academic labs. 
If you are like me who doesn't like writing equations for backpropagation everytime you want to try a simple model, yet would like the power to do pretty much everything you can imagine, be it writing a custom loss functions, dreaming up an arbitrary acyclic graph network, using multiple GPU's or load pre-trained models on imagenet from [caffe model-zoo](https://github.com/BVLC/caffe/wiki/Model-Zoo) (yes, you can load models trained in caffe with a single line), torch arms you with everything you need complete with automatic differentiation.
Without further ado, let's jump right into the awesome world of deep learning.

## Prerequisites
  - Some knowledge of deep learning - [A Primer](http://neuralnetworksanddeeplearning.com/),  [Bengio's deep learning book](http://www.deeplearningbook.org/), [Hinton's Coursera course](https://www.coursera.org/course/neuralnets)
  - A bit of lua. It's syntax is very C-like and can be picked up fairly quickly if you know Python or Javascript - [Learn Lua in 15 minutes](http://tylerneylon.com/a/learn-lua/), [Torch For Numpy Users](https://github.com/torch/torch7/wiki/Torch-for-Numpy-users) 
  - A machine with torch installed since this is intented to be hands on. 

On Ubuntu 12+ and Mac OSX, Installing Torch looks like this

```
# in a terminal, run the commands WITHOUT sudo
$ git clone https://github.com/torch/distro.git ~/torch --recursive
$ cd ~/torch; bash install-deps;
$ ./install.sh
# On Linux with bash
$ source ~/.bashrc
# On OSX or in Linux with no bash.
$ source ~/.profile
```

Once you install torch, you can run a torch script using 

```
$ th script.lua
# alternatively you can fire up a terminal torch interpreter using th -i
$ th -i
# and run multiple scripts one by one, the variables will be accessible to other scripts
> dofile 'script1.lua'
> dofile 'script2.lua'
> print(variable) -- variable from either of these scripts.
```

The sections below are very code intensive, you can run these commands from torch's terminal interpreter.

```
$th -i
```

## Building A Model: The Basics
A module is the basic building block of any torch model. It has forward, backward methods for the forward and backward passes of backpropagation. You can combine them using containers, and ofcourse, calling forward and backward on containers propagates inputs and gradients correctly.

```
-- A simple mlp model with sigmoids

require 'nn'
linear1 = nn.Linear(100,10) -- A linear layer Module
linear2 = nn.Linear(10,2)

-- You can combine modulues using containers, sequential is the most used one
model = nn.Sequential() -- A container
model:add(linear1)
model:add(nn.Sigmoid())
model:add(linear2)
model:add(nn.Sigmoid())

-- the forward step
input = torch.rand(100)
target = torch.rand(2)
output = linear:forward(input)
```

Now we need a criterion to measure how well our model is performing, in other words, a loss function. `nn.Criterion` is the abstract class which all loss functions inherit. It provides `forward` and `backward` methods, computing loss and gradients respectively. Torch provides most of the commonly used criterions out of the box. It isn't much of an effort to write your own either.

```
criterion = nn.MSECriterioin() -- mean squared error criterion
loss = criterion:forward(output,target)
gradientsAtOutput = criterion:backward(output,target)
-- To perform the backprop step, we need to pass these gradients to the backward 
-- method of the model
gradAtInput = model:backward(input,gradientsAtOutput)
lr = 0.1  -- learning rate for our model
model:updateParameters(lr)  -- updates the parameters using the lr parameter.
```

The `updateParameters` method just subtracts the model parameters by gradients scaled by the learning rate. This is the vanilla [stochastic gradient descent](https://en.wikipedia.org/wiki/Stochastic_gradient_descent). Typically, the updates we do are more complex. For example, if we want to use [momentum](https://en.wikipedia.org/wiki/Stochastic_gradient_descent#Momentum), we need to keep track of updates we did in the previous epoch. There are lot more fancy optimization schemes like RMSProp, adam, adagrad, L-BFGS which do more complex things like adapting learning rate, momentum factor etc. The `optim` package provides optimization routines out of the box. 

## Dataset
We'll use the [German Traffic Sign Recognition Benchmark](http://benchmark.ini.rub.de/?section=gtsrb&subsection=news)(GTSRB) dataset. 
This dataset has 43 classes of traffic signs of varying sizes, illuminations and occlusions. 
There are 39000 tranining images and 12000 test images. 
Traffic sign in each of the images is not centered and has 10% border around it.

![Classes of GTSRB](assets/images/traffic/classes.jpg)

I have included a shell script for downloading the data along with the code for this tutorial in [this github repo](https://github.com/preethamsp/tutorial.gtsrb.torch)[^repo-note]. 

[^repo-note]: Code in the repo is much more polished than the snippets in the tutorial. It is modular and allows you to change model and/or datasets easily.

```
git clone https://github.com/preethamsp/tutorial.gtsrb.torch.git
cd tutorial.gtsrb.torch/datasets
bash download_gtsrb.sh
```

## Model

Let's build a downsized vgg style model with what we've learned. 

```
function createModel()
  require 'nn'
  nbClasses = 43
  local net = nn.Sequential()

  --[[building block: adds a convolution layer, batch norm layer and a relu activation to the net]]--
  function ConvBNReLU(nInputPlane, nOutputPlane)
  -- kernel size = (3,3), stride = (1,1), padding = (1,1)
    net:add(nn.SpatialConvolution(nInputPlane, nOutputPlane, 3,3, 1,1, 1,1))
    net:add(nn.SpatialBatchNormalization(nOutputPlane,1e-3))
    net:add(nn.ReLU(true))
  end

  ConvBNReLU(3,32)
  ConvBNReLU(32,32)
  net:add(nn.SpatialMaxPooling(2,2,2,2))
  net:add(nn.Dropout(0.2))
  ConvBNReLU(32,64)
  ConvBNReLU(64,64)
  net:add(nn.SpatialMaxPooling(2,2,2,2))
  net:add(nn.Dropout(0.2))
  ConvBNReLU(64,128)
  ConvBNReLU(128,128)
  net:add(nn.SpatialMaxPooling(2,2,2,2))
  net:add(nn.Dropout(0.2))
  net:add(nn.View(128*6*6))
  net:add(nn.Dropout(0.5))
  net:add(nn.Linear(128*6*6,512))
  net:add(nn.BatchNormalization(512))
  net:add(nn.ReLU(true))
  net:add(nn.Linear(512,nbClasses))
  net:add(nn.LogSoftMax())
  return net
end
```

The first layer contains 3 input channels because we're going to pass RGB images(3 channel). For Gray Scale images, the first layer would have 1 input channel. I encourage you to play around and modify the network [^model-note].

[^model-note]: You'll find that model code (`models/vgg_small.lua`) in the repo is different. It is designed to allow you to experiment quickly.

There are bunch of new modules which need some elaboration, the `Dropout` module randomly deactivates a neuron with some probability. It is known to help generalization by preventing co-adaptation between neurons, i.e., a neuron should now depend less on its peer, forcing it to learn a bit more. 
`BatchNormalization` is a very recent development. It is known to speed up convergence by normalizing the outputs of a layer to unit gaussian using the statistics of a batch. 

Let us use this model and train it . In the interest of brievity, I'll use these constructs directly. 
Code descibing these constructs is in `datasets/gtsrb.lua`

- `DataGen:trainGenerator(batchSize)`
- `DataGen:valGenerator(batchSize)` 

These will give us iterators over batches of train and test data respectively. 

## Using `optim` to train the model

Using stochastic gradient descent(`sgd`) from `optim` package to minimize a function `f` looks like this:

```
optim.sgd(feval, params, optimState)
```

where:

* `feval`: a user-defined function that respects the API: `f, df/params = feval(params)`
* `params`: the current parameter vector (a 1D torch.Tensor)
* `optimState`: a table of parameters, and state variables, dependent upon the algorithm

Since we are optimizing loss of the neural network, `parameters` should be the weights and other parameters of the network. 
We get these as a flattened 1d tensor using `model:getParameters`. It also returns a tensor containing gradients of these parameters. This is useful in creating `feval` function above.

```
model = createModel()
criterion = nn.ClassNLLCriterion() -- criterion we are optimizing: negative log loss

params, gradParams = model:getParameters()

local function feval()
  -- criterion.output stores the latest output of criterion
  return criterion.output, gradParams
end
```

We need to create `optimState` table and initialize it with configuration of our optimizer like learning rate and momentum:

```
optimState = {
      learningRate = 0.01,
      momentum = 0.9,
      dampening = 0.0,
      nesterov = true,
   }
```

Now, an update to the model should:

1. Compute the output of the model using `model:forward()` 
2. Compute the loss and the gradients at output layer using `criterion:forward()` and `criterion:backward()` respectively.
3. Update the gradients of the model parameters using `model:backward()` 
4. Update the `model` using `optim.sgd`

```
-- Forward pass
output = model:forward(input)
loss = criterion:forward(output, target)

-- Backward pass
critGrad = criterion:backward(output, target)
model:backward(input, critGrad)

-- Updates
optim.sgd(feval, params, optimState)
```
*Note*: The order above should be respected, as `backward` assumes `forward` was run just before it. Changing this order might result in gradients not being computed correctly.

## Putting it all together

Let's put it all together and write a function which trains the model for an epoch. 
We'll create a loop which iterates over the train data in batches and updates the model.

```
model = createModel()
criterion = nn.ClassNLLCriterion()
dataGen = DataGen('datasets/GTSRB/') -- Data generator

params, gradParams = model:getParameters()

batchSize = 32
optimState = {
      learningRate = 0.01,
      momentum = 0.9,
      dampening = 0.0,
      nesterov = true,
   }

function train()
  -- Dropout and BN behave differently during training and testing
  -- So, switch to training mode
  model:training() 

  local function feval()
      return criterion.output, gradParams
  end
      
  for input, target in dataGen:trainGenerator(batchSize) do
      -- Forward pass
      local output = model:forward(input)
      local loss = criterion:forward(output, target)
      
      -- Backward pass
      model:zeroGradParameters() -- clear grads from previous update
      local critGrad = criterion:backward(output, target)
      model:backward(input, critGrad)

      -- Updates
      optim.sgd(feval, params, optimState)
  end
end        
```

The test function is extremely similar except that we don't need to update the parameters

```
confusion = optim.ConfusionMatrix(nbClasses) -- to calculate accuracies

function test()
  model:evaluate()  -- switch to evaluate mode
  confusion:zero()  -- clear confusion matrix

  for input, target in dataGen:valGenerator(batchSize) do
    local output = model:forward(input)
    confusion:batchAdd(output, target)
  end
  
  confusion:updateValids()
  local test_acc = confusion.totalValid * 100  
  print(('Test accuracy: %.2f'):format(test_acc))
end
```

Now that everything is set, you can train your network and print the test accuracies

```
max_epoch = 20
for i = 1,20 do
    train()
    test()
end
```

An epoch takes around 30 seconds on a TitanX and gives about 97.7% accuracy after 20 epochs. 
This is a very basic model and honestly I haven't tried optimizing the parameters much. 
There are a lot of things that can be done to crank up the accuracies.

* Try different processing procedures. 
* Experiment with the net structure.
* Different weight initializations, learning rate schedules.
* An Ensemble of different models, for example , train multiple models and take a majority vote.

You can have a look at the state of the art on this dataset [here](http://torch.ch/blog/2015/09/07/spatial_transformers.html). They achieve upwards of 99.5% accuracy using a clever method to boost the geometric variation of CNN's.

## Conclusion
We looked at how to build a basic mlp in torch. We then moved on to building a Convolutional Neural Network and trained it to solve a real world problem of traffic sign recognition.

For a beginner, torch/LUA might not be a easy. But once you get a hang of it, 
you have access to a deep learning framework which is very flexible yet fast. 
You will be able to easily reproduce latest research or try new stuff unlike in rigid frameworks like keras or nolearn.
I encourage you to give it a fair try if you are going anywhere near deep learning.

## Resources
* [Torch Cheat Sheet](https://github.com/torch/torch7/wiki/Cheatsheet)
* [Awesome Torch](https://github.com/carpedm20/awesome-torch)
* [Torch Blog](http://torch.ch/blog/)
* [Facebook's Resnet Code](https://github.com/facebook/fb.resnet.torch/)
* [Oxford's ML Course Practicals](https://github.com/oxford-cs-ml-2015)
* [Learn torch from Github repos](http://ruotianluo.github.io/2016/03/10/torch-repositories.html)
   



  



