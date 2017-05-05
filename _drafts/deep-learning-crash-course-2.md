---
layout: post
title: Deep Learning Crash Course Part 2
author: Sasank Chilamkurthy
---

## Deep Neural Networks

Whenever you are asked to do any complex task, you usually break it down to sub tasks and solve the component subtasks. For instance, suppose you're designing a logical circuit to multiply two numbers. Chances are your circuit will look something like this:

<span class="marginnote">
    **Figure**: Logical circuit for multiplication.
    [Source](http://neuralnetworksanddeeplearning.com/chap5.html").
</span>
<img src='/assets/images/crash_course/circuit_multiplication.png'>

Similarly deep neural networks (i.e lot of layers) can build up multiple layers of abstraction. Consider the following network:

<span class="marginnote">
    **Figure**: Deep Neural Network.
    [Source](http://neuralnetworksanddeeplearning.com/chap5.html").
</span>
<img src='/assets/images/crash_course/tikz36.png'>

If we're doing visual pattern recognition, then the neurons in the first layer might learn to recognize edges, the neurons in the second layer could learn to recognize more complex shapes, say triangle or rectangles, built up from edges. The third layer would then recognize still more complex shapes. And so on. These multiple layers of abstraction seem likely to give deep networks a compelling advantage in learning to solve complex pattern recognition problems.

How do we train such deep networks? Stochastic gradient descent as usual. But we'll run into trouble, with our deep networks not performing much (if at all) better than shallow networks.

Let's try to understand why are deep networks hard to train:

1. Consider the number of parameters in the network. They are huge! If we have to connect 1000 unit hidden layer to 224x224 (50,176) image, we have $65,536*1000 \approx 50e6$ parameters in that layer alone! There are so many parameters that network can easily overfit on the data without generalization.
2. Gradients are unstable. Recall the expression for the gradients,
    $\frac{\partial C}{\partial \theta_j} = \frac{\partial y_n}{\partial u_n} * \frac{\partial y_{n-1}}{\partial u_{n-1}} * \cdots * \frac{\partial y_{j-1}}{\partial u_{j-1}} * \frac{\partial y_j}{\partial \theta_j}$. If few of $\frac{\partial y_i}{\partial u_i} \ll 1$, they will multiply up and make $\frac{\partial C}{\partial \theta_j} \approx 0 $.
    <span id="backprop-sigmoid" class="margin-toggle sidenote-number"></span>
    Similarly if few of $\frac{\partial y_i}{\partial u_i} \gg 1$, they make $\frac{\partial C}{\partial \theta_j} \to \infty$.
<span class="sidenote">
    This is the reason why sigmoids are avoided. For sigmoid, $\frac{\partial y}{\partial u} = \frac{d \sigma}{d z}|_{z=u}$ is close to zero if $u$ is either too large or too small. It's maximum is only $1/4$
</span>

Keep these two points in mind. We will see several approaches to deep learning that to some extent manage to overcome or route around these.

### Convolutional Neural Networks

Let's go back to the problem of handwritten digit recognition. MLP looks like this:

<span class="marginnote">
    **Figure**: MLP for MNIST.
    [Source](http://neuralnetworksanddeeplearning.com/chap5.html").
</span>
<img src='/assets/images/crash_course/mnist_net.png'>

In particular, we have connected all the pixels in 28x28 images i.e, 784 pixels to each neuron in hidden layer 1.

Upon reflection, it's strange to use networks with fully-connected layers to classify images. The reason is that such a network architecture does not take into account the spatial structure of the images. For instance, it treats input pixels which are far apart and close together on exactly the same footing. Such concepts of spatial structure must instead be inferred from the training data.

But what if, instead of starting with a network architecture which is *tabula rasa*, we used an architecture which tries to take advantage of the spatial structure? In this section I describe convolutional neural networks. These networks use a special architecture which is particularly well-adapted to classify images. Using this architecture makes convolutional networks fast to train. This, in turn, helps us train deep, many-layer networks, which are very good at classifying images. Today, deep convolutional networks or some close variant are used in most neural networks for image recognition.

Convolutional neural networks use three basic ideas: 

1. Local receptive fields
2. Shared weights
3. Pooling. 

**Local receptive fileds**

As per usual, we'll connect the input pixels to a layer of hidden neurons. But we won't connect every input pixel to every hidden neuron. Instead, we only make connections in small, localized regions of the input image.


<span class="marginnote">
    **Figure**: Local receptive fields of convolution.
    [Source](http://neuralnetworksanddeeplearning.com/chap6.html").
</span>
<img src='/assets/images/crash_course/conv1.png'>

That region in the input image is called the *local receptive field* for the hidden neuron. It's a little window on the input pixels. Each connection learns a weight. And the hidden neuron learns an overall bias as well. You can think of that particular hidden neuron as learning to analyze its particular local receptive field.

We then slide the local receptive field across the entire input image. For each local receptive field, there is a different hidden neuron in the first hidden layer. To illustrate this concretely, let's start with a local receptive field in the top-left corner:

<span class="marginnote">
    **Figure**: Convolution.
    [Source](http://neuralnetworksanddeeplearning.com/chap6.html").
</span>
<img src='/assets/images/crash_course/conv2.png'>

Then we slide the local receptive field over by one pixel
<span id="conv-slide" class="margin-toggle sidenote-number"></span>
<span class="sidenote">
Sometimes a different *stride* length (e.g, 2) is used.
</span>
to the right (i.e., by one neuron), to connect to a second hidden neuron
<span id="conv-out" class="margin-toggle sidenote-number"></span>
<span class="sidenote">
Note that if we have a 28×28 input image, and 5×5 local receptive fields, then there will be 24×24 neurons in the hidden layer.
</span>:

<span class="marginnote">
    **Figure**: Slide the convolution.
    [Source](http://neuralnetworksanddeeplearning.com/chap6.html").
</span>
<img src='/assets/images/crash_course/conv3.png'>

**Shared weights and biases**

I've said that each hidden neuron has a bias and 5×5 weights connected to its local receptive field. What I did not yet mention is that we're going to use the same weights and bias for each of the 24×24 hidden neurons.

Following animation makes the schematic of convolution clear:

<span class="marginnote">
    **Figure**: Convolution Schematic. Note that biases are not shown here.
    [Source](http://ufldl.stanford.edu/tutorial/supervised/FeatureExtractionUsingConvolution/).
</span>
<img src='/assets/images/crash_course/convolution_schematic.gif'>


Sharing weights and biases means that all the neurons in the first hidden layer detect exactly the same feature just at different locations in the input image. To see why this makes sense, consider the following convolution filter:

<span class="marginnote">
    **Figure**: A convolutional filter.
    [Source](https://adeshpande3.github.io/adeshpande3.github.io/A-Beginner's-Guide-To-Understanding-Convolutional-Neural-Networks/).
</span>
<img src='/assets/images/crash_course/conv_example1.png'>

Let's take an example image and apply convolution on a receptive filed:

<span class="marginnote">
    **Figure**: Convolution on an example image.
    [Source](https://adeshpande3.github.io/adeshpande3.github.io/A-Beginner's-Guide-To-Understanding-Convolutional-Neural-Networks/).
</span>
<img src='/assets/images/crash_course/conv_example2.png'>

<span class="marginnote">
    **Figure**: Apply convolution.
    [Source](https://adeshpande3.github.io/adeshpande3.github.io/A-Beginner's-Guide-To-Understanding-Convolutional-Neural-Networks/).
</span>
<img src='/assets/images/crash_course/conv_example3.png'>

Basically, in the input image, if there is a shape that generally resembles the curve that this filter is representing, then all of the multiplications summed together will result in a large value! Now let’s see what happens when we move our filter.

<span class="marginnote">
    **Figure**: Apply convolution at a different receptive field.
    [Source](https://adeshpande3.github.io/adeshpande3.github.io/A-Beginner's-Guide-To-Understanding-Convolutional-Neural-Networks/).
</span>
<img src='/assets/images/crash_course/conv_example4.png'>

Therefore, this convolution picks up a right bending curve wherever it is on .
To put it in slightly more abstract terms, convolutional networks are well adapted to the translation invariance of images: move a picture of a cat (say) a little ways, and it's still an image of a cat

The network structure I've described so far can detect just a single kind of localized feature. To do image recognition we'll need more than one **feature map**. And so a complete convolutional layer consists of several different feature maps

<span class="marginnote">
    **Figure**: Multiple feature maps.
    [Source](https://adeshpande3.github.io/adeshpande3.github.io/A-Beginner's-Guide-To-Understanding-Convolutional-Neural-Networks/).
</span>
<img src='/assets/images/crash_course/conv4.png'>

A big advantage of sharing weights and biases is that it greatly reduces the number of parameters involved in a convolutional network. For each feature map we need 25=5×5 shared weights, plus a single shared bias. So each feature map requires 26 parameters. If we have 20 feature maps that's a total of 20×26=520 parameters defining the convolutional layer. By comparison, suppose we had a fully connected first layer, with 784=28×28 input neurons, and a relatively modest 30 hidden neurons. That's a total of 784×30 weights, plus an extra 30 biases, for a total of 23,550 parameters.

**Pooling layers**

In addition to the convolutional layers just described, convolutional neural networks also contain pooling layers. Pooling layers are usually used immediately after convolutional layers. What the *pooling layers* do is simplify the information in the output from the convolutional layer.

In detail, a pooling layer takes each feature map  output from the convolutional layer and prepares a condensed feature map. For instance, each unit in the pooling layer may summarize a region of (say) 2x2 neurons in the previous layer. As a concrete example, one common procedure for pooling is known as *max-pooling*. In max-pooling, a pooling unit simply outputs the maximum activation in the 2x2 input region, as illustrated in the following diagram:


<span class="marginnote">
    **Figure**: Pooling Layer.
    [Source](http://neuralnetworksanddeeplearning.com/chap6.html).
</span>
<img src='/assets/images/crash_course/max_pooling.png'>

Note that since we have 24×24 neurons output from the convolutional layer, after pooling we have 12×12 neurons.

As mentioned above, the convolutional layer usually involves more than a single feature map. We apply max-pooling to each feature map separately. So if there were three feature maps, the combined convolutional and max-pooling layers would look like:

<span class="marginnote">
    **Figure**: Convolution and pooling layer.
    [Source](http://neuralnetworksanddeeplearning.com/chap6.html).
</span>
<img src='/assets/images/crash_course/max_pooling2.png'>

We can think of max-pooling as a way for the network to ask whether a given feature is found anywhere in a region of the image. It then throws away the exact positional information. The intuition is that once a feature has been found, its exact location isn't as important as its rough location relative to other features. A big benefit is that there are many fewer pooled features, and so this helps reduce the number of parameters needed in later layers.

### Case Study: LeNet

Let's put everything we've learnt together and analyze one of the very early successes of convolutional networks: LeNet. This is the *architecture* of LeNet:

<span class="marginnote">
    **Figure**: Convolution and pooling layer.
    [Source](http://yann.lecun.com/exdb/publis/pdf/lecun-01a.pdf).
</span>
<img src='/assets/images/crash_course/lenet.png'>

Let's go over each of the component layers of LeNet:
<span id="lenet" class="margin-toggle sidenote-number"></span>
<span class="sidenote">
    I actually describe slightly modified version of LeNet. 
</span>

* **Input**: Gray scale image of size 32 x 32.
* **C1**: Convolutional layer of 6 feature maps, kernel size (5, 5) and stride 1. Output size therefore is 6 X 28 x 28. Number of trainable parameters is $(5*5 + 1) * 6 = 156$.
* **S2**: Pooling/subsampling layer with kernel size (2, 2) and stride 2. Output size is 6 x 14 x 14. Number of trainable parameters = 0. 
* **C3**: Convolutional layer of 16 feature maps. Each feature map is connected to all the 6 feature maps from the previous layer. Kernel size and stride are same as before. Output size is 16 x 10 x 10. Number of trainable parameters is $(6*5*5 + 1)*16 = 2416$.
* **S4**: Pooling layer with same *hyperparameters* as above. Output size = 16 x 5 x 5. 
* **C5**: Convolutional layer of 120 feature maps and kernel size (5, 5). This amounts to *full connection* with outputs of previous layer. Number of parameters are $(16*5*5 + 1)*120 = 48120$.
* **F6**: *Fully connected layer* of 84 units. i.e, All units in this layer are connected to previous layer's outputs<span id="fc" class="margin-toggle sidenote-number"></span><span class="sidenote">This is same as layers in MLP we've seen before.</span>
* **Output**: Fully connected layer of 10 units with softmax activation