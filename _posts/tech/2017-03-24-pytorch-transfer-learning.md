---
layout: post
title: PyTorch Transfer Learning Tutorial
author: Sasank Chilamkurthy
description: In this tutorial, you will learn how to train your network using transfer learning. You can read more about the transfer learning at cs231n notes.
---

I have written this for [PyTorch official tutorials](http://pytorch.org/tutorials/beginner/transfer_learning_tutorial.html). Please read this tutorial there.


> In this tutorial, you will learn how to train your network using transfer learning. You can read more about the transfer learning at [cs231n notes](http://cs231n.github.io/transfer-learning/).
>
> Quoting this notes:
> In practice, very few people train an entire Convolutional Network from scratch (with random initialization), because it is relatively rare to have a dataset of sufficient size. Instead, it is common to pretrain a ConvNet on a very large dataset (e.g. ImageNet, which contains 1.2 million images with 1000 categories), and then use the ConvNet either as an initialization or a fixed feature extractor for the task of interest.
> ...
