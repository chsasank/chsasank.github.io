---
layout: post
title: MS AI Challenge - Our entry
---

## Architecture

Our architecture is based on pretrained bidirectional transformer BERT[^1]. In particular, approach is heavily inspired by that on the SWAG dataset from the above paper (section 4.4 in [^1]). We also borrowed idea of attention over attention[^2].

Our algorithm/architecture is as follows:

1. Apply BERT individually to each of the 10 (query, passage) pairs corresponding to ith example. Take hidden state corresponding to `[cls]` token of BERT to get a matrix of size $$R^{10 \times H}$$ where $H$ is the hidden state size of BERT.
2. Add another transformer encoder on top of this matrix i.e. attention over attention (AoA) so that relative importances of passages are considered.
3. Finally, apply a fully connected layer on the AoA so that we have logit scores outputs $$S \in R^{10}$$ corresponding to relevance of each passage.

## Loss functions

There are plethora of loss functions which can be applied to the task of learning to rank. For a brief overview, see this paper[^3]. We have experimented with pointwise, pairwise and listwise loss functions. Pointwise loss functions like binary cross entropy performed the worst. So we abandoned pointwise loss functions and instead experimented on pairwise and listwise logistic functions. Here are the list of loss functions we used to train:

1. Pairwise logistic loss
2. Pairwise hinge loss
3. Softmax loss
4. ListMLE loss

For both the pairwise losses, we have employed lambda weighting[^4] for pairs to maximize MRR.

## Data augmentation

We have used additional data from a very similar task: [MSMARCO](http://www.msmarco.org). One key difference is that none of the passages can be relevant. To handle this gracefully, we have introduced a dummy 'passage', `none of the above` with its logit score as 0. Label corresponding to this dummy passage will be 1 if none of the 10 passages are relevant and 0 otherwise. Thus we apply previously described losses on $$S' = [0, S_0, S_1, \dots S_9]$$

## Training

We have split the data into 4:1 split for training and validation respectively. We have trained about 30 models with varying parameters like data augmentation, choice of loss function, learning rate, batch size, number of epochs etc. We have used only BERT_BASE as our base architecture because BERT_LARGE was too slow to train and did not perform as well on our evaluation set. We have set maximum sequence length of 256 to ensure our models work with large passages.

We have used tensorflow implementation of BERT and TPUs provided by google colab to train our models.

## Model stacking

To combine the predictions of all our models, we have trained a logistic regression model instead of usual ensembling techniques like averaging. Features of this logistic regression model were flattened logit predictions of all our models. Labels were passage ids which was relevant. We trained this stacked model on 9/10 of our eval split and validated on the rest. We have also trained another logistic regression model with following additional features: first word in query, length of the query.

Our three final submissions were:

1. Plain average ensembling
2. Logistic regression stacking
3. Logistic regression stacking with additional query features

[^1]: Devlin, Jacob, et al. "Bert: Pre-training of deep bidirectional transformers for language understanding." arXiv preprint arXiv:1810.04805 (2018).
[^2]: Cui, Yiming, et al. "Attention-over-attention neural networks for reading comprehension." arXiv preprint arXiv:1607.04423 (2016).
[^3]: Pasumarthi, Rama Kumar, et al. "TF-Ranking: Scalable TensorFlow Library for Learning-to-Rank." arXiv preprint arXiv:1812.00073 (2018).
[^4]: Burges, Christopher JC. "From ranknet to lambdarank to lambdamart: An overview." Learning 11.23-581 (2010): 81.