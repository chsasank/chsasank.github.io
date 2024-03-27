---
layout: post
title: "Why Writing Matrix Multiplication Kernel is So Hard"
author: Sasank Chilamkurthy
twitter_image: 
---

It has been almost a month since I wrote a blog: time to document my learnings has come yet again. I have been spending last month dissecting the innards of a high performance matrix multiplication kernel. It is a deceptively simple kernel that is very hard to get right. It took time for me to understand why this has to be so. I think I now understand it reasonably well. We will explore the following in this post:

1. Space time tradeoff
2. Systolic arrays
3. Does threading matter?
4. Red blue pebble game: cache hierarchy

## Space Time Tradeoff

