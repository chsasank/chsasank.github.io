---
layout: post
title: Nvidia's ARM acquisition will start AI winter 
author: Sasank Chilamkurthy
---

It [looks like](https://www.theverge.com/2020/9/12/21434110/softbank-may-sell-arm-to-nvidia) Nvidia is closing in on deal with SoftBank to buy ARM for more than $40mn. [Rumors and news](https://news.ycombinator.com/item?id=24009177) about the acquisition have been going around since at least August.
[Everybody](https://www.ft.com/content/4970848d-7821-45dc-b8cb-211036be5d30) is [concerned](https://tech.newstatesman.com/business/hermann-hauser-nvidia-destroy-arm) [about](https://ownsnap.com/us-government-should-block-nvidia-from-acquiring-arm-open-source-is-in-danger-zone/) the [acquisition](https://ownsnap.com/us-government-should-block-nvidia-from-acquiring-arm-open-source-is-in-danger-zone/) [including](https://www.telegraph.co.uk/technology/2020/08/10/arm-sale-softbank-tragedy-tech-sector-says-former-city-minister/) [ARM's](https://www.bbc.com/news/technology-53678506) [co-founders](https://hothardware.com/news/arm-co-founders-question-nvidias-motives-softbank-deal) [themselves]((https://phonemantra.com/arm-co-founders-fear-nvidia-takeover/)).
If so much has already been said about this, what's the point of this post? This post aims to add artificial intelligence (AI) to the conversation.
To understand this point, we have to step back a bit and examine the history and business models of key players. I aim to keep the conversation accessible to someone unacquainted with technology and hence the lengthy post.

## History and Business Models

It's important to understand the history of AI research in detail before we appreciate the reasons for rise of Nvidia. We'll then continue with business models of ARM and finally end with SoftBank.

### Artificial Intelligence (AI)

There's no denying the pervasiveness of AI in the modern world. AI is everywhere:

* In your pocket. Ever used Uber? [Uber uses AI](https://eng.uber.com/uber-ai-blog-2019/) to match you to drivers. Google assistant and Siri are obviously AI.
* In your living room. It was only in [science fiction](https://www.youtube.com/watch?v=1ZXugicgn6U), until recently, [where you can say aloud](https://www.amazon.in/gp/help/customer/display.html?nodeId=GNQ59GMNBGBU3U8L) *Alexa, play Pink Floyd* and *Comfortably Numb* plays in background.
* In your office. When you write a mail, have you received suggestions from gmail about your next sentence or phrase - as if [Google knows](https://www.techspot.com/news/74533-new-ai-powered-gmail-feature-can-write-emails.html) what you're about to type. 
* In your hospital. If you end up having a stroke, your brain [might be saved](https://info.vrad.com/hubfs/CaseStudy_AI_GreaterRegional_1119%20(002).pdf?__hstc=&__hssc=&hsCtaTracking=e9be34c0-46d3-45e1-90d8-b8153c20804e%7C7abcaae7-6366-4ff0-ad77-834471104fe7) by AI's brain (!) without you ever knowing. AI is enabling faster treatment for strokes and better management of [diseases](https://arxiv.org/abs/2006.05509) and [even pandemics](https://www.technologyreview.com/2020/04/23/1000410/ai-triage-covid-19-patients-health-care/).

These advances are a result of hard work of some of the brightest minds of the world. Alan Turing, the patron saint for computers, wrote a [seminal paper](https://en.wikipedia.org/wiki/Computing_Machinery_and_Intelligence) on AI in 1950 even before transistors were invented and computers were a thing. Many preeminent computer scientists worked on AI as evidenced by [Turing Awards](https://en.wikipedia.org/wiki/Turing_Award) for them. More recently, two Canadian and a French researchers led the deep learning revolution which enabled all the above innovations.

These advances in computer science were not gradual - [science doesn't work that way](https://www.lri.fr/~mbl/Stanford/CS477/papers/Kuhn-SSR-2ndEd.pdf). There were many years since 1950 when there was no significant progress. AI scientists call these times [AI Winters](https://en.wikipedia.org/wiki/AI_winter).

<span class="marginnote">
    AI over years. [Source](https://towardsdatascience.com/history-of-the-first-ai-winter-6f8c2186f80b).
</span>
<img src="/assets/images/nvidia-arm/ai-history.png">

Latest deep learning revolution was caused by the availability of

1. Huge amount of data
2. Huge amount of compute to deal with that data
3. Highly open research and open source code

As computers pervaded our world, huge amounts of data were collected. Everybody should have heard the maxim [*Data is the new oil*](https://www.wired.com/insights/2014/07/data-new-oil-digital-economy/). [ImageNet](https://en.wikipedia.org/wiki/ImageNet), a database of 14 million images each with a description of what's in the image, was very important for AI being where it is now. There were lot more open datasets like ImageNet that led AI revolution. These datasets were available to the most researchers and [competitions](https://en.wikipedia.org/wiki/ImageNet#History_of_the_ImageNet_challenge) were held between them to test out their methods.

GPUs were critical to handle this data as CPUs failed to provide amount of compute required. [Moore's law](https://en.wikipedia.org/wiki/Moore%27s_law), which propounds exponential increase in compute power, stopped working for CPUs in about 2010s. GPUs, originally designed for gaming, provided an alternative to continue the Moore's law. They fit amount of computing power what was possible only with a ultra-expensive super computer. GPUs essentially democratized the super computers which were previously accessible to a select few.

Finally, openness of the latest AI revolution has allowed researchers around the world to work together and generate explosive amount of research. AI researchers have [rejected](https://arxiv.org/help/stats/2018_by_area) traditional journals which guard the research behind paywalls. They also open source the code (i.e. methods) reproducing their research so that others can build upon them. This is a dream come true for any researcher - computer science or not.

### Nvidia

Now that we understood the importance of GPUs for AI, it's time for us to examine the rise of Nvidia.

Nvidia was and is the leading manufacturer of GPUs, even before the rise of AI. Not only in terms of hardware, it has been a key partner in the rise of AI for software too. 