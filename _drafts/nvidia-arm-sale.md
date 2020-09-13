---
layout: post
title: Nvidia's ARM acquisition will start AI winter 
author: Sasank Chilamkurthy
---

It [looks like](https://www.theverge.com/2020/9/12/21434110/softbank-may-sell-arm-to-nvidia) Nvidia is closing in on deal with SoftBank to buy ARM for more than $40 billion. [Rumors and news](https://news.ycombinator.com/item?id=24009177) about the acquisition have been going around since at least August.
[Everybody](https://www.ft.com/content/4970848d-7821-45dc-b8cb-211036be5d30) is [concerned](https://tech.newstatesman.com/business/hermann-hauser-nvidia-destroy-arm) [about](https://ownsnap.com/us-government-should-block-nvidia-from-acquiring-arm-open-source-is-in-danger-zone/) the [acquisition](https://ownsnap.com/us-government-should-block-nvidia-from-acquiring-arm-open-source-is-in-danger-zone/) [including](https://www.telegraph.co.uk/technology/2020/08/10/arm-sale-softbank-tragedy-tech-sector-says-former-city-minister/) [ARM's](https://www.bbc.com/news/technology-53678506) [co-founders](https://hothardware.com/news/arm-co-founders-question-nvidias-motives-softbank-deal) [themselves]((https://phonemantra.com/arm-co-founders-fear-nvidia-takeover/)).
If so much has already been said about this, what's the point of this post? This post aims to add artificial intelligence (AI) to the conversation.
To understand this point, we have to step back a bit and examine the history and business models of key players. I aim to keep the conversation accessible to someone unacquainted with technology and hence the length.

## Background: History and Business of AI

It's important to understand the history of AI research in detail before we appreciate the reasons for rise of Nvidia. We'll then continue with business models of ARM and finally end with SoftBank.

### Artificial Intelligence (AI)

There's no denying the pervasiveness of AI in the modern world. AI is everywhere:

* In your pocket. Ever used Uber? [Uber uses AI](https://eng.uber.com/uber-ai-blog-2019/) to match you to drivers. Google assistant and Siri are obviously AI.
* In your living room. It was only in [science fiction](https://www.youtube.com/watch?v=1ZXugicgn6U), until recently, [where you can say aloud](https://www.amazon.in/gp/help/customer/display.html?nodeId=GNQ59GMNBGBU3U8L) *Alexa, play Pink Floyd* and *Comfortably Numb* plays in background.
* In your office. When you write a mail, have you received suggestions from gmail about your next sentence or phrase - as if [Google knows](https://www.techspot.com/news/74533-new-ai-powered-gmail-feature-can-write-emails.html) what you're about to type. 
* In your hospital. If you end up having a stroke, your brain [might be saved](https://info.vrad.com/hubfs/CaseStudy_AI_GreaterRegional_1119%20(002).pdf?__hstc=&__hssc=&hsCtaTracking=e9be34c0-46d3-45e1-90d8-b8153c20804e%7C7abcaae7-6366-4ff0-ad77-834471104fe7) by AI's brain (!) without you ever knowing. AI is enabling faster treatment for strokes and better management of [diseases](https://arxiv.org/abs/2006.05509) and [even pandemics](https://www.technologyreview.com/2020/04/23/1000410/ai-triage-covid-19-patients-health-care/).

These advances are a result of hard work of some of the brightest minds of the world. Alan Turing, the patron saint for computers, wrote a [seminal paper](https://en.wikipedia.org/wiki/Computing_Machinery_and_Intelligence) on AI in 1950 even before transistors were invented and computers were a thing. Many preeminent computer scientists worked on AI as evidenced by [Turing Awards](https://en.wikipedia.org/wiki/Turing_Award) for them. More recently, two Canadian and a French researchers led the deep learning revolution which enabled all the above innovations.

These advances in computer science were not gradual - [science doesn't work that way]((https://www.lri.fr/~mbl/Stanford/CS477/papers/Kuhn-SSR-2ndEd.pdf)). Science instead works in cycles of revolutions and setbacks - [hype cycle](https://en.wikipedia.org/wiki/Hype_cycle) is another term for this. There were many years since 1950 when there was no significant progress in AI. AI scientists call these times [AI Winters](https://en.wikipedia.org/wiki/AI_winter).

<span class="marginnote">
    AI over years. [Source](https://towardsdatascience.com/history-of-the-first-ai-winter-6f8c2186f80b).
</span>
<img src="/assets/images/nvidia-arm/ai-history.png">

Latest deep learning revolution was caused by the availability of

1. Huge amount of data
2. Huge amount of compute to deal with that data
3. Highly open research and open source code

As computers pervaded our world starting 90s, huge amounts of data were collected. You should have heard the maxim [*Data is the new oil*](https://www.wired.com/insights/2014/07/data-new-oil-digital-economy/). [ImageNet](https://en.wikipedia.org/wiki/ImageNet), a database of 14 million images each with a description of what's in the image, was very important for AI being where it is now. There were lot more open datasets like ImageNet that led AI revolution. These datasets were available to the most researchers and [competitions](https://en.wikipedia.org/wiki/ImageNet#History_of_the_ImageNet_challenge) were held between them to test out their methods.

GPUs were critical to handle this data as CPUs failed to provide amount of compute required. [Moore's law](https://en.wikipedia.org/wiki/Moore%27s_law), which propounds exponential increase in compute power, stopped working for CPUs in about 2010s. GPUs, originally designed for gaming, provided an alternative to continue the Moore's law. They fit amount of computing power what was possible only with a ultra-expensive super computer. GPUs essentially democratized the super computers which were previously accessible to a select few.

Finally, openness of the latest AI revolution has allowed researchers around the world to work together and generate explosive amount of research. AI researchers have [rejected](https://arxiv.org/help/stats/2018_by_area) traditional journals which guard the research behind paywalls <span id="opensciene" class="margin-toggle sidenote-number"></span>.<span class="sidenote">This is a topic for long form discussion in itself. Highly recommend [this paper from 2005](https://journals.uic.edu/ojs/index.php/fm/article/view/1265). Lot more cool stuff happened since publication of this paper</span>
They also open source the code (i.e. methods) reproducing their research so that others can build upon them. This is a dream come true for any researcher - computer science or not.

### Nvidia and AI

Now that we understood the importance of GPUs for AI, it's time for us to examine the rise of Nvidia. Nvidia was and is the leading manufacturer of GPUs, even before the rise of AI. Not only in terms of hardware, it has been a key partner in the AI revolution for software too.

Although GPUs store a lot of computing power, they were especially hard to program. Gaming industry recognized this and converged on a common library of tools called [OpenGL](https://en.wikipedia.org/wiki/OpenGL). However, none of these tools were useful to AI researchers because they're all about graphics while AI is about math. This is where Nvidia excelled - they provided this software library called [CUDA](https://en.wikipedia.org/wiki/CUDA) which made coding up math on Nvidia GPUs easy.

In 2012, a grad student from University of Toronto, Alex Krizhevsky (of [AlexNet Fame](https://papers.nips.cc/paper/4824-imagenet-classification-with-deep-convolutional-neural-networks.pdf)) used CUDA to create the first real Deep Learning model on the ImageNet dataset. Although deep learning ideas weren’t exactly new then, they never really caught on because of the lack of computation power required for them to work. CUDA allowed Alex to tap into the computation prowess which was originally designed for gaming. This paper turned out to be a game changer for AI and started the current revolution.

Nvidia, therefore, was a key contributor to the latest AI revolution. It became apparent soon after that there's a lot of money in AI. This, of course, did not go unnoticed by the market. Nvidia's stock price skyrocketed starting 2015.

<figure class="fullwidth">
<amp-img width="3342" height="1652" layout="responsive" src="/assets/images/nvidia-arm/nvidia-stock.png"></amp-img>
<span class="marginnote">
    Nvidia's stock price. [Source](https://finance.yahoo.com/chart/NVDA#eyJpbnRlcnZhbCI6Im1vbnRoIiwicGVyaW9kaWNpdHkiOjEsImNhbmRsZVdpZHRoIjo5LjYxNjM1MjIwMTI1Nzg2MSwiZmxpcHBlZCI6ZmFsc2UsInZvbHVtZVVuZGVybGF5Ijp0cnVlLCJhZGoiOnRydWUsImNyb3NzaGFpciI6dHJ1ZSwiY2hhcnRUeXBlIjoibW91bnRhaW4iLCJleHRlbmRlZCI6ZmFsc2UsIm1hcmtldFNlc3Npb25zIjp7fSwiYWdncmVnYXRpb25UeXBlIjoib2hsYyIsImNoYXJ0U2NhbGUiOiJsb2ciLCJwYW5lbHMiOnsiY2hhcnQiOnsicGVyY2VudCI6MSwiZGlzcGxheSI6Ik5WREEiLCJjaGFydE5hbWUiOiJjaGFydCIsImluZGV4IjowLCJ5QXhpcyI6eyJuYW1lIjoiY2hhcnQiLCJwb3NpdGlvbiI6bnVsbH0sInlheGlzTEhTIjpbXSwieWF4aXNSSFMiOlsiY2hhcnQiLCLigIx2b2wgdW5kcuKAjCJdfX0sInNldFNwYW4iOm51bGwsImxpbmVXaWR0aCI6Miwic3RyaXBlZEJhY2tncm91bmQiOnRydWUsImV2ZW50cyI6dHJ1ZSwiY29sb3IiOiIjMDA4MWYyIiwic3RyaXBlZEJhY2tncm91ZCI6dHJ1ZSwicmFuZ2UiOm51bGwsImV2ZW50TWFwIjp7ImNvcnBvcmF0ZSI6eyJkaXZzIjp0cnVlLCJzcGxpdHMiOnRydWV9LCJzaWdEZXYiOnt9fSwiY3VzdG9tUmFuZ2UiOm51bGwsInN5bWJvbHMiOlt7InN5bWJvbCI6Ik5WREEiLCJzeW1ib2xPYmplY3QiOnsic3ltYm9sIjoiTlZEQSIsInF1b3RlVHlwZSI6IkVRVUlUWSIsImV4Y2hhbmdlVGltZVpvbmUiOiJBbWVyaWNhL05ld19Zb3JrIn0sInBlcmlvZGljaXR5IjoxLCJpbnRlcnZhbCI6Im1vbnRoIiwic2V0U3BhbiI6bnVsbH1dLCJzdHVkaWVzIjp7IuKAjHZvbCB1bmRy4oCMIjp7InR5cGUiOiJ2b2wgdW5kciIsImlucHV0cyI6eyJpZCI6IuKAjHZvbCB1bmRy4oCMIiwiZGlzcGxheSI6IuKAjHZvbCB1bmRy4oCMIn0sIm91dHB1dHMiOnsiVXAgVm9sdW1lIjoicmdiYSgyMDAsIDI0MCwgMjIwLCAwLjgpIiwiRG93biBWb2x1bWUiOiJyZ2JhKDI1NSwgNDgsIDYwLCAwLjgpIn0sInBhbmVsIjoiY2hhcnQiLCJwYXJhbWV0ZXJzIjp7IndpZHRoRmFjdG9yIjowLjQ1LCJjaGFydE5hbWUiOiJjaGFydCIsInBhbmVsTmFtZSI6ImNoYXJ0In19fX0-): Yahoo Finance
</span>
</figure>

GPU hardware is not exclusive to Nvidia though - gaming existed long before there was any AI. AMD, who acquired [ATI](https://en.wikipedia.org/wiki/Radeon) and a key leader in CPU market, is also a key competitor in GPU hardware. Latest and greatest gaming consoles - both Microsoft's Xbox and Sony's PlayStation - use AMD's GPUs.

However, GPU AI software (CUDA) works only on Nvidia GPUs. This meant AI is *exclusive* to Nvidia GPUs. CUDA became a strong moot for Nvidia's commercial traction. Basically there's no competition for hardware for AI.
This monopoly of AI GPU hardware is quite apparent in the server market<span id="tps" class="margin-toggle sidenote-number"></span>.<span class="sidenote">
Google has developed [TPUs](https://en.wikipedia.org/wiki/Tensor_processing_unit) as an alternative to GPUs. They're [available](https://cloud.google.com/tpu) on Google cloud but they're quite expensive. You can't really get *hands* on them.
<br/>
Most AI researchers, like myself, use consumer grade Nvidia GPUs for research. These are cheaper thanks to the competition from AMD in Gaming market.
</span>
Cloud providers like AWS, who power most of our web, offer no alternative to Nvidia for GPUs. Nvidia is able to charge significant markup for server-grade GPUs because of lack of competition. All of this is a great news for shareholders for Nvidia but is not so great for AI researchers and engineers. 

Nvidia's commercial interests mean that CUDA will never work on Nvidia's competition GPUs. This makes Nvidia fiercely anti-market, like any monopoly. This is also the reason why Nvidia resisted integration of GPUs to Open Source Linux operating system.  In 2012, [Linus Torvalds](https://en.wikipedia.org/wiki/Linus_Torvalds), the maintainer of Linux<span id="linus" class="margin-toggle sidenote-number"></span><span class="sidenote">
and the patron saint of open source movement
</span> famously called Nvidia the [single worst company](https://www.wired.com/2012/06/torvalds-nvidia-linux/) Linux community dealt with and publicly said [*Nvidia, F**k you*](https://www.youtube.com/watch?v=iYWzMvlj2RQ).

Nvidia's consolidation of AI is nowhere at the end. It's aggressively pushing into embedded devices<span id="jetson" class="margin-toggle sidenote-number"></span><span class="sidenote">
with its [Jetson line of products](https://en.wikipedia.org/wiki/Nvidia_Jetson)
</span> and phones. This is the reason why it would want to acquire ARM. Now it's the time to look into what ARM does and how it fits into the equation.

### ARM and Computer Architecture