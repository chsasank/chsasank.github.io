---
layout: post
title: A Quest to Become Well Rounded Computer Scientist
author: Sasank Chilamkurthy
twitter_image: https://chsasank.github.io/assets/images/internship/thumbnail.png
---

## Present: AI @ Qure.ai

Right after the college, I joined a hedge fund called WorldQuant in June 2015. I expected some cool math being used for finance - but the reality was completely different. So, I quit in less than three months. I joined Housing.com, the first offer that came my way, as an NLP data scientist. The work was awesome, but the startup ship was sinking then. So, I had to quit Housing too.

I was looking for jobs and somehow I met Prashant, PhD from GATech and successful startup founder. He was looking to start this cool company which does AI for medical imaging. My friend and roommate, Preetham, also chatted with Prashant and decided to join the startup. I had a bit of hesitation about joining another startup but Preetham convinced me. The knowledge that Pooja, a doctor, PhD and data scientist is another co-founder also helped. So, we both ended up joining the startup as first employees on February 1st 2016.

What a fine time it was! We were reading deep learning papers and implementing them on our datasets. And I was working with friends and great mentors. What more can you ask? For the first 6 months, we had little focus -- I worked on pathology while Preetham worked on genomics. We had to name our startup something and we stuck with qure.ai because cure.ai domain was not available. Eventually, we decided to focus on radiology because that's the most 'digitized' of all the health fields.

Once we picked up focused problems, our original research went up. Prashant, with his prior experience of a data scientist, knew the importance of data. He worked a lot of acquiring data and we ended up getting about 100 TB of data to train our models on. Soon enough, not only were we adopting AI published before but were actively modifying them for our use cases. Medical images are quite different from natural images. So, the published research on ImageNet and other datasets was not directly applicable to our medical imaging datasets.

Not only were we doing AI research and development, we ended up doing lot of engineering and clinical research alongside. We had to work with DICOM and PACS, key technologies in the radiology. These technologies are very different from the internet technologies like HTTP and stuff. We built a dicom database which could handle TBs of data without sweat. Preetham and I also worked a lot on setting up the compute infrastructure (hardware, GPUs, large scale storage systems etc.) basically building a private cloud. 

Because our AI needs to be in clinical workflow, we work on clinical research too. We did research quantifying the accuracy of AI versus radiologists. We did studies to quantify the amount of time we saved as doctors used our algorithms. We end up publishing this research on top clinical journals. My favorite work was the Lancet paper where we described development and validation of qER.

The AI field itself was new when we started up. There were lot of gaps in tools we use like PyTorch, Keras and SimpleITK. So, we also ended up contributing a lot to open source technologies. I was quite fortunate to have involved in early PyTorch's development. It didn't seem so obvious then but in retrospect, it's awesome that a noob like me could contribute to the behemoth that is PyTorch. It shows you the openness and effectiveness of open source technologies.

## Future: Systems @ Qure.ai

I continue to do cool stuff at Qure.ai. I have gained a lot of experience doing AI at qure. However, I have recently decided to do something other than core AI research for sometime and concentrate on software systems. Why is that, you may ask? Two reasons.

Firstly from a research point of view, aim for most of the people in AI is human level intelligence AKA artificial general intelligence (AGI). I'm convinced that AGI is possible only when AI is completely integrated with the systems research. There are two distinct parts of human cognition -- perception and logic. Deep learning has ended up solving perception problems like computer vision and speech recognition quite well. However the same deep learning is not able to do much logical thinking.

This 'logical thinking' is the domain of much of the computer science sans AI -- databases, programming languages, OS, theory etc. If we need AGI, AI has to be integrated into these systems. May be with the power of AI, we will be able to create compiler which lets us give instructions to computer in ambiguous natural language rather than terse programming languages. May be databases will know what we *want* and convert our natural language queries to SQL accurately. To do something cool like this, I need to understand how exactly different software systems work.

Secondly from a technology point of view, computers are the next big thing that happened to technology after printing. Computers  transformed our lives within such a short span of time. First real computers were made during World War II and they were not even based on Silicon. Silicon based micro processors started coming up only in late 60s and 70s. Internet showed up in 90s. Computer science is a young science and is less than 70 years old. Yet it has achieved so much.

What such a young science gives me is an opportunity to be a *renaissance man*. The idea of renaissance man is that a person could be an expert in *everything* that is known. Since the knowledge was being actively discovered or created during renaissance period, this was not impractical. Computer science, being so young and with so much unknown, offers me such an opportunity where I can know *almost everything* about it.

How do I go about all this? I want a birdseye view of computer science research. What better place to start than Nobel prize for computer science, Turing Award? I will read the landmark papers of each of the award winners. Turing award lectures is a great place to understand the work of a pioneer. Another great resource I found is the [list of important publications in computer science](https://en.wikipedia.org/wiki/List_of_important_publications_in_computer_science) on wikipedia.

Of course, there's an alternate and more obvious way -- reading textbooks. But I prefer papers over textbooks because of many reasons:
1. Papers are primary sources. They are subjected to higher peer review standards. Citations are a good way to judge the impact of a paper.
2. They are short and sweet. Unlike a textbook, I should be able to complete the paper in one or two sittings.
3. Authors have more skin in the game because they are presenting their ideas. So, papers are almost necessarily opinionated, which make them entertaining.

I have already started maintaining mirrors of a few classic papers. I will continue digitizing<label for="sn-1" class="margin-toggle sidenote-number"></label><input type="checkbox" id="sn-1" class="margin-toggle"/>
<span class="sidenote">
for a lack of better word for making content web ready.
</span> the landmark papers and add my highlights/notes to them. I will also transcribe Turing lectures into readable articles.

At work, I am doing more engineering with a focus on deployability, reliability and scalability of AI. I am finding React, progressive web apps and node.js super interesting. Concurrent and parallel programming is important for running intensive AI tasks. I plan to do more systems programming around networking and GPU hardware. I'll try to approach the engineering problems I face in a more research-oriented manner just like how I did for AI previously.
