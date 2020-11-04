---
layout: post
title: A Quest to Become Well Rounded Computer Scientist
author: Sasank Chilamkurthy
twitter_image: https://chsasank.github.io/assets/images/internship/thumbnail.png
---

*I graduated from electrical engineering*. This is a deep founded insecurity
for me. I have worked and did research on artificial intelligence, software
engineering and computers in general for around five years now. Yet I
couldn't shake off this insecurity. It always gnaws me somewhere that I
don't know how exactly compilers, databases or operating systems work. I am
keenly aware of this lack of knowledge in these areas of computer science.

I'll explore my life in college, my work after college and what future holds
for me.

## Prelude

I have to start with *how* I chose electrical engineering in the first place and
narrate the story from high school. I worked hard in the school and ended up
being 28th from the top in this standardized test called IIT-JEE. Using this
*rank*, you have to compete with other students in choosing the school you
want to go to. You also have to chose your major before you joined the
school. My good scores in the test allowed me to chose whatever I want. IIT
Bombay is where all my friends are going to, so that was no brainer. But,
for some random reason, I chose electrical engineering (EE) as my major.

I joined IIT Bombay (IITB) in June 2011<label for="sn-1" class="margin-toggle
sidenote-number"></label><input type="checkbox" id="sn-1"
class="margin-toggle"/> <span class="sidenote"> It was a rainy day. It was
always raining those days. I recall taking a photo when I saw Sun finally
after months.</span>. In the first year at IITB, students from all the
majors go through the almost same courses. These courses tend to be math,
basic sciences, an introduction to programming, and a major specific
introduction. EE, being an exception, had two introduction courses. At the
end of the first year, based on your GPA, you'll have a chance to switch
majors. 

In the first semester, I hated  EE 101 course on power, motors and stuff and
doubted the choice of my major. In the second semester, however, I loved the
EE 111 course which was digital electronics and micro processors. In the
same semester, CS 101 -- intro to computer science took place. The professor
crammed a lot of material into that course knowing that we will not do
another CS course. I ended up hating this course because it was so
difficult! Given that I loved EE 111 and hated CS 101, I decided to stick
with EE. At the end of the semester, I had fairly high GPA to allow me to
switch to whatever major I want and I didn't use the opportunity.

You gotta give it to the course designers at EE and IITB. The philosophy
tends to be that there should only a few *core courses* that everybody must
do. Students are instead *encouraged* to do *minors* in other departments
and *honours* in the same department if you want to specialize. You can take
grad-level courses too if you meet the prerequisites and sometimes,
blessings of the professor. Core course load is kept to minimum so that you
can pursue whatever you want. In the final year, you don't have any core
courses whatsoever and you have to instead do electives -- both inside and
outside your major. 

This made sure my next three years were fairly awesome. I loved math so I did
a minor in Mathematics. I loved signal processing and communication courses
because they involved a lot of Fourier transform and math. I also discovered
information theory in our intro to probability course and ended up doing
three follow up courses. I did a course on processor design and ended up
coding up a ARM7 CPU. I did some courses on quantum physics and quantum
computing because, you guessed it, math! 

<figure>
<label for="mn-fig-1" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-1" class="margin-toggle">
<span class="marginnote">A tutorial from the time apparently. I have no recollection of having done this much math :)</span>
<img src="assets/images/my_notes.png">
</figure>

At the end of my third year, my friend, [Sai Bhargav](https://www.linkedin.com/in/sai-bhargav-yalamanchi/), introduced me to *Machine Learning* (ML). He was doing that course at the time and I was embarrassed to ask what the hell the course was about. Machine learning has math and did something cool with that math -- what not to like! And turns out, I did all the basic courses to start a career in ML -- probability, information theory, linear algebra etc. My internship also made it clear that EE research is not for me. It started becoming clear that machine learning is my future.

While at IITB, I always wanted to go to grad school and do a PhD. You usually apply for grad school after your third year. I wanted to do a PhD in CS department on machine learning or computer vision. The competition for ML PhD was quite high even in those days. And look at me: an EE guy with no course work either on basic computer science or machine learning trying to apply for a ML PhD. All my research work so far had been on signal processing and optical fibers. So yeah, the grad school applications didn't work out. It also didn't help that my writing skills were not mature. You could consider this post a *Statement of Purpose* and my chances would've been probably better.

I anyways continued with computer science and machine learning courses. In my fourth and final year at IITB, I did almost exclusively computer science and related courses. EE guys, in their wisdom, considered CS courses as in-major elective. So I could do courses like graph theory, computer vision and medical imaging as EE electives. I did a research project on communications which had lot of math, but I could not enjoy it. ML-fication of me is complete. 

## Present

Right after the college, I joined a hedge fund called WorldQuant in June 2015. I expected some cool math being used for finance - but the reality was completely different. So, I quit in less than three months. I joined Housing.com, the first offer that came my way, as an NLP data scientist. The work was awesome, but the startup ship was sinking then. So, I had to quit Housing too.

I was looking for jobs and somehow I met Prashant, PhD from GATech and successful startup founder. He was looking to start this cool company which does AI for medical imaging. My friend and roommate, Preetham, also chatted with Prashant and decided to join the startup. I had a bit of hesitation about joining another startup but Preetham convinced me. The knowledge that Pooja, a doctor, PhD and data scientist is another co-founder also helped. So, we both ended up joining the startup as first employees on February 1st 2016.

What a fine time it was! We were reading deep learning papers and implementing them on our datasets. And I was working with friends and great mentors. What more can you ask? For the first 6 months, we had little focus -- I worked on pathology while Preetham worked on genomics. We had to name our startup something and we stuck with qure.ai because cure.ai domain was not available. Eventually, we decided to focus on radiology because that's the most 'digitized' of all the health fields.

Once we picked up focused problems, our original research went up. Prashant, with his prior experience of a data scientist, knew the importance of data. He worked a lot of acquiring data and we ended up getting about 100 TB of data to train our models on. Soon enough, not only were we adopting AI published before but were actively modifying them for our use cases. Medical images are quite different from natural images. So, the published research on ImageNet and other datasets was not directly applicable to our medical imaging datasets.

Not only were we doing AI research and development, we ended up doing lot of engineering and clinical research alongside. We had to work with DICOM and PACS, key technologies in the radiology. These technologies are very different from the internet technologies like HTTP and stuff. We built a database 

Preetham and I also worked a lot on setting up the compute infrastructure (hardware, GPUs, large scale storage systems etc.) basically building a private cloud. 

Because our AI needs to be in clinical workflow, we had to work on core clinical research too. 

My proudest personal moments are the Lancet paper on qER and my work on PyTorch.

## Future

I continue to do cool stuff at Qure.ai. However, I have recently decided to do something other than core AI research for sometime. Why is that you may ask? Many reasons.

Firstly from a research point of view, aim for most of the people in AI is human level intelligence AKA artificial general intelligence (AGI).

I'm convinced that artificial general intelligence is possible only when AI is completely integrated into the prior systems research. 

Renaissance man.

