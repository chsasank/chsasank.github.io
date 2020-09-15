---
layout: post
title: Nvidia's ARM Acquisition Explained
author: Sasank Chilamkurthy
---

Nvidia has [announced](https://www.theverge.com/2020/9/13/21435507/nvidia-acquiring-arm-40-billion-chips-ai-deal) that it is buying ARM for more than $40 billion. [Rumors and news](https://news.ycombinator.com/item?id=24009177) about the acquisition have been going around since at least August but is confirmed by Nvidia in September.
[Everybody](https://www.ft.com/content/4970848d-7821-45dc-b8cb-211036be5d30) is [concerned](https://tech.newstatesman.com/business/hermann-hauser-nvidia-destroy-arm) [about](https://ownsnap.com/us-government-should-block-nvidia-from-acquiring-arm-open-source-is-in-danger-zone/) the [acquisition](https://ownsnap.com/us-government-should-block-nvidia-from-acquiring-arm-open-source-is-in-danger-zone/) [including](https://www.telegraph.co.uk/technology/2020/08/10/arm-sale-softbank-tragedy-tech-sector-says-former-city-minister/) [ARM's](https://www.bbc.com/news/technology-53678506) [co-founders](https://hothardware.com/news/arm-co-founders-question-nvidias-motives-softbank-deal) [themselves]((https://phonemantra.com/arm-co-founders-fear-nvidia-takeover/)).
If so much has already been said about this, what's the point of this post? This post aims to add artificial intelligence (AI) to the conversation.
To understand this point, we have to step back a bit and examine the history and business models of key players and technologies. I aim to keep the conversation accessible to someone unacquainted with technology and hence the length.

Contents:
1. Background: History and Business of AI and Computers
2. Why ARM’s Cofounders are Against the Acquisition
3. What About AI?
4. What are the Alternatives?
5. Conclusion

## Background: History and Business of AI and Computers

It's important to understand the history of AI research in detail before we appreciate the reasons for the rise of Nvidia.
We, then, also have to dig deep into the history of computers themselves.
We'll then continue with ARM and the reason why ARM is up for sale.

### Artificial Intelligence (AI)

There's no denying the pervasiveness of AI in the modern world. AI is everywhere:

* In your pocket. Ever used Uber? [Uber uses AI](https://eng.uber.com/uber-ai-blog-2019/) to match you to drivers. Google assistant and Siri are obviously AI.
* In your living room. It was only in [science fiction](https://www.youtube.com/watch?v=1ZXugicgn6U), until recently, [that you can say aloud](https://www.amazon.in/gp/help/customer/display.html?nodeId=GNQ59GMNBGBU3U8L) *Alexa, play Pink Floyd* and *Comfortably Numb* plays in the background.
* In your office. When you wrote a mail in Gmail, have you received suggestions for your next sentence or phrase - as if [Google knows](https://www.techspot.com/news/74533-new-ai-powered-gmail-feature-can-write-emails.html) what you're about to type?
* In your hospital. If you end up having a stroke, your brain [might be saved](https://info.vrad.com/hubfs/CaseStudy_AI_GreaterRegional_1119%20(002).pdf?__hstc=&__hssc=&hsCtaTracking=e9be34c0-46d3-45e1-90d8-b8153c20804e%7C7abcaae7-6366-4ff0-ad77-834471104fe7) by AI's brain (!) without you ever knowing. AI is enabling faster treatment for strokes and better management of [diseases](https://arxiv.org/abs/2006.05509) and [even pandemics](https://www.technologyreview.com/2020/04/23/1000410/ai-triage-covid-19-patients-health-care/).

These advances are a result of hard work of some of the brightest minds of the world. Alan Turing, the patron saint for computers, wrote a [seminal paper](https://en.wikipedia.org/wiki/Computing_Machinery_and_Intelligence) on AI in 1950 even before transistor based computers were a thing. Many preeminent computer scientists worked on AI as evidenced by [Turing Awards](https://en.wikipedia.org/wiki/Turing_Award) for them. More recently, three researchers, two Canadian and a French, led the deep learning revolution which enabled all the above innovations.

These advances in computer science were not gradual - [science doesn't work that way]((https://www.lri.fr/~mbl/Stanford/CS477/papers/Kuhn-SSR-2ndEd.pdf)). Science instead works in cycles of revolutions and setbacks<span id="hype" class="margin-toggle sidenote-number"></span>.
<span class="sidenote">
[Hype cycle](https://en.wikipedia.org/wiki/Hype_cycle) is another term for this
</span> There were many years since 1950 when there was no significant progress in AI. AI scientists call these times [AI Winters](https://en.wikipedia.org/wiki/AI_winter).

<span class="marginnote">
    AI over years. [Source](https://towardsdatascience.com/history-of-the-first-ai-winter-6f8c2186f80b).
</span>
<img src="/assets/images/nvidia-arm/ai-history.png">

Latest deep learning revolution of AI was caused by the availability of

1. Huge amount of data
2. Huge amount of compute to deal with that data
3. Highly open research and open source code

As computers pervaded our world starting 90s, huge amounts of data were collected. All of us have heard the maxim [*Data is the new oil*](https://www.wired.com/insights/2014/07/data-new-oil-digital-economy/). 
In 2009, researchers from Stanford released [ImageNet](https://en.wikipedia.org/wiki/ImageNet), a database of 14 million images each with a description of what's in the image. This large dataset allowed AI to learn *how to see* by example. Concurrently large datasets were created for translation, speech recognition and other cognitive areas. Imagenet and other datasets were available to the most researchers around the world and [competitions](https://en.wikipedia.org/wiki/ImageNet#History_of_the_ImageNet_challenge) were held between them to test out their methods.

GPUs were critical to handle this data as CPUs failed to provide the amount of compute required. [Moore's law](https://en.wikipedia.org/wiki/Moore%27s_law), which propounds exponential increase in compute power, stopped working for CPUs in 2010s. GPUs, originally designed for gaming, provided an alternative to continue Moore's law. They fit an amount of computing power that was possible only with an ultra-expensive supercomputer. GPUs essentially democratized the super computers which were previously accessible only to a select few.

Finally, the openness of the latest AI revolution has allowed researchers around the world to work together and generate an explosive amount of research. AI researchers have [rejected](https://arxiv.org/help/stats/2018_by_area) traditional journals which guard the research behind paywalls <span id="opensciene" class="margin-toggle sidenote-number"></span>.<span class="sidenote">This is a topic for long form discussion in itself. Highly recommend [this paper from 2005](https://journals.uic.edu/ojs/index.php/fm/article/view/1265). Lot more cool stuff happened since publication of this paper</span>
They also open source the code (i.e. methods) reproducing their research so that others can build upon them. This is a dream come true for any researcher - computer science or not.

### GPUs and Nvidia

Now that we understood the importance of GPUs for AI, it's time for us to examine the rise of Nvidia. Nvidia was and is the leading manufacturer of GPUs, even before the rise of AI. Not only in terms of hardware, it has been a key partner in the AI revolution for software too.

Although GPUs store a lot of computing power, they were especially hard to program. Gaming industry recognized this and converged on a common library of tools called [OpenGL](https://en.wikipedia.org/wiki/OpenGL). However, none of these tools were useful to AI researchers because they're all about graphics while AI is about math. This is where Nvidia excelled - they provided this software library called [CUDA](https://en.wikipedia.org/wiki/CUDA) which made coding up math on Nvidia GPUs easy.

In 2012, a grad student from University of Toronto, Alex Krizhevsky (of [AlexNet Fame](https://papers.nips.cc/paper/4824-imagenet-classification-with-deep-convolutional-neural-networks.pdf)) used CUDA to create the first real Deep Learning model on the ImageNet dataset. Although deep learning ideas weren’t exactly new then, they never really caught on because of the lack of computation power required for them to work. CUDA allowed Alex to tap into the computation prowess which was originally designed for gaming. This paper turned out to be a game changer for AI and started the current revolution.

With GPUs and its CUDA software, NVIDIA then became a key contributor to the latest AI revolution. It soon became very apparent that there is quite a big business opportunity in AI for these companies.
This, of course, did not go unnoticed by the market. Nvidia's stock price skyrocketed starting 2015. Since 2012, Nvidia's stock price has grown 3500% (!) compared to 380% of NASDAQ composite. 

<figure class="fullwidth">
<amp-img width="3342" height="1652" layout="responsive" src="/assets/images/nvidia-arm/nvidia-stock.png"></amp-img>
<span class="marginnote">
    Nvidia's stock price. [Source](https://finance.yahoo.com/chart/NVDA#eyJpbnRlcnZhbCI6IndlZWsiLCJwZXJpb2RpY2l0eSI6MSwiY2FuZGxlV2lkdGgiOjMuNDg0MjM0MjM0MjM0MjM0LCJmbGlwcGVkIjpmYWxzZSwidm9sdW1lVW5kZXJsYXkiOnRydWUsImFkaiI6dHJ1ZSwiY3Jvc3NoYWlyIjp0cnVlLCJjaGFydFR5cGUiOiJtb3VudGFpbiIsImV4dGVuZGVkIjpmYWxzZSwibWFya2V0U2Vzc2lvbnMiOnt9LCJhZ2dyZWdhdGlvblR5cGUiOiJvaGxjIiwiY2hhcnRTY2FsZSI6InBlcmNlbnQiLCJwYW5lbHMiOnsiY2hhcnQiOnsicGVyY2VudCI6MSwiZGlzcGxheSI6Ik5WREEiLCJjaGFydE5hbWUiOiJjaGFydCIsImluZGV4IjowLCJ5QXhpcyI6eyJuYW1lIjoiY2hhcnQiLCJwb3NpdGlvbiI6bnVsbH0sInlheGlzTEhTIjpbXSwieWF4aXNSSFMiOlsiY2hhcnQiLCLigIx2b2wgdW5kcuKAjCJdfX0sInNldFNwYW4iOm51bGwsImxpbmVXaWR0aCI6Miwic3RyaXBlZEJhY2tncm91bmQiOnRydWUsImV2ZW50cyI6ZmFsc2UsImNvbG9yIjoiIzAwODFmMiIsInN0cmlwZWRCYWNrZ3JvdWQiOnRydWUsImV2ZW50TWFwIjp7ImNvcnBvcmF0ZSI6eyJkaXZzIjpmYWxzZSwic3BsaXRzIjpmYWxzZX0sInNpZ0RldiI6e319LCJjdXN0b21SYW5nZSI6bnVsbCwic3ltYm9scyI6W3sic3ltYm9sIjoiTlZEQSIsInN5bWJvbE9iamVjdCI6eyJzeW1ib2wiOiJOVkRBIiwicXVvdGVUeXBlIjoiRVFVSVRZIiwiZXhjaGFuZ2VUaW1lWm9uZSI6IkFtZXJpY2EvTmV3X1lvcmsifSwicGVyaW9kaWNpdHkiOjEsImludGVydmFsIjoid2VlayIsInRpbWVVbml0IjpudWxsLCJzZXRTcGFuIjpudWxsfSx7InN5bWJvbCI6Ik5EQVEiLCJzeW1ib2xPYmplY3QiOnsic3ltYm9sIjoiTkRBUSJ9LCJwZXJpb2RpY2l0eSI6MSwiaW50ZXJ2YWwiOiJ3ZWVrIiwidGltZVVuaXQiOm51bGwsInNldFNwYW4iOm51bGwsImlkIjoiTkRBUSIsInBhcmFtZXRlcnMiOnsiY29sb3IiOiIjNzJkM2ZmIiwid2lkdGgiOjQsImlzQ29tcGFyaXNvbiI6dHJ1ZSwic2hhcmVZQXhpcyI6dHJ1ZSwiY2hhcnROYW1lIjoiY2hhcnQiLCJzeW1ib2xPYmplY3QiOnsic3ltYm9sIjoiTkRBUSJ9LCJwYW5lbCI6ImNoYXJ0IiwiZmlsbEdhcHMiOmZhbHNlLCJhY3Rpb24iOiJhZGQtc2VyaWVzIiwic3ltYm9sIjoiTkRBUSIsImdhcERpc3BsYXlTdHlsZSI6InRyYW5zcGFyZW50IiwibmFtZSI6Ik5EQVEiLCJvdmVyQ2hhcnQiOnRydWUsInVzZUNoYXJ0TGVnZW5kIjp0cnVlLCJoZWlnaHRQZXJjZW50YWdlIjowLjcsIm9wYWNpdHkiOjEsImhpZ2hsaWdodGFibGUiOnRydWUsInR5cGUiOiJsaW5lIiwic3R5bGUiOiJzdHhfbGluZV9jaGFydCIsImhpZ2hsaWdodCI6dHJ1ZX19XSwic3R1ZGllcyI6eyLigIx2b2wgdW5kcuKAjCI6eyJ0eXBlIjoidm9sIHVuZHIiLCJpbnB1dHMiOnsiaWQiOiLigIx2b2wgdW5kcuKAjCIsImRpc3BsYXkiOiLigIx2b2wgdW5kcuKAjCJ9LCJvdXRwdXRzIjp7IlVwIFZvbHVtZSI6InJnYmEoMjAwLCAyNDAsIDIyMCwgMC44KSIsIkRvd24gVm9sdW1lIjoicmdiYSgyNTUsIDQ4LCA2MCwgMC44KSJ9LCJwYW5lbCI6ImNoYXJ0IiwicGFyYW1ldGVycyI6eyJ3aWR0aEZhY3RvciI6MC40NSwiY2hhcnROYW1lIjoiY2hhcnQiLCJwYW5lbE5hbWUiOiJjaGFydCJ9fX0sInRpbWVVbml0IjpudWxsLCJyYW5nZSI6bnVsbH0-): Yahoo Finance
</span>
</figure>

GPU hardware is not exclusive to Nvidia though - gaming existed long before there was any AI. AMD, who acquired [ATI](https://en.wikipedia.org/wiki/Radeon) and a key leader in CPU market, is also a key competitor in GPU hardware. Latest and greatest gaming consoles - both Microsoft's Xbox and Sony's PlayStation - use AMD's GPUs.

However, GPU AI software (CUDA) works only on Nvidia GPUs. This means AI is *exclusive* to Nvidia GPUs. CUDA became a strong moat for Nvidia's commercial traction. Basically there's no competition for hardware for AI.
This monopoly of AI GPU hardware is quite apparent in the server market<span id="tps" class="margin-toggle sidenote-number"></span>.<span class="sidenote">
Google has developed [TPUs](https://en.wikipedia.org/wiki/Tensor_processing_unit) as an alternative to GPUs. They're [available](https://cloud.google.com/tpu) on Google cloud but they're quite expensive. You can't really get *hands* on them.
<br/>
Most AI researchers, like myself, use consumer grade Nvidia GPUs for research. These are cheaper thanks to the competition from AMD in Gaming market.
</span>
Cloud providers like AWS, who power most of our web, offer no alternative to Nvidia for GPUs. Nvidia is able to charge significant markup for server-grade GPUs because of lack of competition. All of this is a great news for shareholders for Nvidia but not so great for AI researchers and engineers.

Nvidia's commercial interests mean that CUDA will never work on their competitor's GPUs. This is also the reason why Nvidia resisted integration of GPUs to Open Source Linux operating system.  In 2012, [Linus Torvalds](https://en.wikipedia.org/wiki/Linus_Torvalds), the maintainer of Linux<span id="linus" class="margin-toggle sidenote-number"></span><span class="sidenote">
and the patron saint of open source movement
</span> famously called Nvidia the [single worst company](https://www.wired.com/2012/06/torvalds-nvidia-linux/) Linux community dealt with and publicly said [*Nvidia, F**k you*](https://www.youtube.com/watch?v=iYWzMvlj2RQ).

Nvidia's consolidation of AI is nowhere at the end. It's aggressively pushing into embedded devices<span id="jetson" class="margin-toggle sidenote-number"></span><span class="sidenote">
with its [Jetson line of products](https://en.wikipedia.org/wiki/Nvidia_Jetson)
</span> and phones. This is the reason why it would want to acquire ARM. Now it's the time to look into what ARM does and how it fits into the equation.

### Computers and their Architecture

So far, we have discussed GPUs and AI. We have to digress a bit and discuss the history of computers themselves and computer architecture - how compute is organized *inside* the chips.

A key moment in the history of computers was the discovery of transistors in 1947 at Bell Labs. Transistors are these little circuits made out of semi-conductors<span id="semi-conductors" class="margin-toggle sidenote-number"></span><span class="sidenote">Semi conductors are chemical materials whose electrical properties fall between metals (think electrical wire) and insulators (think plastic around the wire).
</span> like silicon that allowed simulation of logic. Electrical circuits available until then<span id="vaccum tubes" class="margin-toggle sidenote-number"></span><span class="sidenote">[Vaccum tubes](https://en.wikipedia.org/wiki/Vacuum_tube) existed but they were too bulky.
</span> were what mathematicians would call linear and didn't allow for logical computation.
New kind of circuits with transistors started to be called *Electronics*.
Now you should have got why the computer industry is also called electronics or semi-conductor business and the reason behind the name of [silicon valley](https://en.wikipedia.org/wiki/Silicon_Valley).

Second type of transistors called [MOSFETs](https://en.wikipedia.org/wiki/MOSFET) were soon discovered in 1959, also at Bell Labs. Many of these transistors could be fit on one piece of silicon die - called [integrated circuits](https://en.wikipedia.org/wiki/Integrated_circuit) or *chips*. Intel (stood **Int**egrated **El**ectronics) was founded in 1968 to manufacture silicon chips.
It became obvious soon after that these chips could handle the logic we expected of computers - i.e chips are micro-processors aka CPUs! You use what are called [instruction sets](https://en.wikipedia.org/wiki/Instruction_set_architecture)<span class="margin-toggle sidenote-number"></span><span class="sidenote">Example instruction is to add two numbers A and B: `ADD A, B`
</span> to program these chips.

First true microprocessor, Intel 4004 was released in 1971. Computer architecture - organization of the transistors on the chips and instruction sets that are used to program them - evolved quickly. In 1978, Intel introduced landmark 8086 chip and followed up with 80186, 80286 etc chips over the years. These family of chips shared a similar architecture called [x86 architecture](https://en.wikipedia.org/wiki/X86). This architecture persists to today and is the base of chips found in almost all the desktops, laptops and servers<span class="margin-toggle sidenote-number"></span><span class="sidenote">Note the exclusion of smart phones!
</span> to program these chips.

x86 was not the only computer architecture available. Many of Intel's competitors including Motorola, IBM, Sun<span class="margin-toggle sidenote-number"></span><span class="sidenote">Note the exclusion of AMD, key competition to Intel. AMD uses x86 architecture and maintains compatibility with Intel processors.
</span> and some academic institutions created alternate architectures. Key distinctions between these architectures is complexity of the instructions. x86 instructions tend to be complex and do multiple things at once while the competition tends to follow keep it simple stupid ([KISS](https://en.wikipedia.org/wiki/KISS_principle)) principle. Academics believed the latter set to be better but they never really caught on until very recently!


### Smart Phones and ARM

The Stage is now set to understand what ARM does! Let's get a perspective on another key technology - mobiles aka smart phones.

<!-- Starting 90s, cellphones started appearing on the market allowing calls to be done from the pockets (2G). In 2000s, these phones started getting data / internet access (3G). Blackberry smartphones, now extinct breed were some of the first smartphones. In 2007, Apple released landmark smartphone [iPhone](https://en.wikipedia.org/wiki/IPhone_(1st_generation)) and in 2008, Google collaborated with phone manufacturers to release [Android](https://en.wikipedia.org/wiki/Android_(operating_system)) breed of smartphones. -->

Smartphones are essentially pocket sized computers and their compute power steadily grew over the years. Both iPhones and Androids used this curious new chips called ARM chips. ARM Holdings, the company behind the ARM, designed the chips and their architectures but does *not* actually manufacture them! ARM licenses its designs to manufacturers so that they can modify the designs and fabricate the chips<span class="margin-toggle sidenote-number"></span><span class="sidenote">This is a completely different business model! Also note how allowing people to edit designs is similar to open source!</span>. Curiously, ARM chips keep their instructions simple as opposed to x86's complex instructions - which academics always said was a better design. Key customers of ARM are Samsung, Qualcomm and Apple.

Unlike desktops and laptops, smart phones are small in size and their batteries have a limitation to how big they can be. Therefore, power consumption is a key consideration on why Apple and Google chose ARM chips as opposed to x86 chips. Intel and AMD [tried](https://en.wikipedia.org/wiki/Intel_Atom) to decrease power consumption for x86 chips but they were not successful. Both obliviously ignored the smartphone market assuming their bread and butter x86 market for desktop/laptop/server is not going anywhere. In 2020, this assumption is challenged - by none other than Apple!

Initially ARM chips tended to be underpowered compared to their x86 brethren. Apple started designing its own ARM chips<span class="margin-toggle sidenote-number"></span><span class="sidenote">These chips were manufactured by Taiwanese giant [TSMC](https://en.wikipedia.org/wiki/TSMC)</span> for their iPhones and iPads. It's latest chip [A13](https://en.wikipedia.org/wiki/Apple_A13) proved to be as powerful as x86 chips while being super efficient! Meanwhile, its laptop lineup (MacBooks) tended to be plagued by heating issues and design limitations
<span class="margin-toggle sidenote-number"></span><span class="sidenote">Apple wants their laptops thin and sleek. However, x86 chips' power consumption meant they needed fans to cool and space to keep these fans. So it ended up having to redesign the keyboards so that they are thin. This turned out to be a disaster.</span> stemming from x86 chips' power inefficiency. In June 2020, Apple has [announced](https://www.apple.com/in/newsroom/2020/06/apple-announces-mac-transition-to-apple-silicon/) that it is ditching Intel's x86 chips for its own ARM chips!

Overall, ARM computers are going to be the future. If there's such a positive outlook for ARM, why is it up for sale. Curse the market and pandemic! ARM is wholly owned by a Japanese holding company called SoftBank. In 2019, Softbank [lost hell lot of money](https://www.cnbc.com/2020/05/18/softbank-ceo-calls-wework-investment-foolish-valuation-falls-to-2point9-billion.html) on IPO of another of its holding called WeWork. 2020 didn't help either with a pandemic. So, SoftBank is selling off ARM to cover its losses. What an unfortunate situation!

## Why ARM's Cofounders are Against the Acquisition

Summarizing the above discussion, a few things are quite clear:

* AI requires huge amount of computational power provided by GPUs
* Nvidia is a monopoly in AI computing
* ARM is the future of computing

Putting all these pieces together, it's no wonder that Nvidia wants to acquire ARM.
Previous acquisitions of open companies by not-so-open companies did not go well. Sun Microsystems developed many innovative technologies like Java, MySQL, ZFS and NFS. Sun was also an active contributor to Open Source. In 2010, it was acquired by it's competitor Oracle. Oracle essentially killed off Sun and its products after the acquisition.

Nvidia has a lot of reasons to kill off ARM after the acquisition. Most of the customers of ARM's designs are competitors of Nvidia. Given the Nvidia's interests and anti-market nature, Oracle-Sun disaster is likely to repeat itself with Nvidia-ARM acquisition. Computing world will suffer from the absence of a reliable architecture. ARM's cofounders, Tudor Brown and Hermann Hauser, voiced [these](https://www.bbc.com/news/technology-53678506) [concerns](https://www.bbc.com/news/technology-53637463) publicly to BBC.

In a [letter to Financial Times](https://www.ft.com/content/4970848d-7821-45dc-b8cb-211036be5d30), Hermann Hauser puts forward the idea that acquisition will hit technological sovereignty of UK and Europe. ARM is a British company employing about 2,500 employees in Cambridge, UK. ARM's acquisition by an American company will inevitably lead to job losses in UK. ARM is one of the last great European technology  companies. Its owning by a US company will be a disaster for UK's sovergnity. With the looming tech war with China, this is going to be doubly important.

<blockquote>

Whether we are allowed to use our own British-designed microprocessors in the UK and Europe will be decided in the White House rather than in Downing Street. This is a major step towards the UK becoming an American vassal state. It must be stopped.

<p class="footer">Hermann Hauser</p>
</blockquote>


## What About AI?

x86 is a dying breed and ARM is the future. If Nvidia acquires ARM, there'll be a complete consolidation of computing market and no appreciable competition. GPU market is already suffering due to the monopoly of Nvidia. Eventually monopolies stop performing and start seeking rent on the market. This happened with Microsoft in 2000s as it consolidated PC market. Microsoft resisted and actively contributed to the detriment of innovations in web browsers, operating systems and lot more. It required a big punch in the face with Linux and cloud servers for it to start innovating again<span class="margin-toggle sidenote-number"></span><span class="sidenote">2020 Microsoft has completely embraced open source movement. It even [openly admitted](https://www.theverge.com/2020/5/18/21262103/microsoft-open-source-linux-history-wrong-statement) that it was wrong about open source. Gotta love Satya Nadella for what he did with the Micro-gaint.</span>.

Nvidia is already showing signs of becoming 2000s Microsoft. Its propietarary software is at odds with rest of the open source ecosystem of AI. As of 2020, deployment of AI is hard is because of the fact that Nvidia won't play well with open source and Linux kernel. Nvidia will never let another GPU manufacturer eat its market share. Soon, Nvidia will start actively contributing to the detriment of AI - just like Microsoft did for operating systems<span class="margin-toggle sidenote-number"></span><span class="sidenote">Microsoft's former CEO famously caled [*Linux a cancer for intellectual property*](https://www.theverge.com/2020/5/18/21262103/microsoft-open-source-linux-history-wrong-statement). When Nvidia starts clamoring about IP, we will know that the transformation is complete. </span>.

AI models are particularly compute hungry. Let's take [GPT-3](https://en.wikipedia.org/wiki/GPT-3) for example. GPT's generated text is so coherent that it is hard to tell if it's written by a machine. Computing power was an [essential ingredient](https://www.zdnet.com/article/what-is-gpt-3-everything-business-needs-to-know-about-openais-breakthrough-ai-language-program/) for this AI's training. It has been [estimated](https://lambdalabs.com/blog/demystifying-gpt-3/) that it costs USD 4.6 million just for the compute. There were articles [discussing](https://news.mit.edu/2020/shrinking-deep-learning-carbon-footprint-0807) carbon footprint of AI and deep learning!
Computing power was and will remain important to the progress of AI. 

<span class="marginnote">
    Compute of GPT-3 compared. [Source](https://www.zdnet.com/article/what-is-gpt-3-everything-business-needs-to-know-about-openais-breakthrough-ai-language-program/).
</span>
<img src="/assets/images/nvidia-arm/gpt-compute.png">

If there's no innovation and growth in the computing market, AI will suffer. Once compute power becomes saturating (yet again), training new AIs like GPT-3 will be ruled out or only available to select few. In the latter case, open and collaborative nature of AI research will die off. As of 2020, AI has the *potential* to solve some of the world's toughest problems including health and education. As compute saturates, AI will fail to achieve these '[grandiose objectives](https://en.wikipedia.org/wiki/AI_winter#The_Lighthill_report)' it set out for.

## What are the Alternatives?

ARM is up for sale, no question in that. Ideally, no chip manufacturer should buy ARM. If regulators think that Nvidia and ARM are not competitors and the deal will go fine, they will be up for a sucker punch. Regulators should ideally block this deal, but we all know how powerful they are against commercial interests. ARM should probably be acquired by a software first company like Google, Microsoft or IBM. Microsoft and IBM/RedHat are a great fit because of their focus on open source<span class="margin-toggle sidenote-number"></span><span class="sidenote">Can't believe how things have changed for open source and MSFT/IBM. Topic for another day.</span> and compute on cloud.

If Nvidia-ARM deal does go through, rest of the technology community<span class="margin-toggle sidenote-number"></span><span class="sidenote">Starting with smart phone ecosystem (Google, Apple, Samsung) and academic institutions.</span> should invest hard into [RISC-V](https://en.wikipedia.org/wiki/RISC-V), a truly open source alternative to ARM. Industry is likely to be set back by a few years because RISC-V doesn't have parity with ARM. As ARM becomes closed over the years, we can be pretty sure that people will find ways to make RISC-V better.

Another way AI community can protect itself from another AI winter is to directly attack Nvidia's moot, CUDA. Community should come together and create an open source CUDA alternative<span class="margin-toggle sidenote-number"></span><span class="sidenote">Many CUDA alternatives *do* exist today - OpenCL being the best known. Unfortunately none of them got traction.</span>. With an open source CUDA alternative, AI will not be exclusive to Nvidia and it will not be able to control AI ecosystem. AI community should ensure that compute software doesn't depend on a hardware manufacturer.

## Conclusion

In this post, we have discussed the history of AI and how compute power is important to the progress of AI. Nvidia played a key role in the current deep learning revolution in terms of both hardware and software. In the process, Nvidia has become a monopoly for AI compute.

We have discussed the history of computers themselves and their architecture. We have inspected how Intel and AMD's x86 chips are getting irrelevant. Rise of smartphones lead to proliferation of ARM processors. We observed the business model of ARM and how it doesn't actually manufacture the chips. We also showed why ARM is the future of computers.

We have also discussed why Nvidia's ARM acquisition is likely to be a disaster. This acquisition will cement Nvidia's position as a monopoly not only for AI, but also for computing in general. Since compute is important for AI's progress and monopolies don't perform, AI's progress may be hindered. We have ended the discussion with what we could do about the whole situation.