---
layout: post
title: "Here is Why Google is Challenging Pharma with AI"
author: Sasank Chilamkurthy
twitter_image: 
---

Google's parent company Alphabet [just](https://www.ft.com/content/499debf2-4643-449c-83e4-20a532624bbd) [announced](https://www.cnbc.com/2021/11/05/isomorphic-labs-deepmind-ceo-to-lead-new-alphabet-drug-discovery-lab.html) that it has launched a new drug discovery company called [Isomorphic Labs](https://www.isomorphiclabs.com/). At the first sight, this doesn't make any sense. Why is a computer technology company taking on well-entrenched pharmaceutical companies like Pfizer and Roche? See the confidence exuded by one of the Google's executives:

> UK-registered Isomorphic Labs will use technology from its sister company DeepMind “to accelerate drug discovery, and ultimately, find cures for some of humanity’s most devastating diseases,” said Demis Hassabis, the head of DeepMind, in a blog post.

He can't be bluffing, right? Even more interestingly, Google says it'll use artificial intelligence (AI) to discover drugs to cure humanity's diseases. AI and drugs? They must be crazy now. Well, not really. Google does have a trick by its sleeve and it's called AlphaFold. It can be used to predict the shape of protein with great accuracy. So what? How does that help solve humanity's most devastating diseases? Well that's the point of this post -- I'll explain how our body *really* works, why AlphaFold is a breakthrough and finally how it can help find new drugs.

We need to start at the basics of biochemistry and macromolecules. Next, we'll see how a protein's shape allows it to do its functions. This will allow us to appreciate the discovery that is AlphaFold. Finally, we'll understand how our body is really made up of protein-protein interactions and is kind of a network, not unlike neural networks. This should help us understand why Google is getting into drug discovery business.

## Biochemistry and Macromolecules

All known living organisms, including ourselves, are built up of and run by molecules containing carbon -- the same carbon present in coal and diamonds. These molecules are called 'organic' because they are related to life. There are multiple ways to classify vast number of organic molecules present in our body based on their size and building blocks. 

Let's start with size. Micromolecules or [monomers](https://en.wikipedia.org/wiki/Monomer) are small size molecules that usually contain less than 100 atoms. Examples include glucose (C₆H₁₂O₆), glycine (C₂H₅NO₂) and adenine (C₅H₅N₅). [Macromolecules](https://en.wikipedia.org/wiki/Macromolecule) or polymers are formed by repeating or putting together monomers using chemical bonds, usually in a chain. For example, cellulose, main structural component of plants (think wood) is nothing but repetition of glucose molecules up to 10,000 times. 

<figure>
<label for="mn-fig-1" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-1" class="margin-toggle">
<span class="marginnote">Cellulose chemical structure. Building block of cellulose (green) is glucose (red). Only a small portion of cellulose is shown here. Full structure repeats glucose thousands of times.</span>
<img src="assets/images/protein_drugs/cellulose.png" alt="">
</figure>

While cellulose can get really large with more than hundreds of thousands of atoms, it's a simple repetition of a single unit. Things get lot more complicated if macromolecules are a non-repeating sequence of more than one building block. That gets us to next type of classification based on building blocks. Here are the major types of macromolecules and their building blocks.


| Macromolecule  (Polymer)   | Building Block  (Monomer)   | 
|---|---|---|
| Nucleic acids  | Nucleotides | 
| Proteins  | Amino acids  | 
| Polysaccharides  | Monosaccharides |
| Lipids (Fats)  | Hydrocarbons | 

Nucleic acids, for example, are sequences of four nucleotides -- adenine [A], thymine [T], cytosine [C] and guanine [G]. Most well known nucleic acid is probably Deoxyribonucleic acid or DNA. The sequence of nucleotides is *not* repetitive (like that of cellulose) which makes them perfect for the storage of information. Think how a book is just a sequence of alphabet. DNA, for example, contains information required for development and functioning of a organism written in the alphabet of 'ATCG'. We'll discuss the information part and DNA later in a later post.

## Proteins Control Reactions

Similar to nucleic acids, a protein is a sequences of amino acids. There are 20 of them and some of them are attracted to water while others get repelled by water. This property is important because, unlike DNA which has famous double helical structure, 3D structure of proteins is not straight forward. Because of attractions and repulsion between amino acids and life's main solvent water, proteins 'fold' into a vast number of different three-dimensional shapes based on the sequence. This structure is what makes a protein biologically functional.

<figure>
<label for="mn-fig-3" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-3" class="margin-toggle">
<span class="marginnote">Protein before and after folding.</span>
<img src="https://upload.wikimedia.org/wikipedia/commons/a/a9/Protein_folding.png" alt="">
</figure>

So, what exactly proteins do? They dramatically speed up chemical reactions that make up life including photosynthesis, metabolism, movement and vision. So much so that these biochemical reactions do not happen without the presence<label for="sn-1" class="margin-toggle sidenote-number"></label><input type="checkbox" id="sn-1" class="margin-toggle"/>
<span class="sidenote">
If you're computer engineer like me, you can think of protein as a gate of [transistor](https://en.wikipedia.org/wiki/Transistor). Without gate potential, transistor doesn't let the current pass. Similarly without the presence of a protein, reaction doesn't happen.
</span> of a protein. This process is called as [enzyme catalysis](https://en.wikipedia.org/wiki/Enzyme_catalysis). Each reaction step has its own protein enzyme and these enzymes are extraordinarily specific: a protein can catalyze only one or two reactions.

How does this enzyme catalysis thing work? It works based on shape of the protein! When a protein is folded, its varied shape and local amino acid presence can create so-called 'active sites'. These active sites are purpose built for both shape and chemistry of a specific substrate. Finally active site can make the reactions happen by breaking up or bringing together the substrates. See the below animation for schematic of how this works.

<figure>
<label for="mn-fig-3" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-3" class="margin-toggle">
<span class="marginnote">How enzymes work. [Source](https://www.mrdubuque.com/home/biodub-my-gifs-to-you-enzyme-reactions)</span>
<img src="https://www.mrdubuque.com/uploads/2/4/5/0/24509062/x2xgpu-orig_orig.gif" alt="">
</figure>

You can see how shape of proteins is all-important for its functioning. How do we then guess the shape of a protein given the sequence of amino acid? This is not straightforward at all yet protein holding happens almost instantaneously. Protein folding been a unsolved problem for years. However last year, Deepmind, AI research division of Google, [announced](https://deepmind.com/blog/article/AlphaFold-Using-AI-for-scientific-discovery) solving it with very high accuracy using deep learning.


<figure>
<label for="mn-fig-3" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-3" class="margin-toggle">
<span class="marginnote">A schematic of the architecture of the AlphaFold system predicting structure from protein sequence. [Source](https://deepmind.com/blog/article/AlphaFold-Using-AI-for-scientific-discovery)</span>
<img src="
https://kstatic.googleusercontent.com/files/7f0ce54218f3f56f78f544146d261f4010f04390e00edf680434a8dc1e34bcb10255605db91a9e339335050a52261ae3523725cc1512095e221befb6f1cf2504
" alt="">
</figure>

Rightly so, this is touted as one of [the](https://www.embl.org/news/science/alphafold-potential-impacts/) [biggest](https://www.nature.com/articles/d41586-020-03348-4) [breakthroughs](https://www.forbes.com/sites/robtoews/2021/10/03/alphafold-is-the-most-important-achievement-in-ai-ever/). Deepmind later [published](https://www.nature.com/articles/s41586-021-03819-2) their methods in Nature and [opensourced](https://github.com/deepmind/alphafold) the code to predict protein structure. While protein folding is important, it doesn't fully explain why Google is getting into drug discovery. We need to understand bit more biology and current drug discovery process.
 
## Proteins Control Other Proteins

We understood basics of biochemistry and enzymology. Enzyme catalysis, as explained above, can only make a specific reaction step to happen. Everything we associate with life--breathing and generation of energy, muscle movement, functioning of brain--are sets of many reaction steps put together. These reactions interact with each other and can get extraordinarily complex. For example, the infographic below illustrates the reaction steps involved in generation of energy from glucose and other food items.

<figure>
<label for="mn-fig-4" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-4" class="margin-toggle">
<span class="marginnote">Major metabolic pathways in metro-style map. [Source](https://en.wikipedia.org/wiki/Metabolic_pathway)</span>
<img src="https://upload.wikimedia.org/wikipedia/commons/6/6e/Metabolic_Metro_Map.svg" alt="">
</figure>

Metabolism or generation of energy is one of the most well-understood reaction pathways. There are thousands of such pathways peppered around our body. Proteins not only enable each step in these pathways but they also regulate the whole pathway. How? A protein (say X) can also control how much of another protein (say Y) is produced through a mechanism called [transcription regulation](https://en.wikipedia.org/wiki/Transcriptional_regulation)<label for="sn-2" class="margin-toggle sidenote-number"></label><input type="checkbox" id="sn-2" class="margin-toggle"/>
<span class="sidenote">
X is then called as [transcription factor](https://en.wikipedia.org/wiki/Transcription_factor) of Y</span>. And this protein X can itself regulated by yet another protein Z and so on. This whole transcription thing can be represented as graph or network called transcription network.

<figure>
<label for="mn-fig-4" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-4" class="margin-toggle">
<span class="marginnote">A fairly *simple* transcription network. Taken from Uri Alon's book</span>
<img src="/assets/images/protein_drugs/transcription_network.png" alt="">
</figure>

## Cognitive Problem of the Cell

This whole protein story is getting more and more complex. Why does this have to be like this? Let's take a step back and look at bigger picture of what an organism is trying to do. Let's restrict to simple single-celled organisms like bacteria instead of complicated multi-celled multi-organ organism like ourselves. If you think about it, even the basic singled-celled organism is solving a complex information-processing problem and this is the reason why things are so complex.

Cells live in a complex environment and has to handle variety of situations. Let's take eating or consumption of food. Cell has to first sense if there is a sugar around it in a reasonable distance. Then it has to determine which direction the sugar is present. Then it has to co-ordinate itself to move towards the sugar and transport it into cell. Finally, above described metabolic pathway kicks in and the sugar is utilized. If cells gets damaged in the process, it has to sense that and start repair process.

Therefore, cell has to sense many different physical parameters including temperature, osmotic pressure, signals from other cells, nutrients, harmful chemicals and make decisions to maximize its survival.

References:
1. https://en.wikipedia.org/wiki/Macromolecule
2. https://en.wikipedia.org/wiki/Protein_folding
3. https://app.getpocket.comhttps://en.wikipedia.org/wiki/Enzyme_catalysis
4. https://en.wikipedia.org/wiki/Genetic_code