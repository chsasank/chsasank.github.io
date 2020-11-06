# My Blog


## Sidenote

```
<label for="sn-2" class="margin-toggle sidenote-number"></label><input type="checkbox" id="sn-2" class="margin-toggle"/>
<span class="sidenote">
Actually, Einstein did work for the patent office when he did his work on special relativity. But he was special. Even most patent clerks admit that.
</span>
```


## Marginnote

```
<label for="mn-1" class="margin-toggle">⊕</label><input type="checkbox" id="mn-1" class="margin-toggle"/>
<span class="marginnote">
Blue text, while also a widely recognizable clickable-text indicator, is crass and distracting. Luckily, it is also rendered unnecessary by the use of underlining.
</span>
```

## Figures

```
<figure>
    <label for="mn-fig-1" class="margin-toggle">⊕</label><input type="checkbox" id="mn-fig-1" class="margin-toggle">
    <span class="marginnote">From Edward Tufte, page 92.</span>
    <img src="img/exports-imports.png" alt="Exports and Imports to and from Denmark &amp; Norway from 1700 to 1780">
</figure>
```

## Convert pdf to text

```
pdftoppm -png ~/Downloads/hamming.pdf /tmp/hamming/page
for f in /tmp/hamming/page*; do tesseract $f $f; done