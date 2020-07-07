#!/bin/sh
Rscript ../render.R min-iterations.Rmd min-iter.md out
cp out/min-iter.md ~/Projects/stefan-marr.de/_posts/Research/2020-07-05-is-this-noise-or-does-this-mean-something-benchmarking.md
cp min-iterations-figures/* ~/Projects/stefan-marr.de/assets/2020/07/is-this-noise
cp figure/* ~/Projects/stefan-marr.de/assets/2020/07/is-this-noise

sed -i '' 's/min-iterations-figures/{{ site.baseurl }}\/assets\/2020\/07\/is-this-noise/g' ~/Projects/stefan-marr.de/_posts/Research/2020-07-05-is-this-noise-or-does-this-mean-something-benchmarking.md
sed -i '' 's/figure\//{{ site.baseurl }}\/assets\/2020\/07\/is-this-noise\//g' ~/Projects/stefan-marr.de/_posts/Research/2020-07-05-is-this-noise-or-does-this-mean-something-benchmarking.md
