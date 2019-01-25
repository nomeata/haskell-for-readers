all: haskell-for-readers.html haskell-for-readers.pdf haskell-for-readers-solutions.pdf haskell-for-readers-exercises.pdf slides.html files

SHELL=/bin/bash

haskell-for-readers.html: haskell-for-readers.md pandoc.css solution.css solution.js label-exercises inline-code.css
	pandoc \
	  --toc \
	  --toc-depth 2 \
	  --number-sections \
	  --section-divs \
	  --standalone \
	  --include-in-header solution.js \
	  --css pandoc.css \
	  --css solution.css \
	  --css inline-code.css \
	  --filter ./label-exercises \
	  --write=html5 \
	  -V lang=en-US \
	  $< -o $@

only-code: only-code.hs
	ghc --make -O $<
label-exercises: label-exercises.hs
	ghc --make -O $<
write-files: write-files.hs
	ghc --make -O $<
move-solutions: move-solutions.hs
	ghc --make -O $<
only-solutions: only-solutions.hs
	ghc --make -O $<
only-exercises: only-exercises.hs
	ghc --make -O $<

TEX_OPTIONS=\
  --pdf-engine xelatex \
  -V documentclass=scrartcl \
  -V lang=en-US \
  -V classoption=DIV13 \
  -V colorlinks \
  -V links-as-notes \
  -V "mainfont=TeX Gyre Pagella" \
  -V "monofont=Inconsolata" \
  --include-in-header=<(echo '\usepackage{newunicodechar}') \
  --include-in-header=<(echo '\newfontfamily{\fallbackfont}{Unifont}[Scale=MatchLowercase]') \
  --include-in-header=<(echo '\DeclareTextFontCommand{\textfallback}{\fallbackfont}') \
  --include-in-header=<(echo '\newunicodechar{☆}{\textrm{\textfallback{☆}}}') \
  --include-in-header=<(echo '\newunicodechar{λ}{\textrm{\textfallback{λ}}}') \
  --include-in-header=<(echo '\newunicodechar{∘}{\textrm{\textfallback{∘}}}') \
  --include-in-header=<(echo '\newunicodechar{☃}{\textrm{\textfallback{☃}}}') \
  --include-in-header=<(echo '\newunicodechar{█}{\textrm{\textfallback{█}}}') \
  --include-in-header=<(echo '\newunicodechar{ツ}{\textrm{\textfallback{ツ}}}') \

haskell-for-readers.pdf: haskell-for-readers.md label-exercises move-solutions
	pandoc \
	  --toc \
	  --toc-depth 2 \
	  --number-sections \
	  --section-divs \
	  --filter ./label-exercises \
	  --filter ./move-solutions \
	  $(TEX_OPTIONS) \
	  $< -o $@

	  -V "sansfont=TeX Gyre Heros" \
	  # -V "sansfont=FreeSans" \

haskell-for-readers-solutions.pdf: haskell-for-readers.md label-exercises only-solutions
	pandoc \
	  --filter ./label-exercises \
	  --filter ./only-solutions \
	  $(TEX_OPTIONS) \
	  $< -o $@

haskell-for-readers-exercises.pdf: haskell-for-readers.md label-exercises only-exercises
	pandoc \
	  --filter ./label-exercises \
	  --filter ./only-exercises \
	  $(TEX_OPTIONS) \
	  $< -o $@


slides.html: haskell-for-readers.md only-code label-exercises inline-code.css
	pandoc \
	  --filter ./label-exercises \
	  --filter ./only-code \
	  --css inline-code.css \
	  --number-sections \
	  --section-divs \
	  -t slidy \
	  --standalone \
	  $< -o $@

files: haskell-for-readers.md write-files
	rm -rf files
	mkdir files
	pandoc -t json $< | ./write-files
	cd files; tree -H '.' -L 1 --noreport --charset utf-8 > index.html
