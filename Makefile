all: haskell-for-readers.html haskell-for-readers.pdf haskell-for-readers-solutions.pdf haskell-for-readers-exercises.pdf code-slides.html files

SHELL=/bin/bash

haskell-for-readers.html: haskell-for-readers.md pandoc.css solution.css solution.js label-exercises
	pandoc \
	  --toc \
	  --toc-depth 2 \
	  --number-sections \
	  --section-divs \
	  --standalone \
	  --include-in-header solution.js \
	  --css pandoc.css \
	  --css solution.css \
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

haskell-for-readers.pdf: haskell-for-readers.md label-exercises move-solutions
	pandoc \
	  --toc \
	  --toc-depth 2 \
	  --number-sections \
	  --section-divs \
	  --filter ./label-exercises \
	  --filter ./move-solutions \
	  --pdf-engine xelatex \
	  -V documentclass=scrartcl \
	  -V lang=en-US \
	  -V classoption=DIV13 \
	  -V colorlinks \
	  -V links-as-notes \
	  -V "mainfont=FreeSerif" \
	  -V "sansfont=FreeSans" \
	  -V "monofont=Inconsolata" \
	  $< -o $@

haskell-for-readers-solutions.pdf: haskell-for-readers.md label-exercises only-solutions
	pandoc \
	  --filter ./label-exercises \
	  --filter ./only-solutions \
	  --pdf-engine xelatex \
	  -V documentclass=scrartcl \
	  -V lang=en-US \
	  -V classoption=DIV13 \
	  -V colorlinks \
	  -V links-as-notes \
	  -V "mainfont=FreeSerif" \
	  -V "sansfont=FreeSans" \
	  -V "monofont=Inconsolata" \
	  $< -o $@

haskell-for-readers-exercises.pdf: haskell-for-readers.md label-exercises only-exercises
	pandoc \
	  --filter ./label-exercises \
	  --filter ./only-exercises \
	  --pdf-engine xelatex \
	  -V documentclass=scrartcl \
	  -V lang=en-US \
	  -V classoption=DIV13 \
	  -V colorlinks \
	  -V links-as-notes \
	  -V "mainfont=FreeSerif" \
	  -V "sansfont=FreeSans" \
	  -V "monofont=Inconsolata" \
	  $< -o $@


code-slides.html: haskell-for-readers.md only-code label-exercises
	pandoc \
	  --filter ./label-exercises \
	  --filter ./only-code \
	  --number-sections \
	  --section-divs \
	  -t slidy \
	  --standalone \
	  $< -o $@

files: haskell-for-readers.md write-files
	rm -rf files
	mkdir files
	pandoc -t json $< | ./write-files
