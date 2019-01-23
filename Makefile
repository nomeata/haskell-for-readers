all: haskell-for-readers.html code-slides.html files

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
