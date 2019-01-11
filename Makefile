all: haskell-for-readers.html

%.html: %.md pandoc.css solution.css solution.js
	pandoc \
	  --toc \
	  --toc-depth 2 \
	  --number-sections \
	  --section-divs \
	  --standalone \
	  --include-in-header solution.js \
	  --css pandoc.css \
	  --css solution.css \
	  --write=html5 \
	  -V lang=en-US \
	  $< -o $@
