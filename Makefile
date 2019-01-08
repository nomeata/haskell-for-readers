all: haskell-for-readers.html

%.html: %.md pandoc.css
	pandoc --toc --standalone --css pandoc.css $< -o $@
