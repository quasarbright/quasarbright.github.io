default: composable-promises.html
%.html: %.scrbl
	scribble --redirect-main https://docs.racket-lang.org/ +m $<
clean:
	rm *.html *.css *.js
