default: main.html
%.html: %.scrbl
	scribble --htmls --redirect-main https://docs.racket-lang.org/ +m $<
clean:
	rm main/*.{html,css,js}
