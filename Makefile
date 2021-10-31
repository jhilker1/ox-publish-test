server:
	python3 -m http.server --directory=./public

clean:
	rm -rf ./public/

site:
	emacs.exe -Q --script build.el
