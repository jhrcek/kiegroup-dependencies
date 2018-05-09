fe_dir = frontend/collect-deps

back:
	stack build --pedantic

front:
	cd $(fe_dir) && elm make --yes --warn src/Main.elm --output dist/js/elm.js

report: back
	cd $(fe_dir)/dist && stack exec collect-deps -- $(HOME)/Devel/github.com/kiegroup

run: front back
	sws -d $(fe_dir)/dist &
	google-chrome http://localhost:3000/index.html >/dev/null 2>&1 &

mini: front
	cd $(fe_dir)/dist/js && uglifyjs elm.js --compress --mangle --output elm.js

kill:
	kill -9 $(shell pgrep sws)

clean:
	stack clean
