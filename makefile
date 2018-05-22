static/map.css: static/map.css.tmpl static/data/areas-1b7549470.json
	cabal run -v0 make-map-css -- $^ > $@
