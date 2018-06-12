IMAGE := docker.dragonfly.co.nz/bailiwick-ui/nix-build


static/map.css: static/map.css.tmpl static/data/areas-1b7549470.json
	cabal run -v0 make-map-css -- $^ > $@


docker:
	docker build -t $(IMAGE) .


push:
	docker push $(IMAGE)

interact:
	docker run -it --rm $(IMAGE) bash
