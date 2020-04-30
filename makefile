IMAGE := docker.dragonfly.co.nz/bailiwick-ui/nix-build:v10

static/map.css: static/map.css.tmpl static/data/areas-1b7549470.json
	cabal run -v0 make-map-css -- $^ > $@


docker:
	#NIX_SECRET_KEY_FILE=/home/finlay/dragonfly/bailiwick-ui/nix-serve.sec nix-serve  -p 8080 &
	docker build -t $(IMAGE) .




push:
	docker push $(IMAGE)

interact:
	docker run -it --rm -v $$(pwd):/work -w /work $(IMAGE) bash
