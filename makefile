IMAGE := docker.dragonfly.co.nz/bailiwick-ui/nix-build:v5
AWS_ACCESS_KEY_ID=$(shell cat .env/AWS_ACCESS_KEY_ID)
AWS_SECRET_ACCESS_KEY=$(shell cat .env/AWS_SECRET_ACCESS_KEY)



static/map.css: static/map.css.tmpl static/data/areas-1b7549470.json
	cabal run -v0 make-map-css -- $^ > $@


docker:
	docker build -t $(IMAGE) \
    --build-arg AWS_ACCESS_KEY_ID=$(AWS_ACCESS_KEY_ID) \
    --build-arg AWS_SECRET_ACCESS_KEY=$(AWS_SECRET_ACCESS_KEY) .




push:
	docker push $(IMAGE)

interact:
	docker run -it --rm -v $$(pwd):/work -w /work $(IMAGE) bash
