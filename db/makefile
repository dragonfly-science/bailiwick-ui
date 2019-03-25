## Run make here to write out files
PREFIX := dev


all: $(PREFIX)/areas.json $(PREFIX)/themes.json $(PREFIX)/area-summaries.json

$(PREFIX)/areas.json: areas.r data/REARdb.rda functions.r
	mkdir -p $(PREFIX)
	Rscript $^ $@

$(PREFIX)/themes.json: themes.r data/REARdb.rda config/themes.yaml functions.r
	mkdir -p $(PREFIX)
	Rscript $^ $@

$(PREFIX)/%.json: %.r data/REARdb.rda functions.r
	mkdir -p $(PREFIX)
	Rscript $^ $@
