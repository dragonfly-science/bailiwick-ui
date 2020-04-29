## Run make here to write out files
PREFIX := dev


all: $(PREFIX)/areas.json \
	$(PREFIX)/themes.json \
	$(PREFIX)/area-summaries.json \
	$(PREFIX)/.indicators

$(PREFIX)/areas.json: areas.r data/REARdb.rda $(PREFIX)/indicatorAreas.rda functions.r
	mkdir -p $(PREFIX)
	Rscript areas.r data/REARdb.rda $(PREFIX)/indicatorAreas.rda $(PREFIX)/areas.json

$(PREFIX)/indicatorAreas.rda: $(PREFIX)/.indicators

$(PREFIX)/themes.json: themes.r data/REARdb.rda config/themes.yaml functions.r patch.r language.r
	mkdir -p $(PREFIX)
	Rscript $^ $@

$(PREFIX)/area-summaries.json: area-summaries.r data/REARdb.rda \
		   $(PREFIX)/areas.json $(PREFIX)/themes.json functions.r
	mkdir -p $(PREFIX)
	Rscript $^ $@

$(PREFIX)/.indicators: indicator-numbers.r data/REARdb.rda \
			$(PREFIX)/themes.json functions.r
	mkdir -p $(PREFIX)
	Rscript indicator-numbers.r data/REARdb.rda $(PREFIX)/themes.json $(PREFIX)
	touch $@
