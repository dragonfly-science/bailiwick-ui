## Run make here to write out files
PREFIX := dev


all: $(PREFIX)/areas.json

$(PREFIX)/areas.json: areas.r data/REARdb.rda
	mkdir -p $(PREFIX)
	Rscript $^ $@
