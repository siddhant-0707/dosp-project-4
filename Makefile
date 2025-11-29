PARAMS = 200 50 120

all: engine simulator

clean:
	rm -f reddit.db
	rm -rf simulator/metrics

engine: $(shell find engine -type f)
	cd engine && gleam build

simulator: $(shell find simulator -type f)
	cd simulator && gleam build

run: clean engine simulator
	mkdir -p simulator/metrics
	cd engine && gleam run -m main
	cd simulator && gleam run -m sim_main $(PARAMS)