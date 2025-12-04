PARAMS = demo

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
# 	cd client && gleam run -m main -- $(PARAMS)