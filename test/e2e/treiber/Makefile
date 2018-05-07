BIN = ../../../coachman.native ${ARGS}

.PHONY: all clean emp pop push mgc

all: emp pop push mgc

clean:
	rm -f *.dot *.pdf

emp:
	$(BIN) treiber.tiny treiber.heap ../empty.summaries

pop:
	$(BIN) -f pop treiber.tiny treiber.heap treiber.summaries

push:
	$(BIN) -f push treiber.tiny treiber.heap treiber.summaries

mgc:
	$(BIN) treiber.tiny treiber.heap treiber.summaries
