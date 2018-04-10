.PHONY: all clean emp pop push

BIN = ../../../codscost.native ${ARGS}

all: emp pop push

clean:
	rm -f *.dot *.pdf

emp:
	$(BIN) treiber.tiny treiber.heap ../empty.summaries

pop:
	$(BIN) -f pop treiber.tiny treiber.heap treiber.summaries

push:
	$(BIN) -f push treiber.tiny treiber.heap treiber.summaries

mcg:
	$(BIN) treiber.tiny treiber.heap treiber.summaries
