.PHONY: all clean emp pop push

BIN = ../../../codscost.native

all: emp pop push

clean:
	rm -f *.dot *.pdf

emp:
	$(BIN) treiber.tiny treiber.heap ../empty.summaries

pop:
	$(BIN) treiber_pop.tiny treiber.heap treiber.summaries

push:
	$(BIN) treiber_push.tiny treiber.heap treiber.summaries
