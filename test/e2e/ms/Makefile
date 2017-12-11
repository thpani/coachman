.PHONY: all clean emp emp_nolag deq deq_nolag enq enq_nolag

BIN = ../../../codscost.native ${ARGS}

all: emp emp_nolag deq deq_nolag enq enq_nolag

clean:
	rm -f *.dot *.pdf

emp:
	$(BIN) ms.tiny ms.heap ../empty.summaries

emp_nolag:
	$(BIN) ms.tiny ms_nolag.heap ../empty.summaries

deq:
	$(BIN) ms_deq.tiny ms.heap ms_deq.summaries

deq_nolag:
	$(BIN) ms_deq.tiny ms_nolag.heap ms_deq.summaries

enq:
	$(BIN) ms_enq.tiny ms.heap ms_enq.summaries

enq_nolag:
	$(BIN) ms_enq.tiny ms.tail_nolag.heap ms_enq.summaries
