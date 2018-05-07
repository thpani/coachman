BIN = ../../../coachman.native ${ARGS}

.PHONY: all clean emp emp_nolag deq deq_nolag enq enq_nolag mgc

all: emp_nolag emp deq_nolag deq enq_nolag enq mgc

clean:
	rm -f *.dot *.pdf

emp:
	$(BIN) ms.tiny ms.heap ../empty.summaries

emp_nolag:
	$(BIN) ms.tiny ms_nolag.heap ../empty.summaries

deq:
	$(BIN) -f deq ms.tiny ms.heap ms.summaries

deq_nolag:
	$(BIN) -f deq ms.tiny ms_nolag.heap ms.summaries

enq:
	$(BIN) -ai -f enq ms.tiny ms.heap ms.summaries

enq_nolag:
	$(BIN) -ai -f enq ms.tiny ms_nolag.heap ms.summaries

mgc:
	$(BIN) -ai ms.tiny ms.heap ms.summaries
