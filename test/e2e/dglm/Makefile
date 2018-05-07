BIN = ../../../coachman.native ${ARGS}

.PHONY: all clean emp emp_nolag deq deq_nolag enq enq_nolag mgc

all: emp_nolag emp deq_nolag deq enq_nolag enq mgc

clean:
	rm -f *.dot *.pdf

emp:
	$(BIN) dglm.tiny dglm.heap ../empty.summaries

emp_nolag:
	$(BIN) dglm.tiny dglm_nolag.heap ../empty.summaries

deq:
	$(BIN) -f deq dglm.tiny dglm.heap dglm.summaries

deq_nolag:
	$(BIN) -f deq dglm.tiny dglm_nolag.heap dglm.summaries

enq:
	$(BIN) -ai -f enq dglm.tiny dglm.heap dglm.summaries

enq_nolag:
	$(BIN) -ai -f enq dglm.tiny dglm_nolag.heap dglm.summaries

mgc:
	$(BIN) -ai dglm.tiny dglm.heap dglm.summaries
