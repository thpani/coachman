.PHONY: all emp emp_nolag deq deq_nolag

BIN = ../../../main.native

all: emp emp_nolag deq deq_nolag

emp:
	$(BIN) ms.tiny ms.heap ../empty.summaries

emp_nolag:
	$(BIN) ms.tiny ms_nolag.heap ../empty.summaries

deq:
	$(BIN) ms_deq.tiny ms.heap ms_deq.summaries

deq_nolag:
	$(BIN) ms_deq.tiny ms_nolag.heap ms_deq.summaries
