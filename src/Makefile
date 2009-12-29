# from chaos comes order - Friedrich Nietzsche
# if chaos not expected, your dev process will not survive.

LFE_SRC   := $(wildcard *.lfe)
LFE_BEAM  := $(LFE_SRC:%.lfe=%.beam)
LFE_FLY   := $(LFE_SRC:%.lfe=%_flymake.lfe)
LFE       := ~/lithium/lib/lfe/ebin
OPT       := -noshell -noinput -pa $(LFE) -pa ./ebin

.PHONY: all start

all: $(LFE_BEAM)

#sup_comp.beam: sup_comp.lfe
#	@erl $(OPT) -eval 'case lfe_comp:file("$<") of error -> halt(1); _ -> halt(0) end.'

%.beam: %.lfe
	@erl $(OPT) -eval 'lfe_comp:file("$<").' -s erlang halt

init: matrix.beam
	@erl $(OPT) -s matrix init -s erlang halt
start: matrix.beam
	@erl $(OPT) -s matrix start -s erlang halt

repl:
	@erl $(OPT) -s lfe_boot start
 
clean:
	@rm -rf $(LFE_BEAM) *.dump err out log *.beam ebin/*.beam tmp/*.lfe

check-syntax: 
	@erl $(OPT) -eval 'code:load_file(sup_comp).'  -eval 'lfe_comp:file("$(CHK_SOURCES)").' -s erlang halt


