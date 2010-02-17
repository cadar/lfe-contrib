# from chaos comes order - Friedrich Nietzsche
# if chaos not expected, your dev process will not survive.

LFE_SRC   := $(wildcard *.lfe) 
LFE_BEAM  := $(LFE_SRC:%.lfe=%.beam)
LFE_FLY   := $(LFE_SRC:%.lfe=%_flymake.lfe)
LFE       := ~/lithium/lib/lfe/ebin
OPT       := -pa $(LFE) -pa ./ebin -pa . -noshell -noinput

.PHONY: all start

all: $(LFE_BEAM)

%.beam: %.lfe
	@echo $<
	@erl $(OPT) -eval 'lfe_comp:file("$<",[{outdir,"ebin"}]).' -s erlang halt
	@erl $(OPT) -eval 'lfe_comp:file("$<",[to_exp]).' -s erlang halt
	@cp $< ../../$*.txt

start: all
	@erl $(OPT) -eval 'io:format("~p~n",[lfeimage:start_link()]).' -s erlang halt

repl:
	@erl $(OPT) -s lfe_boot start

clean:
	@rm -rf $(LFE_BEAM) *.dump err out log *.beam ebin/*.beam esrc/*.lfe Mnesia.nonode@nohost *.expand 
	@mv *~ typescript /tmp 

check-syntax: 
	@erl $(OPT) -eval 'code:load_file(sup_comp).'  -eval 'lfe_comp:file("$(CHK_SOURCES)").' -s erlang halt

