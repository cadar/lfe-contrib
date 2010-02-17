# from chaos comes order - Friedrich Nietzsche
# if chaos not expected, your dev process will not survive.

LFE_SRC   := $(wildcard src/*.lfe)
LFE_BEAM  := $(LFE_SRC:src/%.lfe=ebin/%.beam)
 
INCL_DIRS := $(wildcard deps/*/include) include
EBIN_DIRS := $(wildcard deps/*/ebin) ebin ~/lithium/lib/lfe/ebin
FLAGS     := -noshell -noinput $(INCL_DIRS:%=-I %) $(EBIN_DIRS:%=-pa %) 
OPT       := {outdir,"ebin"}
 
compile : $(LFE_BEAM)
 
ebin/%.beam: src/%.lfe
	@mkdir -p ebin
	@echo $<
	@erl $(FLAGS) -eval 'lfe_comp:file("$<",[${OPT}]).' -s erlang halt
 
repl:
	@erl $(FLAGS) -s lfe_boot start 
 
clean:
	@rm -rf $(LFE_BEAM) *.dump image

start: compile
	@erl $(FLAGS) -s lfeimage start_link 
 

