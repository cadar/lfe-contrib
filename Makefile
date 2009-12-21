# from chaos comes order - Friedrich Nietzsche
# if chaos not expected, your dev process will not survive.

LFE_SRC   := $(wildcard src/*.lfe)
LFE_BEAM  := $(LFE_SRC:src/%.lfe=ebin/%.beam)
 
INCL_DIRS := $(wildcard deps/*/include) include
EBIN_DIRS := $(wildcard deps/*/ebin) ebin ~/lfe/ebin
 
FLAGS     := -noshell -noinput $(INCL_DIRS:%=-I %) $(EBIN_DIRS:%=-pa %)  -pa ~/lithium/lib/lfe/ebin
OPTIONS   := {outdir,"ebin"}
 
compile: $(LFE_BEAM)
 
ebin/%.beam: src/%.lfe
	@mkdir -p ebin
	@erl $(FLAGS) -eval \
	'case lfe_comp:file("$<",[report,$(OPTIONS)]) of error -> halt(1); _ -> halt(0) end.'
 
repl:
	@erl $(FLAGS) -s lfe_boot start
 
clean:
	@rm -rf $(LFE_BEAM) *.dump

start: compile
	@erl -pa ~/lithium/lib/lfe/ebin -pa ./ebin/ -s matrix start -noshell -s erlang halt
 
