#**************************************************************************
#*                                                                        *
#*                                OCaml                                   *
#*                                                                        *
#*                 Xavier Clerc, SED, INRIA Rocquencourt                  *
#*            Mark Shinwell and Leo White, Jane Street Europe             *
#*                                                                        *
#*   Copyright 2010 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*   Copyright 2017--2018 Jane Street Group LLC                           *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

BASEDIR=../..
COMPFLAGS=-I $(OTOPDIR)/otherlibs/dynlink
LD_PATH=.:$(TOPDIR)/otherlibs/dynlink

.PHONY: default
default:
	@if ! $(SUPPORTS_SHARED_LIBRARIES); then \
	  echo ' ... skipped (shared libraries not available)'; \
	else \
    printf " ... testing with ocamlc: "; \
	  $(SET_LD_PATH) $(MAKE) compile run; \
		if $(BYTECODE_ONLY); then \
			echo ' ... testing with ocamlopt skipped'; \
		else \
			printf " ... testing with ocamlopt: "; \
			$(SET_LD_PATH) $(MAKE) compile_opt run_opt; \
		fi; \
	fi

.PHONY: compile
compile:
	@$(OCAMLC) -c sheep.mli
	@$(OCAMLC) -c sheep.ml
	@$(OCAMLC) -c pig.mli
	@$(OCAMLC) -c test.ml
	@$(OCAMLC) -c plugin1/sheep.mli
	@$(OCAMLC) -I plugin1 -o plugin1/sheep.cmo -c plugin1/sheep.ml
	@$(OCAMLC) -c plugin2/cow.mli
	@$(OCAMLC) -I plugin2 -o plugin2/cow.cmo -c plugin2/cow.ml
	@$(OCAMLC) -c plugin2b/cow.mli
	@$(OCAMLC) -I plugin2b -o plugin2b/cow.cmo -c plugin2b/cow.ml
	@$(OCAMLC) -c plugin2c/cow.mli
	@$(OCAMLC) -I plugin2c -o plugin2c/cow.cmo -c plugin2c/cow.ml
	@$(OCAMLC) -c plugin3/pig.mli
	@$(OCAMLC) -I plugin3 -o plugin3/pig.cmo -c plugin3/pig.ml
	@$(OCAMLC) -c plugin4/chicken.mli
	@$(OCAMLC) -I plugin4 -o plugin4/chicken.cmo -c plugin4/chicken.ml
	@$(OCAMLC) -c plugin5/chicken.mli
	@$(OCAMLC) -I plugin5 -o plugin5/chicken.cmo -c plugin5/chicken.ml
	@$(OCAMLC) -c plugin6/pheasant.mli
	@$(OCAMLC) -I plugin6 -o plugin6/pheasant.cmo -c plugin6/pheasant.ml
	@$(OCAMLC) -c plugin6/partridge.mli
	@$(OCAMLC) -I plugin6 -o plugin6/partridge.cmo -c plugin6/partridge.ml
	@$(OCAMLC) -o test.byte dynlink.cma sheep.cmo test.cmo

.PHONY: compile_opt
compile_opt:
	@$(OCAMLOPT) -c sheep.mli
	@$(OCAMLOPT) -c sheep.ml
	@$(OCAMLOPT) -c pig.mli
	@$(OCAMLOPT) -c test.ml
	@$(OCAMLOPT) -c plugin1/sheep.mli
	@$(OCAMLOPT) -I plugin1 -o plugin1/sheep.cmxs -shared plugin1/sheep.ml
	@$(OCAMLOPT) -c plugin2/cow.mli
	@$(OCAMLOPT) -I plugin2 -o plugin2/cow.cmxs -shared plugin2/cow.ml
	@$(OCAMLOPT) -c plugin2b/cow.mli
	@$(OCAMLOPT) -I plugin2b -o plugin2b/cow.cmxs -shared plugin2b/cow.ml
	@$(OCAMLOPT) -c plugin2c/cow.mli
	@$(OCAMLOPT) -I plugin2c -o plugin2c/cow.cmxs -shared plugin2c/cow.ml
	@$(OCAMLOPT) -c plugin3/pig.mli
	@$(OCAMLOPT) -I plugin3 -o plugin3/pig.cmxs -shared plugin3/pig.ml
	@$(OCAMLOPT) -c plugin4/chicken.mli
	@$(OCAMLOPT) -I plugin4 -o plugin4/chicken.cmxs -shared plugin4/chicken.ml
	@$(OCAMLOPT) -c plugin5/chicken.mli
	@$(OCAMLOPT) -I plugin5 -o plugin5/chicken.cmxs -shared plugin5/chicken.ml
	@$(OCAMLOPT) -c plugin6/pheasant.mli
	@$(OCAMLOPT) -I plugin6 -o plugin6/pheasant.cmxs -shared plugin6/pheasant.ml
	@$(OCAMLOPT) -c plugin6/partridge.mli
	@$(OCAMLOPT) -I plugin6 -o plugin6/partridge.cmxs -shared plugin6/partridge.ml
	@$(OCAMLOPT) -o test.exe dynlink.cmxa sheep.cmx test.cmx

.PHONY: run1
run:
	@./test.byte && echo " => passed" || echo " => failed"

.PHONY: run_opt
run_opt:
	@./test.exe && echo " => passed" || echo " => failed"

.PHONY: promote
promote: defaultpromote

.PHONY: clean
clean: defaultclean
	@rm -f main main.native
	@rm -f plugin1/*.cmo plugin1/*.cmi plugin1/*.cmx plugin1/*.cmxs
	@rm -f plugin1/*.$(O) plugin1/*.$(SO)
	@rm -f plugin2/*.cmo plugin2/*.cmi plugin2/*.cmx plugin2/*.cmxs
	@rm -f plugin2/*.$(O) plugin2/*.$(SO)
	@rm -f plugin2b/*.cmo plugin2b/*.cmi plugin2b/*.cmx plugin2b/*.cmxs
	@rm -f plugin2b/*.$(O) plugin2b/*.$(SO)
	@rm -f plugin2c/*.cmo plugin2c/*.cmi plugin2c/*.cmx plugin2c/*.cmxs
	@rm -f plugin2c/*.$(O) plugin2c/*.$(SO)
	@rm -f plugin3/*.cmo plugin3/*.cmi plugin3/*.cmx plugin3/*.cmxs
	@rm -f plugin3/*.$(O) plugin3/*.$(SO)
	@rm -f plugin4/*.cmo plugin4/*.cmi plugin4/*.cmx plugin4/*.cmxs
	@rm -f plugin4/*.$(O) plugin4/*.$(SO)
	@rm -f plugin5/*.cmo plugin5/*.cmi plugin5/*.cmx plugin5/*.cmxs
	@rm -f plugin5/*.$(O) plugin5/*.$(SO)
	@rm -f plugin6/*.cmo plugin6/*.cmi plugin6/*.cmx plugin6/*.cmxs
	@rm -f plugin6/*.$(O) plugin6/*.$(SO)

include $(BASEDIR)/makefiles/Makefile.common
