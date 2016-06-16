#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *
#*                                                                        *
#*   Copyright 1999 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# Makefile for the "print" (exact rational arithmetic) library

LIBNAME=print
COBJS=printstubs.$(O)
CLIBNAME=camlprint
EXTRACAMLCFLAGS= \
	-I $(ROOTDIR)/utils -I $(ROOTDIR)/parsing \
	-I $(ROOTDIR)/typing -I $(ROOTDIR)/toplevel
CAMLOBJS= \
	$(ROOTDIR)/utils/misc.cmo \
	$(ROOTDIR)/utils/warnings.cmo \
	$(ROOTDIR)/utils/consistbl.cmo \
	$(ROOTDIR)/parsing/location.cmo \
	$(ROOTDIR)/parsing/longident.cmo \
	$(ROOTDIR)/typing/ident.cmo $(ROOTDIR)/typing/path.cmo \
	$(ROOTDIR)/typing/primitive.cmo $(ROOTDIR)/typing/types.cmo \
	$(ROOTDIR)/typing/btype.cmo $(ROOTDIR)/typing/oprint.cmo \
	$(ROOTDIR)/typing/subst.cmo $(ROOTDIR)/typing/predef.cmo \
	$(ROOTDIR)/typing/datarepr.cmo $(ROOTDIR)/typing/cmi_format.cmo \
	$(ROOTDIR)/typing/env.cmo \
	$(ROOTDIR)/typing/typedtree.cmo $(ROOTDIR)/typing/printtyped.cmo \
	$(ROOTDIR)/typing/ctype.cmo $(ROOTDIR)/typing/printtyp.cmo \
	$(ROOTDIR)/toplevel/genprintval.cmo \
	print.cmo
CMIFILES=print.cmi

include ../Makefile

clean::
	rm -f *~

depend:
	$(CAMLRUN) ../../tools/ocamldep -slash *.mli *.ml >> .depend

-include .depend
