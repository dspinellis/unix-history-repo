###############################################################################
#   Instructions to Make, for compilation of ISODE QUIPU include files
###############################################################################

###############################################################################
#
# $Header: /f/osi/h/quipu/RCS/Makefile,v 7.4 91/02/22 09:25:20 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.4  91/02/22  09:25:20  mrose
# Interim 6.8
# 
# Revision 7.3  90/12/23  18:46:07  mrose
# update
# 
# Revision 7.2  90/10/17  11:46:10  mrose
# sync
# 
# Revision 7.1  90/07/09  14:38:13  mrose
# sync
# 
# Revision 7.0  89/11/23  21:56:19  mrose
# Release 6.0
# 
###############################################################################

###############################################################################
#
#                                NOTICE
#
#    Acquisition, use, and distribution of this module and related
#    materials are subject to the restrictions of a license agreement.
#    Consult the Preface in the User's Manual for the full terms of
#    this agreement.
#
###############################################################################


HEADERS =       abandon.h add.h attr.h attrvalue.h bind.h \
		common.h commonarg.h compare.h config.h \
		connection.h ds_error.h dsargument.h dsp.h \
		dua.h authen.h entry.h list.h  modify.h \
		modifyrdn.h name.h oid.h read.h remove.h \
		ds_search.h util.h sequence.h dap.h photo.h malloc.h \
		ufn.h nrs_info.h dap2.h dsap.h policy.h turbo.h \
		syntaxes.h algorithm.h

###################################################################
# Here it is...
###################################################################

all:;

inst-all:;      -mkdir $(INCDIR)quipu
		@for h in $(HEADERS); \
			do $(MAKE) DIR=$(INCDIR)quipu TARGET=$$h inst-target; \
			done

inst-target:    $(DIR)/$(TARGET)

$(DIR)/$(TARGET):       $(TARGET)
		-cp $@ z$(TARGET)
		sed -e 's%#include "\(.*\)"%#include "$(INCDIR)\1"%' \
			< $(TARGET) | \
			sed -e 's%#include "/usr/include/\(.*\)"%#include <\1>%' > $@
		-@ls -gls $@
		-@echo ""

install:        inst-all clean

lint:;

clean:;         rm -f z* _* *.orig

grind:;         tgrind -lc $(HEADERS)
