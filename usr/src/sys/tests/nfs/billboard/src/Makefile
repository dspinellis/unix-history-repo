###############################################################################
#	@(#)Makefile	1.18 89/09/13 Copyr 1988 SMI
#
#	Copyright (c) 1988 by Sun Microsystems, Inc.
#
# Description:
#	Makefile for the NETlicense Report Generation Tool.
#
# Comments:
#	1) If you want to add a C file to this directory, just add it to the
#	   C_FILES macro. That is all you have to do.
#
###############################################################################

#
# Location of the root of the source hierarchy
#
SRC_DIR = .

#
#
# Include the generic makefile
#
include $(SRC_DIR)/Makefile.inc

#
# The report generation tool headers and product name
#
PRODUCT = $(ARCH)/bb $(ARCH)/xbb $(ARCH)/billboard  $(ARCH)/xbillboard


#
# Misc stuff for test compilations
#
#MALLOC = /usr/lib/debug/malloc.o
MALLOC = 

#
# Location of the common files
#
COMMON_DIR = .

#
# COMMON header files
#

COMMON_HDR_FILES = server.h common.h

#
# COMMON C files
#
COMMON_C_FILES = 

COMMON_OBJS = $(COMMON_C_FILES:.c=.o)

ARCH_COMMON_OBJS = $(COMMON_OBJS:%=${COMMON_DIR}/${ARCH}/%)

# Protocol files
PROTO_FILE = protocol.x

# XDR Routines and Utilities
#
#PROTO_C_FILES = 

#PROTO_OBJS = $(PROTO_C_FILES:.c=.o)

#ARCH_PROTO_OBJS = $(PROTO_OBJS:%=${ARCH}/%)

LOCAL_CPPFLAGS = -I$(SRC_DIR) 

#
# Billboard Client files.
#
CLIENT_C_FILES= billboard.c billboard_util.c \
		protocol_xdr.c protocol_clnt.c

CLIENT_OBJ_FILES = $(CLIENT_C_FILES:.c=.o)

CLIENT_ARCH_OBJS = $(CLIENT_OBJ_FILES:%=${ARCH}/%)

#
# BillBoard Server C files.
#
SERVER_C_FILES = bb_codata.c bb_hash.c bb_board.c bb_passwd.c \
		 bb_phase.c bb_set.c bb_ip.c bb_list.c bb_grid.c \
		 protocol_xdr.c

SERVER_OBJ_FILES = $(SERVER_C_FILES:.c=.o)

SERVER_ARCH_OBJS = $(SERVER_OBJ_FILES:%=${ARCH}/%)

#
# Combined client and server files
#
C_FILES= bb_server.c billboard_rpc.c $(SERVER_C_FILES) $(CLIENT_C_FILES)

ARCH_OBJS= $(SERVER_ARCH_OBJS) $(CLIENT_ARCH_OBJS)

LINT_FILES = $(C_FILES:.c=.ln)

#
# No nested targets although local targets will descend.
#
$(TARGETS): $$@.local

#
# Local target entries
#
all.local debug.local profile.local: $(ARCH) $(PRODUCT)

#
# link line.
#

#
# NFS billboard server, PROGNUM is the RPC program number
#
$(ARCH)/bb: protocol.h $(SERVER_ARCH_OBJS) bb_server.c
	$(COMPILE.c) -DPROGNUM=100073 bb_server.c -o $(ARCH)/bb_server.o
	$(LINK.c) -o $@ $(SERVER_ARCH_OBJS) $(ARCH_COMMON_OBJS) \
	$(ARCH_UTILS_OBJS) $(ARCH_PROTO_OBJS) $(MALLOC) $(ARCH)/bb_server.o

#
# X billboard server, PROGNUM is the RPC program number
#
$(ARCH)/xbb: protocol.h $(SERVER_ARCH_OBJS) bb_server.c
	$(COMPILE.c) -DPROGNUM=100074 bb_server.c -o $(ARCH)/xbb_server.o
	$(LINK.c) -o $@ $(SERVER_ARCH_OBJS) $(ARCH_COMMON_OBJS) \
	$(ARCH_UTILS_OBJS) $(ARCH_PROTO_OBJS) $(MALLOC) $(ARCH)/xbb_server.o

#
# NFS billboard client program, PROGNUM is the RPC program number
#
$(ARCH)/billboard: protocol.h $(CLIENT_ARCH_OBJS) billboard_rpc.c
	$(COMPILE.c) -DPROGNUM=100073 billboard_rpc.c -o $(ARCH)/billboard_rpc.o
	$(LINK.c) -o $@ $(CLIENT_ARCH_OBJS) $(ARCH)/billboard_rpc.o

#
# X billboard client program, PROGNUM is the RPC program number
#
$(ARCH)/xbillboard: protocol.h $(CLIENT_ARCH_OBJS) billboard_rpc.c
	$(COMPILE.c) -DPROGNUM=100074 billboard_rpc.c -o $(ARCH)/xbillboard_rpc.o
	$(LINK.c) -o $@ $(CLIENT_ARCH_OBJS) $(ARCH)/xbillboard_rpc.o

clean.local:
	$(RM) $(PRODUCT) $(ARCH)/*.o 

cflow.local:
	$(CFLOW) $(CFLOWFLAGS) $(C_FILES) > $(CFLOW_OUT)

lint.local:
	$(LINT.c) $(C_FILES)

install.local:
	@if [ "$(DEBUG)" = "DEBUG" ]                                    ;\
	then    echo                                                    ;\
		echo "Making a debugging release!!!"                    ;\
		echo                                                    ;\
		$(MAKE) debug RELEASE=$(RELEASE)      			;\
	else    echo                                                    ;\
		echo "Making all the products ..."                      ;\
		echo                                                    ;\
		$(MAKE) all RELEASE=$(RELEASE)        			;\
	fi
	-mkdir -p $(DESTDIR_BIN) > /dev/null 2>&1
	cp $(PRODUCT) $(DESTDIR_BIN)

#
# Set the release directory
#
REL_SRC = $(DESTDIR_SRC)

release.local:
	$(MAKE) clean RELEASE=$(RELEASE) DESTDIR=$(DESTDIR)
	$(MAKE) install RELEASE=$(RELEASE) DESTDIR=$(DESTDIR)
	#
	# Make the release directory if necessary and copy the source
	#
	if [ ! -d $(REL_SRC) ]	;\
	then mkdir -p $(REL_SRC)	;\
	cp Makefile.inc Makefile $(C_FILES) $(PROTO_FILE) $(COMMON_HDR_FILES) $(REL_SRC)	;\
	fi

sccs_check.local:
	-sccs check >> $(SCCS_CHECK_FILE)
	-echo "Directory: ./lat/rgt" >> $(SCCS_VERS_FILE)  
	-sccs prs $(SCCS_VERS_ARGS) SCCS >> $(SCCS_VERS_FILE)

tags.local: $(C_FILES) $(COMMON_HDR_FILES)
	ctags -tw $(C_FILES) $(COMMON_HDR_FILES)

#
# Local file dependencies
#
#proto_mk: FORCE
#	cd $(PROTO_DIR); $(MAKE) $(TARGET) RELEASE=$(RELEASE) DESTDIR=$(DESTDIR)

#common_mk: FORCE
#	cd $(COMMON_DIR); $(MAKE) $(TARGET) RELEASE=$(RELEASE) DESTDIR=$(DESTDIR)

$(ARCH_OBJS): $(ARCH)

$(ARCH): 
	-mkdir -p $@

$(ARCH)/%.o: %.c
	$(COMPILE.c) -o $@ $<

protocol.h: $(PROTO_FILE)
	rpcgen -h -o $@ $(PROTO_FILE)

protocol_clnt.c: $(PROTO_FILE)
	rpcgen -l -o $@ $(PROTO_FILE)

protocol_xdr.c: $(PROTO_FILE)
	rpcgen -c -o $@ $(PROTO_FILE)

#protocol_svc.c: $(PROTO_FILE)
#	rpcgen -m -o $@ $(PROTO_FILE)

FORCE:
 
depend.local: makedep
	echo '/^# DO NOT DELETE THIS LINE/+1,$$d' >eddep
	echo '$$r makedep' >>eddep
	echo 'w' >>eddep
	cp Makefile Makefile.bak
	ex - Makefile < eddep
	rm eddep makedep makedep1 makedeperrs

makedep: $(C_FILES) $(COMMON_HDR_FILES)
	@cat /dev/null >makedep
	(for i in ${C_FILES} ; do \
		${CPP} -M ${CPPFLAGS} $$i | sed \
		-e 's,^\(.*\)\.o:,\1.o \1.L:,' >>makedep1; done) \
		2>makedeperrs
	${AWK} ' { if ($$1 != prev) { print rec; rec = $$0; prev = $$1; } \
		else { if (length(rec $$3) > 78) { print rec; rec = $$0; } \
		       else rec = rec " " $$3 } } \
	      END { print rec } ' makedep1 > makedep
	@cat makedeperrs
	@(if [ -s makedeperrs ]; then false; fi)

# DO NOT DELETE THIS LINE -- make depend uses it

