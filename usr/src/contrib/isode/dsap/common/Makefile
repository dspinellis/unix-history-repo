###############################################################################
#   Instructions to Make, for compilation of ISODE DSAP common processes
###############################################################################

###############################################################################
#
# $Header: /f/osi/dsap/common/RCS/Makefile,v 7.5 91/02/22 09:17:58 mrose Interim $
#
#
# $Log:	Makefile,v $
# Revision 7.5  91/02/22  09:17:58  mrose
# Interim 6.8
# 
# Revision 7.4  90/12/23  18:45:29  mrose
# update
# 
# Revision 7.3  90/12/11  10:53:35  mrose
# lock-and-load
# 
# Revision 7.2  90/10/17  11:40:43  mrose
# sync
# 
# Revision 7.1  90/07/09  14:33:40  mrose
# sync
# 
# Revision 7.0  89/11/23  21:41:26  mrose
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


PEPYPATH=	-DPEPYPATH

UPDIR   =       ../

LIBES   =       libcommon.a $(UPDIR)libcommon.a
LLIBS   =       $(TOPDIR)llib-lisode


#######################################################################
# Here is it...
#######################################################################

all:            libcommon
inst-all:	libcommon
install:        inst-all clean
lint:           l-libcommon


#######################################################################
# libcommon
#######################################################################

CFILES  =       acl.c attrv.c ca.c \
		dn_seq.c ds_error.c dsa_info.c \
		entryinfo.c filter.c oid.c oid2.c oidseq.c oc.c \
		parse_error.c pe.c picture.c psap.c \
		tree_struct.c util.c \
		correlate.c std_sntx.c quipu_sntx.c \
		file_print.c file_enc.c file_cpy.c file_str.c \
		attrt_cmp.c attrt_dec.c attrt_new.c attrt_cpy.c  \
		attrt_print.c \
		as_cmp.c as_cpy.c as_del.c as_merge.c \
		as_new.c as_free.c as_find.c as_print.c as_str.c \
		avs_cmp.c avs_cpy.c avs_del.c avs_merge.c \
		avs_new.c avs_free.c avs_print.c avs_str.c \
		dn_cmp.c dn_cpy.c dn_append.c \
		dn_free.c dn_print.c dn_str.c dn_new.c \
		rdn_cmp.c rdn_cpy.c rdn_new.c \
		rdn_free.c rdn_print.c rdn_str.c rdn_merge.c \
		time.c integer.c boolean.c string.c mailbox.c fax.c \
		post.c telex.c teletex.c pdm.c guide.c soundex.c \
		conf.c sys_tai.c sys_init.c tai_init.c tai_args.c \
		certificate.c cpair.c revoke.c protected.c \
		cache.c entry.c parse.c ufn_aet.c ufn_parse.c \
		aetufn.c aetdap.c nrs_info.c inherit.c audio.c \
		cstrings.c service.c security.c cilist.c policy.c \
		ds_ext.c ap.c turbo_avl.c null_int.c turbo_index.c \
		attribute.c docstore.c

OFILES  =       acl.o attrv.o ca.o \
		dn_seq.o ds_error.o dsa_info.o \
		entryinfo.o filter.o oid.o oid2.o oidseq.o oc.o \
		parse_error.o pe.o picture.o psap.o \
		tree_struct.o util.o \
		correlate.o std_sntx.o quipu_sntx.o \
		file_print.o file_enc.o file_cpy.o file_str.o \
		attrt_cmp.o attrt_dec.o attrt_new.o attrt_cpy.o \
		attrt_print.o \
		as_cmp.o as_cpy.o as_del.o as_merge.o \
		as_new.o as_free.o as_find.o as_print.o as_str.o \
		avs_cmp.o avs_cpy.o avs_del.o avs_merge.o \
		avs_new.o avs_free.o avs_print.o avs_str.o \
		dn_cmp.o dn_cpy.o dn_append.o \
		dn_free.o dn_print.o dn_str.o dn_new.o \
		rdn_cmp.o rdn_cpy.o rdn_new.o \
		rdn_free.o rdn_print.o rdn_str.o rdn_merge.o \
		time.o integer.o boolean.o string.o mailbox.o fax.o \
		post.o telex.o teletex.o pdm.o guide.o soundex.o \
		conf.o sys_tai.o sys_init.o tai_init.o tai_args.o \
		certificate.o cpair.o revoke.o protected.o \
		cache.o entry.o parse.o ufn_aet.o ufn_parse.o \
		aetufn.o aetdap.o nrs_info.o inherit.o audio.o \
		cstrings.o service.o security.o cilist.o policy.o \
		ds_ext.o ap.o turbo_avl.o null_int.o turbo_index.o \
		attribute.o docstore.o

libcommon:      libcommon.a

libcommon.a:    commonvrsn.o
		-rm -f $@
		@$(UTILDIR)make-lib.sh $(SYSTEM) $(ARFLAGS) $@ $(OFILES) \
			commonvrsn.o
		-@rm -f $(UPDIR)libcommon.a $(UPDIR)llib-lcommon
		-@ln libcommon.a $(UPDIR)libcommon.a
		-@$(LN) llib-lcommon $(UPDIR)llib-lcommon
		-@ls -l $@
		-@echo "QUIPU COMMON library built normally"

commonvrsn.c:   $(OFILES)
		@$(UTILDIR)version.sh common > $@

l-libcommon:    $(CFILES) true
		$(LINT) $(LFLAGS) $(CFILES) $(LLIBS) \
			| grep -v "warning: possible pointer alignment problem"

test:		test.o libcommon.a ../libx500as.a
		$(LDCC) -g $(LDFLAGS) test.o libcommon.a ../libx500as.a \
			libcommon.a $(TOPDIR)libisode.a -o test

audio.o:	audio.c
		$(CC) $(LIBCFLAGS) -DBINPATH=\"$(BINDIR)\" -c $*.c
		
TESTAVLOBJ =    testavl.o turbo_avl.o

testavl:        $(TESTAVLOBJ)
		$(LDCC) $(LDFLAGS) -o $@ $(TESTAVLOBJ) $(TOPDIR)libisode.a


############################################################
# clean
############################################################

saber_src:;	#load $(OPTIONS) $(CFILES)	

saber_obj:;	#load libcommon.a

############################################################
# clean
############################################################

clean:;         rm -f *.ph *.o *.a a.out _* test x* z* *.orig core commonvrsn.c

grind:;         iprint Makefile
		tgrind -lc $(CFILES) llib-lcommon

true:;

# DO NOT DELETE THIS LINE
# Dependencies follow
acl.o: ../../h/UNIV-types.h
acl.o: ../../h/UNIV_defs.h
acl.o: ../../h/UNIV_pre_defs.h
acl.o: ../../h/cmd_srch.h
acl.o: ../../h/config.h
acl.o: ../../h/general.h
acl.o: ../../h/isoaddrs.h
acl.o: ../../h/logger.h
acl.o: ../../h/manifest.h
acl.o: ../../h/pepsy.h
acl.o: ../../h/psap.h
acl.o: ../../h/quipu/attr.h
acl.o: ../../h/quipu/attrvalue.h
acl.o: ../../h/quipu/authen.h
acl.o: ../../h/quipu/bind.h
acl.o: ../../h/quipu/config.h
acl.o: ../../h/quipu/dsp.h
acl.o: ../../h/quipu/entry.h
acl.o: ../../h/quipu/name.h
acl.o: ../../h/quipu/nrs_info.h
acl.o: ../../h/quipu/oid.h
acl.o: ../../h/quipu/util.h
acl.o: ./../x500as/AF-types.h
acl.o: ./../x500as/AF_defs.h
acl.o: ./../x500as/AF_pre_defs.h
acl.o: ./../x500as/IF-types.h
acl.o: ./../x500as/IF_defs.h
acl.o:../x500as/IF_pre_defs.h
acl.o:../x500as/NRS-types.h
acl.o:../x500as/NRS_defs.h
acl.o:../x500as/NRS_pre_defs.h
acl.o:../x500as/Quipu-types.h
acl.o:../x500as/Quipu_defs.h
acl.o:../x500as/Quipu_pre_defs.h
acl.o:../x500as/SA-types.h
acl.o:../x500as/SA_defs.h
acl.o:../x500as/SA_pre_defs.h
acl.o:../x500as/Thorn-types.h
acl.o:../x500as/Thorn_defs.h
acl.o:../x500as/Thorn_pre_defs.h
acl.o:../x500as/af-cdefs.h
acl.o:../x500as/if-cdefs.h
acl.o:../x500as/nrs-cdefs.h
acl.o:../x500as/qu-cdefs.h
acl.o:../../h/quipu/syntaxes.h
aetdap.o: ../../h/config.h
aetdap.o: ../../h/general.h
aetdap.o: ../../h/isoaddrs.h
aetdap.o: ../../h/logger.h
aetdap.o: ../../h/manifest.h
aetdap.o: ../../h/psap.h
aetdap.o: ../../h/quipu/attr.h
aetdap.o: ../../h/quipu/attrvalue.h
aetdap.o: ../../h/quipu/authen.h
aetdap.o: ../../h/quipu/bind.h
aetdap.o: ../../h/quipu/commonarg.h
aetdap.o: ../../h/quipu/config.h
aetdap.o: ../../h/quipu/dap.h
aetdap.o: ../../h/quipu/ds_error.h
aetdap.o: ../../h/quipu/dsp.h
aetdap.o: ../../h/quipu/dua.h
aetdap.o: ../../h/quipu/name.h
aetdap.o: ../../h/quipu/oid.h
aetdap.o: ../../h/quipu/read.h
aetdap.o: ../../h/quipu/util.h
aetdap.o: ../../h/tailor.h
aetufn.o: ../../h/config.h
aetufn.o: ../../h/general.h
aetufn.o: ../../h/isoaddrs.h
aetufn.o: ../../h/logger.h
aetufn.o: ../../h/manifest.h
aetufn.o: ../../h/psap.h
aetufn.o: ../../h/quipu/attr.h
aetufn.o: ../../h/quipu/attrvalue.h
aetufn.o: ../../h/quipu/authen.h
aetufn.o: ../../h/quipu/bind.h
aetufn.o: ../../h/quipu/commonarg.h
aetufn.o: ../../h/quipu/config.h
aetufn.o: ../../h/quipu/dap.h
aetufn.o: ../../h/quipu/ds_error.h
aetufn.o: ../../h/quipu/dsp.h
aetufn.o: ../../h/quipu/dua.h
aetufn.o: ../../h/quipu/entry.h
aetufn.o: ../../h/quipu/name.h
aetufn.o: ../../h/quipu/oid.h
aetufn.o: ../../h/quipu/read.h
aetufn.o: ../../h/quipu/ufn.h
aetufn.o: ../../h/quipu/util.h
aetufn.o: ../../h/tailor.h
ap.o: ../../h/UNIV-types.h
ap.o: ../../h/UNIV_defs.h
ap.o: ../../h/UNIV_pre_defs.h
ap.o: ../../h/config.h
ap.o: ../../h/general.h
ap.o: ../../h/isoaddrs.h
ap.o: ../../h/logger.h
ap.o: ../../h/manifest.h
ap.o: ../../h/pepsy.h
ap.o: ../../h/psap.h
ap.o: ../../h/quipu/attr.h
ap.o: ../../h/quipu/attrvalue.h
ap.o: ../../h/quipu/authen.h
ap.o: ../../h/quipu/bind.h
ap.o: ../../h/quipu/config.h
ap.o: ../../h/quipu/dsp.h
ap.o: ../../h/quipu/entry.h
ap.o: ../../h/quipu/name.h
ap.o: ../../h/quipu/nrs_info.h
ap.o: ../../h/quipu/oid.h
ap.o: ../../h/quipu/util.h
ap.o:../x500as/DO-types.h
ap.o:../x500as/DO_defs.h
ap.o:../x500as/DO_pre_defs.h
as_cmp.o: ../../h/config.h
as_cmp.o: ../../h/general.h
as_cmp.o: ../../h/logger.h
as_cmp.o: ../../h/manifest.h
as_cmp.o: ../../h/psap.h
as_cmp.o: ../../h/quipu/attr.h
as_cmp.o: ../../h/quipu/attrvalue.h
as_cmp.o: ../../h/quipu/config.h
as_cmp.o: ../../h/quipu/name.h
as_cmp.o: ../../h/quipu/oid.h
as_cmp.o: ../../h/quipu/util.h
as_cpy.o: ../../h/config.h
as_cpy.o: ../../h/general.h
as_cpy.o: ../../h/logger.h
as_cpy.o: ../../h/manifest.h
as_cpy.o: ../../h/psap.h
as_cpy.o: ../../h/quipu/attr.h
as_cpy.o: ../../h/quipu/attrvalue.h
as_cpy.o: ../../h/quipu/config.h
as_cpy.o: ../../h/quipu/malloc.h
as_cpy.o: ../../h/quipu/name.h
as_cpy.o: ../../h/quipu/oid.h
as_cpy.o: ../../h/quipu/util.h
as_del.o: ../../h/config.h
as_del.o: ../../h/general.h
as_del.o: ../../h/logger.h
as_del.o: ../../h/manifest.h
as_del.o: ../../h/psap.h
as_del.o: ../../h/quipu/attr.h
as_del.o: ../../h/quipu/attrvalue.h
as_del.o: ../../h/quipu/config.h
as_del.o: ../../h/quipu/name.h
as_del.o: ../../h/quipu/oid.h
as_del.o: ../../h/quipu/util.h
as_find.o: ../../h/config.h
as_find.o: ../../h/general.h
as_find.o: ../../h/logger.h
as_find.o: ../../h/manifest.h
as_find.o: ../../h/psap.h
as_find.o: ../../h/quipu/attr.h
as_find.o: ../../h/quipu/attrvalue.h
as_find.o: ../../h/quipu/config.h
as_find.o: ../../h/quipu/name.h
as_find.o: ../../h/quipu/oid.h
as_find.o: ../../h/quipu/util.h
as_free.o: ../../h/config.h
as_free.o: ../../h/general.h
as_free.o: ../../h/logger.h
as_free.o: ../../h/manifest.h
as_free.o: ../../h/psap.h
as_free.o: ../../h/quipu/attr.h
as_free.o: ../../h/quipu/attrvalue.h
as_free.o: ../../h/quipu/config.h
as_free.o: ../../h/quipu/name.h
as_free.o: ../../h/quipu/oid.h
as_free.o: ../../h/quipu/util.h
as_merge.o: ../../h/config.h
as_merge.o: ../../h/general.h
as_merge.o: ../../h/logger.h
as_merge.o: ../../h/manifest.h
as_merge.o: ../../h/psap.h
as_merge.o: ../../h/quipu/attr.h
as_merge.o: ../../h/quipu/attrvalue.h
as_merge.o: ../../h/quipu/config.h
as_merge.o: ../../h/quipu/name.h
as_merge.o: ../../h/quipu/oid.h
as_merge.o: ../../h/quipu/util.h
as_new.o: ../../h/config.h
as_new.o: ../../h/general.h
as_new.o: ../../h/logger.h
as_new.o: ../../h/manifest.h
as_new.o: ../../h/psap.h
as_new.o: ../../h/quipu/attr.h
as_new.o: ../../h/quipu/attrvalue.h
as_new.o: ../../h/quipu/config.h
as_new.o: ../../h/quipu/name.h
as_new.o: ../../h/quipu/oid.h
as_new.o: ../../h/quipu/util.h
as_print.o: ../../h/config.h
as_print.o: ../../h/general.h
as_print.o: ../../h/logger.h
as_print.o: ../../h/manifest.h
as_print.o: ../../h/psap.h
as_print.o: ../../h/quipu/attr.h
as_print.o: ../../h/quipu/attrvalue.h
as_print.o: ../../h/quipu/config.h
as_print.o: ../../h/quipu/name.h
as_print.o: ../../h/quipu/oid.h
as_print.o: ../../h/quipu/util.h
as_str.o: ../../h/config.h
as_str.o: ../../h/general.h
as_str.o: ../../h/isoaddrs.h
as_str.o: ../../h/logger.h
as_str.o: ../../h/manifest.h
as_str.o: ../../h/psap.h
as_str.o: ../../h/quipu/attr.h
as_str.o: ../../h/quipu/attrvalue.h
as_str.o: ../../h/quipu/config.h
as_str.o: ../../h/quipu/dsp.h
as_str.o: ../../h/quipu/entry.h
as_str.o: ../../h/quipu/malloc.h
as_str.o: ../../h/quipu/name.h
as_str.o: ../../h/quipu/oid.h
as_str.o: ../../h/quipu/util.h
attrt_cmp.o: ../../h/config.h
attrt_cmp.o: ../../h/general.h
attrt_cmp.o: ../../h/logger.h
attrt_cmp.o: ../../h/manifest.h
attrt_cmp.o: ../../h/psap.h
attrt_cmp.o: ../../h/quipu/attr.h
attrt_cmp.o: ../../h/quipu/config.h
attrt_cmp.o: ../../h/quipu/name.h
attrt_cmp.o: ../../h/quipu/oid.h
attrt_cmp.o: ../../h/quipu/util.h
attrt_cpy.o: ../../h/config.h
attrt_cpy.o: ../../h/general.h
attrt_cpy.o: ../../h/logger.h
attrt_cpy.o: ../../h/manifest.h
attrt_cpy.o: ../../h/psap.h
attrt_cpy.o: ../../h/quipu/attr.h
attrt_cpy.o: ../../h/quipu/config.h
attrt_cpy.o: ../../h/quipu/malloc.h
attrt_cpy.o: ../../h/quipu/name.h
attrt_cpy.o: ../../h/quipu/oid.h
attrt_cpy.o: ../../h/quipu/util.h
attrt_dec.o: ../../h/UNIV-types.h
attrt_dec.o: ../../h/UNIV_defs.h
attrt_dec.o: ../../h/UNIV_pre_defs.h
attrt_dec.o: ../../h/config.h
attrt_dec.o: ../../h/general.h
attrt_dec.o: ../../h/isoaddrs.h
attrt_dec.o: ../../h/logger.h
attrt_dec.o: ../../h/manifest.h
attrt_dec.o: ../../h/pepsy.h
attrt_dec.o: ../../h/psap.h
attrt_dec.o: ../../h/quipu/attr.h
attrt_dec.o: ../../h/quipu/attrvalue.h
attrt_dec.o: ../../h/quipu/authen.h
attrt_dec.o: ../../h/quipu/bind.h
attrt_dec.o: ../../h/quipu/config.h
attrt_dec.o: ../../h/quipu/dsp.h
attrt_dec.o: ../../h/quipu/entry.h
attrt_dec.o: ../../h/quipu/name.h
attrt_dec.o: ../../h/quipu/nrs_info.h
attrt_dec.o: ../../h/quipu/oid.h
attrt_dec.o: ../../h/quipu/util.h
attrt_dec.o:../x500as/AF-types.h
attrt_dec.o:../x500as/AF_defs.h
attrt_dec.o:../x500as/AF_pre_defs.h
attrt_dec.o:../x500as/IF-types.h
attrt_dec.o:../x500as/IF_defs.h
attrt_dec.o:../x500as/IF_pre_defs.h
attrt_dec.o:../x500as/NRS-types.h
attrt_dec.o:../x500as/NRS_defs.h
attrt_dec.o:../x500as/NRS_pre_defs.h
attrt_dec.o:../x500as/Quipu-types.h
attrt_dec.o:../x500as/Quipu_defs.h
attrt_dec.o:../x500as/Quipu_pre_defs.h
attrt_dec.o:../x500as/SA-types.h
attrt_dec.o:../x500as/SA_defs.h
attrt_dec.o:../x500as/SA_pre_defs.h
attrt_dec.o:../x500as/Thorn-types.h
attrt_dec.o:../x500as/Thorn_defs.h
attrt_dec.o:../x500as/Thorn_pre_defs.h
attrt_dec.o:../x500as/af-cdefs.h
attrt_dec.o:../x500as/if-cdefs.h
attrt_dec.o:../x500as/nrs-cdefs.h
attrt_dec.o:../x500as/qu-cdefs.h
attrt_dec.o:../../h/quipu/syntaxes.h
attrt_free.o: ../../h/config.h
attrt_free.o: ../../h/general.h
attrt_free.o: ../../h/logger.h
attrt_free.o: ../../h/manifest.h
attrt_free.o: ../../h/psap.h
attrt_free.o: ../../h/quipu/attr.h
attrt_free.o: ../../h/quipu/config.h
attrt_free.o: ../../h/quipu/name.h
attrt_free.o: ../../h/quipu/oid.h
attrt_free.o: ../../h/quipu/util.h
attrt_new.o: ../../h/config.h
attrt_new.o: ../../h/general.h
attrt_new.o: ../../h/logger.h
attrt_new.o: ../../h/manifest.h
attrt_new.o: ../../h/psap.h
attrt_new.o: ../../h/quipu/attr.h
attrt_new.o: ../../h/quipu/config.h
attrt_new.o: ../../h/quipu/name.h
attrt_new.o: ../../h/quipu/oid.h
attrt_new.o: ../../h/quipu/util.h
attrt_print.o: ../../h/config.h
attrt_print.o: ../../h/general.h
attrt_print.o: ../../h/logger.h
attrt_print.o: ../../h/manifest.h
attrt_print.o: ../../h/psap.h
attrt_print.o: ../../h/quipu/attr.h
attrt_print.o: ../../h/quipu/config.h
attrt_print.o: ../../h/quipu/name.h
attrt_print.o: ../../h/quipu/oid.h
attrt_print.o: ../../h/quipu/util.h
attrv.o: ../../h/cmd_srch.h
attrv.o: ../../h/config.h
attrv.o: ../../h/general.h
attrv.o: ../../h/isoaddrs.h
attrv.o: ../../h/logger.h
attrv.o: ../../h/manifest.h
attrv.o: ../../h/psap.h
attrv.o: ../../h/quipu/attr.h
attrv.o: ../../h/quipu/attrvalue.h
attrv.o: ../../h/quipu/config.h
attrv.o: ../../h/quipu/ds_error.h
attrv.o: ../../h/quipu/dsp.h
attrv.o: ../../h/quipu/entry.h
attrv.o: ../../h/quipu/malloc.h
attrv.o: ../../h/quipu/name.h
attrv.o: ../../h/quipu/oid.h
attrv.o: ../../h/quipu/util.h
audio.o: ../../h/config.h
audio.o: ../../h/general.h
audio.o: ../../h/logger.h
audio.o: ../../h/manifest.h
audio.o: ../../h/psap.h
audio.o: ../../h/quipu/attr.h
audio.o: ../../h/quipu/config.h
audio.o: ../../h/quipu/oid.h
audio.o: ../../h/quipu/util.h
avs_cmp.o: ../../h/config.h
avs_cmp.o: ../../h/general.h
avs_cmp.o: ../../h/isoaddrs.h
avs_cmp.o: ../../h/logger.h
avs_cmp.o: ../../h/manifest.h
avs_cmp.o: ../../h/psap.h
avs_cmp.o: ../../h/quipu/attr.h
avs_cmp.o: ../../h/quipu/attrvalue.h
avs_cmp.o: ../../h/quipu/config.h
avs_cmp.o: ../../h/quipu/ds_error.h
avs_cmp.o: ../../h/quipu/dsp.h
avs_cmp.o: ../../h/quipu/name.h
avs_cmp.o: ../../h/quipu/oid.h
avs_cmp.o: ../../h/quipu/util.h
avs_cpy.o: ../../h/config.h
avs_cpy.o: ../../h/general.h
avs_cpy.o: ../../h/logger.h
avs_cpy.o: ../../h/manifest.h
avs_cpy.o: ../../h/psap.h
avs_cpy.o: ../../h/quipu/attr.h
avs_cpy.o: ../../h/quipu/attrvalue.h
avs_cpy.o: ../../h/quipu/config.h
avs_cpy.o: ../../h/quipu/name.h
avs_cpy.o: ../../h/quipu/oid.h
avs_cpy.o: ../../h/quipu/util.h
avs_del.o: ../../h/config.h
avs_del.o: ../../h/general.h
avs_del.o: ../../h/logger.h
avs_del.o: ../../h/manifest.h
avs_del.o: ../../h/psap.h
avs_del.o: ../../h/quipu/attr.h
avs_del.o: ../../h/quipu/attrvalue.h
avs_del.o: ../../h/quipu/config.h
avs_del.o: ../../h/quipu/name.h
avs_del.o: ../../h/quipu/oid.h
avs_del.o: ../../h/quipu/util.h
avs_free.o: ../../h/config.h
avs_free.o: ../../h/general.h
avs_free.o: ../../h/logger.h
avs_free.o: ../../h/manifest.h
avs_free.o: ../../h/psap.h
avs_free.o: ../../h/quipu/attr.h
avs_free.o: ../../h/quipu/attrvalue.h
avs_free.o: ../../h/quipu/config.h
avs_free.o: ../../h/quipu/name.h
avs_free.o: ../../h/quipu/oid.h
avs_free.o: ../../h/quipu/util.h
avs_merge.o: ../../h/config.h
avs_merge.o: ../../h/general.h
avs_merge.o: ../../h/isoaddrs.h
avs_merge.o: ../../h/logger.h
avs_merge.o: ../../h/manifest.h
avs_merge.o: ../../h/psap.h
avs_merge.o: ../../h/quipu/attr.h
avs_merge.o: ../../h/quipu/attrvalue.h
avs_merge.o: ../../h/quipu/config.h
avs_merge.o: ../../h/quipu/ds_error.h
avs_merge.o: ../../h/quipu/dsp.h
avs_merge.o: ../../h/quipu/name.h
avs_merge.o: ../../h/quipu/oid.h
avs_merge.o: ../../h/quipu/util.h
avs_new.o: ../../h/config.h
avs_new.o: ../../h/general.h
avs_new.o: ../../h/logger.h
avs_new.o: ../../h/manifest.h
avs_new.o: ../../h/psap.h
avs_new.o: ../../h/quipu/attr.h
avs_new.o: ../../h/quipu/attrvalue.h
avs_new.o: ../../h/quipu/config.h
avs_new.o: ../../h/quipu/name.h
avs_new.o: ../../h/quipu/oid.h
avs_new.o: ../../h/quipu/util.h
avs_print.o: ../../h/config.h
avs_print.o: ../../h/general.h
avs_print.o: ../../h/logger.h
avs_print.o: ../../h/manifest.h
avs_print.o: ../../h/psap.h
avs_print.o: ../../h/quipu/attr.h
avs_print.o: ../../h/quipu/attrvalue.h
avs_print.o: ../../h/quipu/config.h
avs_print.o: ../../h/quipu/name.h
avs_print.o: ../../h/quipu/oid.h
avs_print.o: ../../h/quipu/util.h
avs_str.o: ../../h/config.h
avs_str.o: ../../h/general.h
avs_str.o: ../../h/logger.h
avs_str.o: ../../h/manifest.h
avs_str.o: ../../h/psap.h
avs_str.o: ../../h/quipu/attr.h
avs_str.o: ../../h/quipu/attrvalue.h
avs_str.o: ../../h/quipu/config.h
avs_str.o: ../../h/quipu/name.h
avs_str.o: ../../h/quipu/oid.h
avs_str.o: ../../h/quipu/util.h
boolean.o: ../../h/config.h
boolean.o: ../../h/general.h
boolean.o: ../../h/logger.h
boolean.o: ../../h/manifest.h
boolean.o: ../../h/psap.h
boolean.o: ../../h/quipu/attr.h
boolean.o: ../../h/quipu/config.h
boolean.o: ../../h/quipu/oid.h
boolean.o: ../../h/quipu/util.h
ca.o: ../../h/config.h
ca.o: ../../h/general.h
ca.o: ../../h/isoaddrs.h
ca.o: ../../h/logger.h
ca.o: ../../h/manifest.h
ca.o: ../../h/psap.h
ca.o: ../../h/quipu/abandon.h
ca.o: ../../h/quipu/add.h
ca.o: ../../h/quipu/attr.h
ca.o: ../../h/quipu/attrvalue.h
ca.o: ../../h/quipu/authen.h
ca.o: ../../h/quipu/bind.h
ca.o: ../../h/quipu/common.h
ca.o: ../../h/quipu/commonarg.h
ca.o: ../../h/quipu/compare.h
ca.o: ../../h/quipu/config.h
ca.o: ../../h/quipu/dap.h
ca.o: ../../h/quipu/ds_error.h
ca.o: ../../h/quipu/ds_search.h
ca.o: ../../h/quipu/dsargument.h
ca.o: ../../h/quipu/dsp.h
ca.o: ../../h/quipu/entry.h
ca.o: ../../h/quipu/list.h
ca.o: ../../h/quipu/modify.h
ca.o: ../../h/quipu/modifyrdn.h
ca.o: ../../h/quipu/name.h
ca.o: ../../h/quipu/oid.h
ca.o: ../../h/quipu/read.h
ca.o: ../../h/quipu/remove.h
ca.o: ../../h/quipu/util.h
cache.o: ../../h/config.h
cache.o: ../../h/general.h
cache.o: ../../h/isoaddrs.h
cache.o: ../../h/logger.h
cache.o: ../../h/manifest.h
cache.o: ../../h/psap.h
cache.o: ../../h/quipu/attr.h
cache.o: ../../h/quipu/attrvalue.h
cache.o: ../../h/quipu/authen.h
cache.o: ../../h/quipu/commonarg.h
cache.o: ../../h/quipu/config.h
cache.o: ../../h/quipu/dap.h
cache.o: ../../h/quipu/ds_error.h
cache.o: ../../h/quipu/dsp.h
cache.o: ../../h/quipu/dua.h
cache.o: ../../h/quipu/entry.h
cache.o: ../../h/quipu/list.h
cache.o: ../../h/quipu/name.h
cache.o: ../../h/quipu/oid.h
cache.o: ../../h/quipu/util.h
certificate.o: ../../h/UNIV-types.h
certificate.o: ../../h/UNIV_defs.h
certificate.o: ../../h/UNIV_pre_defs.h
certificate.o: ../../h/config.h
certificate.o: ../../h/general.h
certificate.o: ../../h/isoaddrs.h
certificate.o: ../../h/logger.h
certificate.o: ../../h/manifest.h
certificate.o: ../../h/pepsy.h
certificate.o: ../../h/psap.h
certificate.o: ../../h/quipu/attr.h
certificate.o: ../../h/quipu/attrvalue.h
certificate.o: ../../h/quipu/authen.h
certificate.o: ../../h/quipu/bind.h
certificate.o: ../../h/quipu/config.h
certificate.o: ../../h/quipu/dsp.h
certificate.o: ../../h/quipu/entry.h
certificate.o: ../../h/quipu/name.h
certificate.o: ../../h/quipu/nrs_info.h
certificate.o: ../../h/quipu/oid.h
certificate.o: ../../h/quipu/util.h
certificate.o:../x500as/AF-types.h
certificate.o:../x500as/AF_defs.h
certificate.o:../x500as/AF_pre_defs.h
certificate.o:../x500as/IF-types.h
certificate.o:../x500as/IF_defs.h
certificate.o:../x500as/IF_pre_defs.h
certificate.o:../x500as/NRS-types.h
certificate.o:../x500as/NRS_defs.h
certificate.o:../x500as/NRS_pre_defs.h
certificate.o:../x500as/Quipu-types.h
certificate.o:../x500as/Quipu_defs.h
certificate.o:../x500as/Quipu_pre_defs.h
certificate.o:../x500as/SA-types.h
certificate.o:../x500as/SA_defs.h
certificate.o:../x500as/SA_pre_defs.h
certificate.o:../x500as/Thorn-types.h
certificate.o:../x500as/Thorn_defs.h
certificate.o:../x500as/Thorn_pre_defs.h
certificate.o:../x500as/af-cdefs.h
certificate.o:../x500as/if-cdefs.h
certificate.o:../x500as/nrs-cdefs.h
certificate.o:../x500as/qu-cdefs.h
certificate.o:../../h/quipu/syntaxes.h
cilist.o: ../../h/UNIV-types.h
cilist.o: ../../h/UNIV_defs.h
cilist.o: ../../h/UNIV_pre_defs.h
cilist.o: ../../h/config.h
cilist.o: ../../h/general.h
cilist.o: ../../h/isoaddrs.h
cilist.o: ../../h/logger.h
cilist.o: ../../h/manifest.h
cilist.o: ../../h/pepsy.h
cilist.o: ../../h/psap.h
cilist.o: ../../h/quipu/attr.h
cilist.o: ../../h/quipu/attrvalue.h
cilist.o: ../../h/quipu/authen.h
cilist.o: ../../h/quipu/bind.h
cilist.o: ../../h/quipu/config.h
cilist.o: ../../h/quipu/dsp.h
cilist.o: ../../h/quipu/entry.h
cilist.o: ../../h/quipu/name.h
cilist.o: ../../h/quipu/nrs_info.h
cilist.o: ../../h/quipu/oid.h
cilist.o: ../../h/quipu/util.h
cilist.o:../x500as/AF-types.h
cilist.o:../x500as/AF_defs.h
cilist.o:../x500as/AF_pre_defs.h
cilist.o:../x500as/IF-types.h
cilist.o:../x500as/IF_defs.h
cilist.o:../x500as/IF_pre_defs.h
cilist.o:../x500as/NRS-types.h
cilist.o:../x500as/NRS_defs.h
cilist.o:../x500as/NRS_pre_defs.h
cilist.o:../x500as/Quipu-types.h
cilist.o:../x500as/Quipu_defs.h
cilist.o:../x500as/Quipu_pre_defs.h
cilist.o:../x500as/SA-types.h
cilist.o:../x500as/SA_defs.h
cilist.o:../x500as/SA_pre_defs.h
cilist.o:../x500as/Thorn-types.h
cilist.o:../x500as/Thorn_defs.h
cilist.o:../x500as/Thorn_pre_defs.h
cilist.o:../x500as/af-cdefs.h
cilist.o:../x500as/if-cdefs.h
cilist.o:../x500as/nrs-cdefs.h
cilist.o:../x500as/qu-cdefs.h
cilist.o:../../h/quipu/syntaxes.h
conf.o: ../../h/config.h
conf.o: ../../h/general.h
conf.o: ../../h/logger.h
conf.o: ../../h/manifest.h
conf.o: ../../h/quipu/config.h
conf.o: ../../h/quipu/util.h
conf.o: ../../h/tailor.h
correlate.o: ../../h/config.h
correlate.o: ../../h/general.h
correlate.o: ../../h/isoaddrs.h
correlate.o: ../../h/logger.h
correlate.o: ../../h/manifest.h
correlate.o: ../../h/psap.h
correlate.o: ../../h/quipu/attr.h
correlate.o: ../../h/quipu/attrvalue.h
correlate.o: ../../h/quipu/authen.h
correlate.o: ../../h/quipu/commonarg.h
correlate.o: ../../h/quipu/config.h
correlate.o: ../../h/quipu/dap.h
correlate.o: ../../h/quipu/ds_error.h
correlate.o: ../../h/quipu/ds_search.h
correlate.o: ../../h/quipu/dsp.h
correlate.o: ../../h/quipu/list.h
correlate.o: ../../h/quipu/name.h
correlate.o: ../../h/quipu/oid.h
correlate.o: ../../h/quipu/util.h
cpair.o: ../../h/UNIV-types.h
cpair.o: ../../h/UNIV_defs.h
cpair.o: ../../h/UNIV_pre_defs.h
cpair.o: ../../h/config.h
cpair.o: ../../h/general.h
cpair.o: ../../h/isoaddrs.h
cpair.o: ../../h/logger.h
cpair.o: ../../h/manifest.h
cpair.o: ../../h/pepsy.h
cpair.o: ../../h/psap.h
cpair.o: ../../h/quipu/attr.h
cpair.o: ../../h/quipu/attrvalue.h
cpair.o: ../../h/quipu/authen.h
cpair.o: ../../h/quipu/bind.h
cpair.o: ../../h/quipu/config.h
cpair.o: ../../h/quipu/dsp.h
cpair.o: ../../h/quipu/entry.h
cpair.o: ../../h/quipu/name.h
cpair.o: ../../h/quipu/nrs_info.h
cpair.o: ../../h/quipu/oid.h
cpair.o: ../../h/quipu/util.h
cpair.o:../x500as/AF-types.h
cpair.o:../x500as/AF_defs.h
cpair.o:../x500as/AF_pre_defs.h
cpair.o:../x500as/IF-types.h
cpair.o:../x500as/IF_defs.h
cpair.o:../x500as/IF_pre_defs.h
cpair.o:../x500as/NRS-types.h
cpair.o:../x500as/NRS_defs.h
cpair.o:../x500as/NRS_pre_defs.h
cpair.o:../x500as/Quipu-types.h
cpair.o:../x500as/Quipu_defs.h
cpair.o:../x500as/Quipu_pre_defs.h
cpair.o:../x500as/SA-types.h
cpair.o:../x500as/SA_defs.h
cpair.o:../x500as/SA_pre_defs.h
cpair.o:../x500as/Thorn-types.h
cpair.o:../x500as/Thorn_defs.h
cpair.o:../x500as/Thorn_pre_defs.h
cpair.o:../x500as/af-cdefs.h
cpair.o:../x500as/if-cdefs.h
cpair.o:../x500as/nrs-cdefs.h
cpair.o:../x500as/qu-cdefs.h
cpair.o:../../h/quipu/syntaxes.h
cstrings.o: ../../h/config.h
cstrings.o: ../../h/general.h
cstrings.o: ../../h/manifest.h
cstrings.o: ../../h/psap.h
dn_append.o: ../../h/config.h
dn_append.o: ../../h/general.h
dn_append.o: ../../h/logger.h
dn_append.o: ../../h/manifest.h
dn_append.o: ../../h/psap.h
dn_append.o: ../../h/quipu/attr.h
dn_append.o: ../../h/quipu/config.h
dn_append.o: ../../h/quipu/name.h
dn_append.o: ../../h/quipu/oid.h
dn_append.o: ../../h/quipu/util.h
dn_cmp.o: ../../h/config.h
dn_cmp.o: ../../h/general.h
dn_cmp.o: ../../h/logger.h
dn_cmp.o: ../../h/manifest.h
dn_cmp.o: ../../h/psap.h
dn_cmp.o: ../../h/quipu/attr.h
dn_cmp.o: ../../h/quipu/config.h
dn_cmp.o: ../../h/quipu/name.h
dn_cmp.o: ../../h/quipu/oid.h
dn_cmp.o: ../../h/quipu/util.h
dn_cpy.o: ../../h/config.h
dn_cpy.o: ../../h/general.h
dn_cpy.o: ../../h/logger.h
dn_cpy.o: ../../h/manifest.h
dn_cpy.o: ../../h/psap.h
dn_cpy.o: ../../h/quipu/attr.h
dn_cpy.o: ../../h/quipu/config.h
dn_cpy.o: ../../h/quipu/name.h
dn_cpy.o: ../../h/quipu/oid.h
dn_cpy.o: ../../h/quipu/util.h
dn_free.o: ../../h/config.h
dn_free.o: ../../h/general.h
dn_free.o: ../../h/logger.h
dn_free.o: ../../h/manifest.h
dn_free.o: ../../h/psap.h
dn_free.o: ../../h/quipu/attr.h
dn_free.o: ../../h/quipu/config.h
dn_free.o: ../../h/quipu/name.h
dn_free.o: ../../h/quipu/oid.h
dn_free.o: ../../h/quipu/util.h
dn_new.o: ../../h/config.h
dn_new.o: ../../h/general.h
dn_new.o: ../../h/logger.h
dn_new.o: ../../h/manifest.h
dn_new.o: ../../h/psap.h
dn_new.o: ../../h/quipu/attr.h
dn_new.o: ../../h/quipu/config.h
dn_new.o: ../../h/quipu/name.h
dn_new.o: ../../h/quipu/oid.h
dn_new.o: ../../h/quipu/util.h
dn_print.o: ../../h/config.h
dn_print.o: ../../h/general.h
dn_print.o: ../../h/logger.h
dn_print.o: ../../h/manifest.h
dn_print.o: ../../h/psap.h
dn_print.o: ../../h/quipu/attr.h
dn_print.o: ../../h/quipu/config.h
dn_print.o: ../../h/quipu/name.h
dn_print.o: ../../h/quipu/oid.h
dn_print.o: ../../h/quipu/util.h
dn_seq.o: ../../h/config.h
dn_seq.o: ../../h/general.h
dn_seq.o: ../../h/isoaddrs.h
dn_seq.o: ../../h/logger.h
dn_seq.o: ../../h/manifest.h
dn_seq.o: ../../h/psap.h
dn_seq.o: ../../h/quipu/attr.h
dn_seq.o: ../../h/quipu/attrvalue.h
dn_seq.o: ../../h/quipu/config.h
dn_seq.o: ../../h/quipu/dsp.h
dn_seq.o: ../../h/quipu/entry.h
dn_seq.o: ../../h/quipu/name.h
dn_seq.o: ../../h/quipu/oid.h
dn_seq.o: ../../h/quipu/util.h
dn_str.o: ../../h/config.h
dn_str.o: ../../h/general.h
dn_str.o: ../../h/logger.h
dn_str.o: ../../h/manifest.h
dn_str.o: ../../h/psap.h
dn_str.o: ../../h/quipu/attr.h
dn_str.o: ../../h/quipu/config.h
dn_str.o: ../../h/quipu/name.h
dn_str.o: ../../h/quipu/oid.h
dn_str.o: ../../h/quipu/util.h
docstore.o: ../../h/UNIV-types.h
docstore.o: ../../h/UNIV_defs.h
docstore.o: ../../h/UNIV_pre_defs.h
docstore.o: ../../h/config.h
docstore.o: ../../h/general.h
docstore.o: ../../h/isoaddrs.h
docstore.o: ../../h/logger.h
docstore.o: ../../h/manifest.h
docstore.o: ../../h/pepsy.h
docstore.o: ../../h/psap.h
docstore.o: ../../h/quipu/attr.h
docstore.o: ../../h/quipu/attrvalue.h
docstore.o: ../../h/quipu/authen.h
docstore.o: ../../h/quipu/bind.h
docstore.o: ../../h/quipu/config.h
docstore.o: ../../h/quipu/dsp.h
docstore.o: ../../h/quipu/entry.h
docstore.o: ../../h/quipu/name.h
docstore.o: ../../h/quipu/nrs_info.h
docstore.o: ../../h/quipu/oid.h
docstore.o: ../../h/quipu/util.h
docstore.o:../x500as/AF-types.h
docstore.o:../x500as/AF_defs.h
docstore.o:../x500as/AF_pre_defs.h
docstore.o:../x500as/IF-types.h
docstore.o:../x500as/IF_defs.h
docstore.o:../x500as/IF_pre_defs.h
docstore.o:../x500as/NRS-types.h
docstore.o:../x500as/NRS_defs.h
docstore.o:../x500as/NRS_pre_defs.h
docstore.o:../x500as/Quipu-types.h
docstore.o:../x500as/Quipu_defs.h
docstore.o:../x500as/Quipu_pre_defs.h
docstore.o:../x500as/SA-types.h
docstore.o:../x500as/SA_defs.h
docstore.o:../x500as/SA_pre_defs.h
docstore.o:../x500as/Thorn-types.h
docstore.o:../x500as/Thorn_defs.h
docstore.o:../x500as/Thorn_pre_defs.h
docstore.o:../x500as/af-cdefs.h
docstore.o:../x500as/if-cdefs.h
docstore.o:../x500as/nrs-cdefs.h
docstore.o:../x500as/qu-cdefs.h
docstore.o:../../h/quipu/syntaxes.h
ds_error.o: ../../h/config.h
ds_error.o: ../../h/general.h
ds_error.o: ../../h/isoaddrs.h
ds_error.o: ../../h/logger.h
ds_error.o: ../../h/manifest.h
ds_error.o: ../../h/psap.h
ds_error.o: ../../h/quipu/attr.h
ds_error.o: ../../h/quipu/authen.h
ds_error.o: ../../h/quipu/bind.h
ds_error.o: ../../h/quipu/config.h
ds_error.o: ../../h/quipu/ds_error.h
ds_error.o: ../../h/quipu/dsp.h
ds_error.o: ../../h/quipu/name.h
ds_error.o: ../../h/quipu/oid.h
ds_error.o: ../../h/quipu/util.h
ds_ext.o: ../../h/config.h
ds_ext.o: ../../h/general.h
ds_ext.o: ../../h/isoaddrs.h
ds_ext.o: ../../h/logger.h
ds_ext.o: ../../h/manifest.h
ds_ext.o: ../../h/psap.h
ds_ext.o: ../../h/quipu/abandon.h
ds_ext.o: ../../h/quipu/add.h
ds_ext.o: ../../h/quipu/attr.h
ds_ext.o: ../../h/quipu/attrvalue.h
ds_ext.o: ../../h/quipu/authen.h
ds_ext.o: ../../h/quipu/bind.h
ds_ext.o: ../../h/quipu/common.h
ds_ext.o: ../../h/quipu/commonarg.h
ds_ext.o: ../../h/quipu/compare.h
ds_ext.o: ../../h/quipu/config.h
ds_ext.o: ../../h/quipu/dap.h
ds_ext.o: ../../h/quipu/ds_error.h
ds_ext.o: ../../h/quipu/ds_search.h
ds_ext.o: ../../h/quipu/dsp.h
ds_ext.o: ../../h/quipu/list.h
ds_ext.o: ../../h/quipu/modify.h
ds_ext.o: ../../h/quipu/modifyrdn.h
ds_ext.o: ../../h/quipu/name.h
ds_ext.o: ../../h/quipu/oid.h
ds_ext.o: ../../h/quipu/read.h
ds_ext.o: ../../h/quipu/remove.h
ds_ext.o: ../../h/quipu/util.h
dsa_info.o: ../../h/UNIV-types.h
dsa_info.o: ../../h/UNIV_defs.h
dsa_info.o: ../../h/UNIV_pre_defs.h
dsa_info.o: ../../h/config.h
dsa_info.o: ../../h/general.h
dsa_info.o: ../../h/isoaddrs.h
dsa_info.o: ../../h/logger.h
dsa_info.o: ../../h/manifest.h
dsa_info.o: ../../h/pepsy.h
dsa_info.o: ../../h/psap.h
dsa_info.o: ../../h/quipu/attr.h
dsa_info.o: ../../h/quipu/attrvalue.h
dsa_info.o: ../../h/quipu/authen.h
dsa_info.o: ../../h/quipu/bind.h
dsa_info.o: ../../h/quipu/config.h
dsa_info.o: ../../h/quipu/dsp.h
dsa_info.o: ../../h/quipu/entry.h
dsa_info.o: ../../h/quipu/name.h
dsa_info.o: ../../h/quipu/nrs_info.h
dsa_info.o: ../../h/quipu/oid.h
dsa_info.o: ../../h/quipu/util.h
dsa_info.o:../x500as/AF-types.h
dsa_info.o:../x500as/AF_defs.h
dsa_info.o:../x500as/AF_pre_defs.h
dsa_info.o:../x500as/IF-types.h
dsa_info.o:../x500as/IF_defs.h
dsa_info.o:../x500as/IF_pre_defs.h
dsa_info.o:../x500as/NRS-types.h
dsa_info.o:../x500as/NRS_defs.h
dsa_info.o:../x500as/NRS_pre_defs.h
dsa_info.o:../x500as/Quipu-types.h
dsa_info.o:../x500as/Quipu_defs.h
dsa_info.o:../x500as/Quipu_pre_defs.h
dsa_info.o:../x500as/SA-types.h
dsa_info.o:../x500as/SA_defs.h
dsa_info.o:../x500as/SA_pre_defs.h
dsa_info.o:../x500as/Thorn-types.h
dsa_info.o:../x500as/Thorn_defs.h
dsa_info.o:../x500as/Thorn_pre_defs.h
dsa_info.o:../x500as/af-cdefs.h
dsa_info.o:../x500as/if-cdefs.h
dsa_info.o:../x500as/nrs-cdefs.h
dsa_info.o:../x500as/qu-cdefs.h
dsa_info.o:../../h/quipu/syntaxes.h
entry.o: ../../h/config.h
entry.o: ../../h/general.h
entry.o: ../../h/isoaddrs.h
entry.o: ../../h/logger.h
entry.o: ../../h/manifest.h
entry.o: ../../h/psap.h
entry.o: ../../h/quipu/attr.h
entry.o: ../../h/quipu/attrvalue.h
entry.o: ../../h/quipu/config.h
entry.o: ../../h/quipu/dsp.h
entry.o: ../../h/quipu/entry.h
entry.o: ../../h/quipu/name.h
entry.o: ../../h/quipu/oid.h
entry.o: ../../h/quipu/util.h
entryinfo.o: ../../h/config.h
entryinfo.o: ../../h/general.h
entryinfo.o: ../../h/isoaddrs.h
entryinfo.o: ../../h/logger.h
entryinfo.o: ../../h/manifest.h
entryinfo.o: ../../h/psap.h
entryinfo.o: ../../h/quipu/attr.h
entryinfo.o: ../../h/quipu/attrvalue.h
entryinfo.o: ../../h/quipu/authen.h
entryinfo.o: ../../h/quipu/commonarg.h
entryinfo.o: ../../h/quipu/config.h
entryinfo.o: ../../h/quipu/dsp.h
entryinfo.o: ../../h/quipu/name.h
entryinfo.o: ../../h/quipu/oid.h
entryinfo.o: ../../h/quipu/util.h
fax.o: ../../h/UNIV-types.h
fax.o: ../../h/UNIV_defs.h
fax.o: ../../h/UNIV_pre_defs.h
fax.o: ../../h/config.h
fax.o: ../../h/general.h
fax.o: ../../h/isoaddrs.h
fax.o: ../../h/logger.h
fax.o: ../../h/manifest.h
fax.o: ../../h/pepsy.h
fax.o: ../../h/psap.h
fax.o: ../../h/quipu/attr.h
fax.o: ../../h/quipu/attrvalue.h
fax.o: ../../h/quipu/authen.h
fax.o: ../../h/quipu/bind.h
fax.o: ../../h/quipu/config.h
fax.o: ../../h/quipu/dsp.h
fax.o: ../../h/quipu/entry.h
fax.o: ../../h/quipu/name.h
fax.o: ../../h/quipu/nrs_info.h
fax.o: ../../h/quipu/oid.h
fax.o: ../../h/quipu/util.h
fax.o:../x500as/AF-types.h
fax.o:../x500as/AF_defs.h
fax.o:../x500as/AF_pre_defs.h
fax.o:../x500as/IF-types.h
fax.o:../x500as/IF_defs.h
fax.o:../x500as/IF_pre_defs.h
fax.o:../x500as/NRS-types.h
fax.o:../x500as/NRS_defs.h
fax.o:../x500as/NRS_pre_defs.h
fax.o:../x500as/Quipu-types.h
fax.o:../x500as/Quipu_defs.h
fax.o:../x500as/Quipu_pre_defs.h
fax.o:../x500as/SA-types.h
fax.o:../x500as/SA_defs.h
fax.o:../x500as/SA_pre_defs.h
fax.o:../x500as/Thorn-types.h
fax.o:../x500as/Thorn_defs.h
fax.o:../x500as/Thorn_pre_defs.h
fax.o:../x500as/af-cdefs.h
fax.o:../x500as/if-cdefs.h
fax.o:../x500as/nrs-cdefs.h
fax.o:../x500as/qu-cdefs.h
fax.o:../../h/quipu/syntaxes.h
file_cpy.o: ../../h/config.h
file_cpy.o: ../../h/general.h
file_cpy.o: ../../h/logger.h
file_cpy.o: ../../h/manifest.h
file_cpy.o: ../../h/psap.h
file_cpy.o: ../../h/quipu/attr.h
file_cpy.o: ../../h/quipu/attrvalue.h
file_cpy.o: ../../h/quipu/config.h
file_cpy.o: ../../h/quipu/name.h
file_cpy.o: ../../h/quipu/oid.h
file_cpy.o: ../../h/quipu/util.h
file_enc.o: ../../h/config.h
file_enc.o: ../../h/general.h
file_enc.o: ../../h/logger.h
file_enc.o: ../../h/manifest.h
file_enc.o: ../../h/psap.h
file_enc.o: ../../h/quipu/attr.h
file_enc.o: ../../h/quipu/attrvalue.h
file_enc.o: ../../h/quipu/config.h
file_enc.o: ../../h/quipu/name.h
file_enc.o: ../../h/quipu/oid.h
file_enc.o: ../../h/quipu/util.h
file_print.o: ../../h/config.h
file_print.o: ../../h/general.h
file_print.o: ../../h/logger.h
file_print.o: ../../h/manifest.h
file_print.o: ../../h/psap.h
file_print.o: ../../h/quipu/attr.h
file_print.o: ../../h/quipu/attrvalue.h
file_print.o: ../../h/quipu/config.h
file_print.o: ../../h/quipu/malloc.h
file_print.o: ../../h/quipu/name.h
file_print.o: ../../h/quipu/oid.h
file_print.o: ../../h/quipu/util.h
file_str.o: ../../h/config.h
file_str.o: ../../h/general.h
file_str.o: ../../h/logger.h
file_str.o: ../../h/manifest.h
file_str.o: ../../h/psap.h
file_str.o: ../../h/quipu/attr.h
file_str.o: ../../h/quipu/attrvalue.h
file_str.o: ../../h/quipu/config.h
file_str.o: ../../h/quipu/name.h
file_str.o: ../../h/quipu/oid.h
file_str.o: ../../h/quipu/util.h
filter.o: ../../h/config.h
filter.o: ../../h/general.h
filter.o: ../../h/isoaddrs.h
filter.o: ../../h/logger.h
filter.o: ../../h/manifest.h
filter.o: ../../h/psap.h
filter.o: ../../h/quipu/attr.h
filter.o: ../../h/quipu/attrvalue.h
filter.o: ../../h/quipu/authen.h
filter.o: ../../h/quipu/commonarg.h
filter.o: ../../h/quipu/config.h
filter.o: ../../h/quipu/dap.h
filter.o: ../../h/quipu/ds_error.h
filter.o: ../../h/quipu/ds_search.h
filter.o: ../../h/quipu/dsp.h
filter.o: ../../h/quipu/name.h
filter.o: ../../h/quipu/oid.h
filter.o: ../../h/quipu/util.h
guide.o: ../../h/UNIV-types.h
guide.o: ../../h/UNIV_defs.h
guide.o: ../../h/UNIV_pre_defs.h
guide.o: ../../h/cmd_srch.h
guide.o: ../../h/config.h
guide.o: ../../h/general.h
guide.o: ../../h/isoaddrs.h
guide.o: ../../h/logger.h
guide.o: ../../h/manifest.h
guide.o: ../../h/pepsy.h
guide.o: ../../h/psap.h
guide.o: ../../h/quipu/attr.h
guide.o: ../../h/quipu/attrvalue.h
guide.o: ../../h/quipu/authen.h
guide.o: ../../h/quipu/bind.h
guide.o: ../../h/quipu/config.h
guide.o: ../../h/quipu/dsp.h
guide.o: ../../h/quipu/entry.h
guide.o: ../../h/quipu/name.h
guide.o: ../../h/quipu/nrs_info.h
guide.o: ../../h/quipu/oid.h
guide.o: ../../h/quipu/util.h
guide.o:../x500as/AF-types.h
guide.o:../x500as/AF_defs.h
guide.o:../x500as/AF_pre_defs.h
guide.o:../x500as/IF-types.h
guide.o:../x500as/IF_defs.h
guide.o:../x500as/IF_pre_defs.h
guide.o:../x500as/NRS-types.h
guide.o:../x500as/NRS_defs.h
guide.o:../x500as/NRS_pre_defs.h
guide.o:../x500as/Quipu-types.h
guide.o:../x500as/Quipu_defs.h
guide.o:../x500as/Quipu_pre_defs.h
guide.o:../x500as/SA-types.h
guide.o:../x500as/SA_defs.h
guide.o:../x500as/SA_pre_defs.h
guide.o:../x500as/Thorn-types.h
guide.o:../x500as/Thorn_defs.h
guide.o:../x500as/Thorn_pre_defs.h
guide.o:../x500as/af-cdefs.h
guide.o:../x500as/if-cdefs.h
guide.o:../x500as/nrs-cdefs.h
guide.o:../x500as/qu-cdefs.h
guide.o:../../h/quipu/syntaxes.h
inherit.o: ../../h/UNIV-types.h
inherit.o: ../../h/UNIV_defs.h
inherit.o: ../../h/UNIV_pre_defs.h
inherit.o: ../../h/config.h
inherit.o: ../../h/general.h
inherit.o: ../../h/isoaddrs.h
inherit.o: ../../h/logger.h
inherit.o: ../../h/manifest.h
inherit.o: ../../h/pepsy.h
inherit.o: ../../h/psap.h
inherit.o: ../../h/quipu/attr.h
inherit.o: ../../h/quipu/attrvalue.h
inherit.o: ../../h/quipu/authen.h
inherit.o: ../../h/quipu/bind.h
inherit.o: ../../h/quipu/config.h
inherit.o: ../../h/quipu/dsp.h
inherit.o: ../../h/quipu/entry.h
inherit.o: ../../h/quipu/name.h
inherit.o: ../../h/quipu/nrs_info.h
inherit.o: ../../h/quipu/oid.h
inherit.o: ../../h/quipu/util.h
inherit.o:../x500as/AF-types.h
inherit.o:../x500as/AF_defs.h
inherit.o:../x500as/AF_pre_defs.h
inherit.o:../x500as/IF-types.h
inherit.o:../x500as/IF_defs.h
inherit.o:../x500as/IF_pre_defs.h
inherit.o:../x500as/NRS-types.h
inherit.o:../x500as/NRS_defs.h
inherit.o:../x500as/NRS_pre_defs.h
inherit.o:../x500as/Quipu-types.h
inherit.o:../x500as/Quipu_defs.h
inherit.o:../x500as/Quipu_pre_defs.h
inherit.o:../x500as/SA-types.h
inherit.o:../x500as/SA_defs.h
inherit.o:../x500as/SA_pre_defs.h
inherit.o:../x500as/Thorn-types.h
inherit.o:../x500as/Thorn_defs.h
inherit.o:../x500as/Thorn_pre_defs.h
inherit.o:../x500as/af-cdefs.h
inherit.o:../x500as/if-cdefs.h
inherit.o:../x500as/nrs-cdefs.h
inherit.o:../x500as/qu-cdefs.h
inherit.o:../../h/quipu/syntaxes.h
integer.o: ../../h/config.h
integer.o: ../../h/general.h
integer.o: ../../h/logger.h
integer.o: ../../h/manifest.h
integer.o: ../../h/psap.h
integer.o: ../../h/quipu/attr.h
integer.o: ../../h/quipu/config.h
integer.o: ../../h/quipu/oid.h
integer.o: ../../h/quipu/util.h
mailbox.o: ../../h/UNIV-types.h
mailbox.o: ../../h/UNIV_defs.h
mailbox.o: ../../h/UNIV_pre_defs.h
mailbox.o: ../../h/config.h
mailbox.o: ../../h/general.h
mailbox.o: ../../h/isoaddrs.h
mailbox.o: ../../h/logger.h
mailbox.o: ../../h/manifest.h
mailbox.o: ../../h/pepsy.h
mailbox.o: ../../h/psap.h
mailbox.o: ../../h/quipu/attr.h
mailbox.o: ../../h/quipu/attrvalue.h
mailbox.o: ../../h/quipu/authen.h
mailbox.o: ../../h/quipu/bind.h
mailbox.o: ../../h/quipu/config.h
mailbox.o: ../../h/quipu/dsp.h
mailbox.o: ../../h/quipu/entry.h
mailbox.o: ../../h/quipu/name.h
mailbox.o: ../../h/quipu/nrs_info.h
mailbox.o: ../../h/quipu/oid.h
mailbox.o: ../../h/quipu/util.h
mailbox.o:../x500as/AF-types.h
mailbox.o:../x500as/AF_defs.h
mailbox.o:../x500as/AF_pre_defs.h
mailbox.o:../x500as/IF-types.h
mailbox.o:../x500as/IF_defs.h
mailbox.o:../x500as/IF_pre_defs.h
mailbox.o:../x500as/NRS-types.h
mailbox.o:../x500as/NRS_defs.h
mailbox.o:../x500as/NRS_pre_defs.h
mailbox.o:../x500as/Quipu-types.h
mailbox.o:../x500as/Quipu_defs.h
mailbox.o:../x500as/Quipu_pre_defs.h
mailbox.o:../x500as/SA-types.h
mailbox.o:../x500as/SA_defs.h
mailbox.o:../x500as/SA_pre_defs.h
mailbox.o:../x500as/Thorn-types.h
mailbox.o:../x500as/Thorn_defs.h
mailbox.o:../x500as/Thorn_pre_defs.h
mailbox.o:../x500as/af-cdefs.h
mailbox.o:../x500as/if-cdefs.h
mailbox.o:../x500as/nrs-cdefs.h
mailbox.o:../x500as/qu-cdefs.h
mailbox.o:../../h/quipu/syntaxes.h
nrs_info.o: ../../h/config.h
nrs_info.o: ../../h/general.h
nrs_info.o: ../../h/logger.h
nrs_info.o: ../../h/manifest.h
nrs_info.o: ../../h/psap.h
nrs_info.o: ../../h/quipu/attr.h
nrs_info.o: ../../h/quipu/config.h
nrs_info.o: ../../h/quipu/nrs_info.h
nrs_info.o: ../../h/quipu/oid.h
nrs_info.o: ../../h/quipu/util.h
oc.o: ../../h/config.h
oc.o: ../../h/general.h
oc.o: ../../h/isoaddrs.h
oc.o: ../../h/logger.h
oc.o: ../../h/manifest.h
oc.o: ../../h/psap.h
oc.o: ../../h/quipu/attr.h
oc.o: ../../h/quipu/attrvalue.h
oc.o: ../../h/quipu/config.h
oc.o: ../../h/quipu/dsp.h
oc.o: ../../h/quipu/entry.h
oc.o: ../../h/quipu/name.h
oc.o: ../../h/quipu/oid.h
oc.o: ../../h/quipu/util.h
oc.o: ../../h/tailor.h
oid.o: ../../h/cmd_srch.h
oid.o: ../../h/config.h
oid.o: ../../h/general.h
oid.o: ../../h/isoaddrs.h
oid.o: ../../h/logger.h
oid.o: ../../h/manifest.h
oid.o: ../../h/psap.h
oid.o: ../../h/quipu/attr.h
oid.o: ../../h/quipu/attrvalue.h
oid.o: ../../h/quipu/config.h
oid.o: ../../h/quipu/dsp.h
oid.o: ../../h/quipu/entry.h
oid.o: ../../h/quipu/name.h
oid.o: ../../h/quipu/oid.h
oid.o: ../../h/quipu/util.h
oid.o: ../../h/tailor.h
oid2.o: ../../h/cmd_srch.h
oid2.o: ../../h/config.h
oid2.o: ../../h/general.h
oid2.o: ../../h/isoaddrs.h
oid2.o: ../../h/logger.h
oid2.o: ../../h/manifest.h
oid2.o: ../../h/psap.h
oid2.o: ../../h/quipu/attr.h
oid2.o: ../../h/quipu/attrvalue.h
oid2.o: ../../h/quipu/config.h
oid2.o: ../../h/quipu/dsp.h
oid2.o: ../../h/quipu/entry.h
oid2.o: ../../h/quipu/name.h
oid2.o: ../../h/quipu/oid.h
oid2.o: ../../h/quipu/util.h
oid2.o: ../../h/tailor.h
oidseq.o: ../../h/config.h
oidseq.o: ../../h/general.h
oidseq.o: ../../h/isoaddrs.h
oidseq.o: ../../h/logger.h
oidseq.o: ../../h/manifest.h
oidseq.o: ../../h/psap.h
oidseq.o: ../../h/quipu/attr.h
oidseq.o: ../../h/quipu/attrvalue.h
oidseq.o: ../../h/quipu/config.h
oidseq.o: ../../h/quipu/dsp.h
oidseq.o: ../../h/quipu/entry.h
oidseq.o: ../../h/quipu/name.h
oidseq.o: ../../h/quipu/oid.h
oidseq.o: ../../h/quipu/util.h
parse.o: ../../h/config.h
parse.o: ../../h/general.h
parse.o: ../../h/isoaddrs.h
parse.o: ../../h/logger.h
parse.o: ../../h/manifest.h
parse.o: ../../h/psap.h
parse.o: ../../h/quipu/attr.h
parse.o: ../../h/quipu/attrvalue.h
parse.o: ../../h/quipu/config.h
parse.o: ../../h/quipu/ds_error.h
parse.o: ../../h/quipu/dsp.h
parse.o: ../../h/quipu/entry.h
parse.o: ../../h/quipu/malloc.h
parse.o: ../../h/quipu/name.h
parse.o: ../../h/quipu/oid.h
parse.o: ../../h/quipu/util.h
parse.o: ../../h/tailor.h
parse_error.o: ../../h/config.h
parse_error.o: ../../h/general.h
parse_error.o: ../../h/logger.h
parse_error.o: ../../h/manifest.h
parse_error.o: ../../h/psap.h
parse_error.o: ../../h/quipu/config.h
parse_error.o: ../../h/quipu/util.h
pdm.o: ../../h/UNIV-types.h
pdm.o: ../../h/UNIV_defs.h
pdm.o: ../../h/UNIV_pre_defs.h
pdm.o: ../../h/cmd_srch.h
pdm.o: ../../h/config.h
pdm.o: ../../h/general.h
pdm.o: ../../h/isoaddrs.h
pdm.o: ../../h/logger.h
pdm.o: ../../h/manifest.h
pdm.o: ../../h/pepsy.h
pdm.o: ../../h/psap.h
pdm.o: ../../h/quipu/attr.h
pdm.o: ../../h/quipu/attrvalue.h
pdm.o: ../../h/quipu/authen.h
pdm.o: ../../h/quipu/bind.h
pdm.o: ../../h/quipu/config.h
pdm.o: ../../h/quipu/dsp.h
pdm.o: ../../h/quipu/entry.h
pdm.o: ../../h/quipu/name.h
pdm.o: ../../h/quipu/nrs_info.h
pdm.o: ../../h/quipu/oid.h
pdm.o: ../../h/quipu/util.h
pdm.o:../x500as/AF-types.h
pdm.o:../x500as/AF_defs.h
pdm.o:../x500as/AF_pre_defs.h
pdm.o:../x500as/IF-types.h
pdm.o:../x500as/IF_defs.h
pdm.o:../x500as/IF_pre_defs.h
pdm.o:../x500as/NRS-types.h
pdm.o:../x500as/NRS_defs.h
pdm.o:../x500as/NRS_pre_defs.h
pdm.o:../x500as/Quipu-types.h
pdm.o:../x500as/Quipu_defs.h
pdm.o:../x500as/Quipu_pre_defs.h
pdm.o:../x500as/SA-types.h
pdm.o:../x500as/SA_defs.h
pdm.o:../x500as/SA_pre_defs.h
pdm.o:../x500as/Thorn-types.h
pdm.o:../x500as/Thorn_defs.h
pdm.o:../x500as/Thorn_pre_defs.h
pdm.o:../x500as/af-cdefs.h
pdm.o:../x500as/if-cdefs.h
pdm.o:../x500as/nrs-cdefs.h
pdm.o:../x500as/qu-cdefs.h
pdm.o:../../h/quipu/syntaxes.h
pe.o: ../../h/config.h
pe.o: ../../h/general.h
pe.o: ../../h/logger.h
pe.o: ../../h/manifest.h
pe.o: ../../h/psap.h
pe.o: ../../h/quipu/attr.h
pe.o: ../../h/quipu/config.h
pe.o: ../../h/quipu/oid.h
pe.o: ../../h/quipu/util.h
picture.o: ../../h/UNIV-types.h
picture.o: ../../h/UNIV_defs.h
picture.o: ../../h/UNIV_pre_defs.h
picture.o: ../../h/config.h
picture.o: ../../h/general.h
picture.o: ../../h/isoaddrs.h
picture.o: ../../h/logger.h
picture.o: ../../h/manifest.h
picture.o: ../../h/pepsy.h
picture.o: ../../h/psap.h
picture.o: ../../h/quipu/attr.h
picture.o: ../../h/quipu/attrvalue.h
picture.o: ../../h/quipu/authen.h
picture.o: ../../h/quipu/bind.h
picture.o: ../../h/quipu/config.h
picture.o: ../../h/quipu/dsp.h
picture.o: ../../h/quipu/entry.h
picture.o: ../../h/quipu/name.h
picture.o: ../../h/quipu/nrs_info.h
picture.o: ../../h/quipu/oid.h
picture.o: ../../h/quipu/photo.h
picture.o: ../../h/quipu/util.h
picture.o:../x500as/AF-types.h
picture.o:../x500as/AF_defs.h
picture.o:../x500as/AF_pre_defs.h
picture.o:../x500as/IF-types.h
picture.o:../x500as/IF_defs.h
picture.o:../x500as/IF_pre_defs.h
picture.o:../x500as/NRS-types.h
picture.o:../x500as/NRS_defs.h
picture.o:../x500as/NRS_pre_defs.h
picture.o:../x500as/Quipu-types.h
picture.o:../x500as/Quipu_defs.h
picture.o:../x500as/Quipu_pre_defs.h
picture.o:../x500as/SA-types.h
picture.o:../x500as/SA_defs.h
picture.o:../x500as/SA_pre_defs.h
picture.o:../x500as/Thorn-types.h
picture.o:../x500as/Thorn_defs.h
picture.o:../x500as/Thorn_pre_defs.h
picture.o:../x500as/af-cdefs.h
picture.o:../x500as/if-cdefs.h
picture.o:../x500as/nrs-cdefs.h
picture.o:../x500as/qu-cdefs.h
picture.o:../../h/quipu/syntaxes.h
policy.o: ../../h/cmd_srch.h
policy.o: ../../h/config.h
policy.o: ../../h/general.h
policy.o: ../../h/manifest.h
policy.o: ../../h/psap.h
policy.o: ../../h/quipu/policy.h
post.o: ../../h/UNIV-types.h
post.o: ../../h/UNIV_defs.h
post.o: ../../h/UNIV_pre_defs.h
post.o: ../../h/config.h
post.o: ../../h/general.h
post.o: ../../h/isoaddrs.h
post.o: ../../h/logger.h
post.o: ../../h/manifest.h
post.o: ../../h/pepsy.h
post.o: ../../h/psap.h
post.o: ../../h/quipu/attr.h
post.o: ../../h/quipu/attrvalue.h
post.o: ../../h/quipu/authen.h
post.o: ../../h/quipu/bind.h
post.o: ../../h/quipu/config.h
post.o: ../../h/quipu/dsp.h
post.o: ../../h/quipu/entry.h
post.o: ../../h/quipu/name.h
post.o: ../../h/quipu/nrs_info.h
post.o: ../../h/quipu/oid.h
post.o: ../../h/quipu/util.h
post.o:../x500as/AF-types.h
post.o:../x500as/AF_defs.h
post.o:../x500as/AF_pre_defs.h
post.o:../x500as/IF-types.h
post.o:../x500as/IF_defs.h
post.o:../x500as/IF_pre_defs.h
post.o:../x500as/NRS-types.h
post.o:../x500as/NRS_defs.h
post.o:../x500as/NRS_pre_defs.h
post.o:../x500as/Quipu-types.h
post.o:../x500as/Quipu_defs.h
post.o:../x500as/Quipu_pre_defs.h
post.o:../x500as/SA-types.h
post.o:../x500as/SA_defs.h
post.o:../x500as/SA_pre_defs.h
post.o:../x500as/Thorn-types.h
post.o:../x500as/Thorn_defs.h
post.o:../x500as/Thorn_pre_defs.h
post.o:../x500as/af-cdefs.h
post.o:../x500as/if-cdefs.h
post.o:../x500as/nrs-cdefs.h
post.o:../x500as/qu-cdefs.h
post.o:../../h/quipu/syntaxes.h
protected.o: ../../h/UNIV-types.h
protected.o: ../../h/UNIV_defs.h
protected.o: ../../h/UNIV_pre_defs.h
protected.o: ../../h/config.h
protected.o: ../../h/general.h
protected.o: ../../h/isoaddrs.h
protected.o: ../../h/logger.h
protected.o: ../../h/manifest.h
protected.o: ../../h/pepsy.h
protected.o: ../../h/psap.h
protected.o: ../../h/quipu/attr.h
protected.o: ../../h/quipu/attrvalue.h
protected.o: ../../h/quipu/authen.h
protected.o: ../../h/quipu/bind.h
protected.o: ../../h/quipu/config.h
protected.o: ../../h/quipu/dsp.h
protected.o: ../../h/quipu/entry.h
protected.o: ../../h/quipu/name.h
protected.o: ../../h/quipu/nrs_info.h
protected.o: ../../h/quipu/oid.h
protected.o: ../../h/quipu/util.h
protected.o:../x500as/AF-types.h
protected.o:../x500as/AF_defs.h
protected.o:../x500as/AF_pre_defs.h
protected.o:../x500as/IF-types.h
protected.o:../x500as/IF_defs.h
protected.o:../x500as/IF_pre_defs.h
protected.o:../x500as/NRS-types.h
protected.o:../x500as/NRS_defs.h
protected.o:../x500as/NRS_pre_defs.h
protected.o:../x500as/Quipu-types.h
protected.o:../x500as/Quipu_defs.h
protected.o:../x500as/Quipu_pre_defs.h
protected.o:../x500as/SA-types.h
protected.o:../x500as/SA_defs.h
protected.o:../x500as/SA_pre_defs.h
protected.o:../x500as/Thorn-types.h
protected.o:../x500as/Thorn_defs.h
protected.o:../x500as/Thorn_pre_defs.h
protected.o:../x500as/af-cdefs.h
protected.o:../x500as/if-cdefs.h
protected.o:../x500as/nrs-cdefs.h
protected.o:../x500as/qu-cdefs.h
protected.o:../../h/quipu/syntaxes.h
psap.o: ../../h/UNIV-types.h
psap.o: ../../h/UNIV_defs.h
psap.o: ../../h/UNIV_pre_defs.h
psap.o: ../../h/config.h
psap.o: ../../h/general.h
psap.o: ../../h/isoaddrs.h
psap.o: ../../h/logger.h
psap.o: ../../h/manifest.h
psap.o: ../../h/pepsy.h
psap.o: ../../h/psap.h
psap.o: ../../h/quipu/attr.h
psap.o: ../../h/quipu/attrvalue.h
psap.o: ../../h/quipu/authen.h
psap.o: ../../h/quipu/bind.h
psap.o: ../../h/quipu/config.h
psap.o: ../../h/quipu/dsp.h
psap.o: ../../h/quipu/entry.h
psap.o: ../../h/quipu/name.h
psap.o: ../../h/quipu/nrs_info.h
psap.o: ../../h/quipu/oid.h
psap.o: ../../h/quipu/util.h
psap.o:../x500as/DO-types.h
psap.o:../x500as/DO_defs.h
psap.o:../x500as/DO_pre_defs.h
rdn_cmp.o: ../../h/config.h
rdn_cmp.o: ../../h/general.h
rdn_cmp.o: ../../h/isoaddrs.h
rdn_cmp.o: ../../h/logger.h
rdn_cmp.o: ../../h/manifest.h
rdn_cmp.o: ../../h/psap.h
rdn_cmp.o: ../../h/quipu/attr.h
rdn_cmp.o: ../../h/quipu/config.h
rdn_cmp.o: ../../h/quipu/ds_error.h
rdn_cmp.o: ../../h/quipu/dsp.h
rdn_cmp.o: ../../h/quipu/name.h
rdn_cmp.o: ../../h/quipu/oid.h
rdn_cmp.o: ../../h/quipu/util.h
rdn_cpy.o: ../../h/config.h
rdn_cpy.o: ../../h/general.h
rdn_cpy.o: ../../h/logger.h
rdn_cpy.o: ../../h/manifest.h
rdn_cpy.o: ../../h/psap.h
rdn_cpy.o: ../../h/quipu/attr.h
rdn_cpy.o: ../../h/quipu/config.h
rdn_cpy.o: ../../h/quipu/malloc.h
rdn_cpy.o: ../../h/quipu/name.h
rdn_cpy.o: ../../h/quipu/oid.h
rdn_cpy.o: ../../h/quipu/util.h
rdn_free.o: ../../h/config.h
rdn_free.o: ../../h/general.h
rdn_free.o: ../../h/logger.h
rdn_free.o: ../../h/manifest.h
rdn_free.o: ../../h/psap.h
rdn_free.o: ../../h/quipu/attr.h
rdn_free.o: ../../h/quipu/config.h
rdn_free.o: ../../h/quipu/name.h
rdn_free.o: ../../h/quipu/oid.h
rdn_free.o: ../../h/quipu/util.h
rdn_merge.o: ../../h/config.h
rdn_merge.o: ../../h/general.h
rdn_merge.o: ../../h/logger.h
rdn_merge.o: ../../h/manifest.h
rdn_merge.o: ../../h/psap.h
rdn_merge.o: ../../h/quipu/attr.h
rdn_merge.o: ../../h/quipu/config.h
rdn_merge.o: ../../h/quipu/name.h
rdn_merge.o: ../../h/quipu/oid.h
rdn_merge.o: ../../h/quipu/util.h
rdn_new.o: ../../h/config.h
rdn_new.o: ../../h/general.h
rdn_new.o: ../../h/logger.h
rdn_new.o: ../../h/manifest.h
rdn_new.o: ../../h/psap.h
rdn_new.o: ../../h/quipu/attr.h
rdn_new.o: ../../h/quipu/config.h
rdn_new.o: ../../h/quipu/name.h
rdn_new.o: ../../h/quipu/oid.h
rdn_new.o: ../../h/quipu/util.h
rdn_print.o: ../../h/config.h
rdn_print.o: ../../h/general.h
rdn_print.o: ../../h/logger.h
rdn_print.o: ../../h/manifest.h
rdn_print.o: ../../h/psap.h
rdn_print.o: ../../h/quipu/attr.h
rdn_print.o: ../../h/quipu/config.h
rdn_print.o: ../../h/quipu/name.h
rdn_print.o: ../../h/quipu/oid.h
rdn_print.o: ../../h/quipu/util.h
rdn_str.o: ../../h/config.h
rdn_str.o: ../../h/general.h
rdn_str.o: ../../h/logger.h
rdn_str.o: ../../h/manifest.h
rdn_str.o: ../../h/psap.h
rdn_str.o: ../../h/quipu/attr.h
rdn_str.o: ../../h/quipu/config.h
rdn_str.o: ../../h/quipu/malloc.h
rdn_str.o: ../../h/quipu/name.h
rdn_str.o: ../../h/quipu/oid.h
rdn_str.o: ../../h/quipu/util.h
revoke.o: ../../h/UNIV-types.h
revoke.o: ../../h/UNIV_defs.h
revoke.o: ../../h/UNIV_pre_defs.h
revoke.o: ../../h/config.h
revoke.o: ../../h/general.h
revoke.o: ../../h/isoaddrs.h
revoke.o: ../../h/logger.h
revoke.o: ../../h/manifest.h
revoke.o: ../../h/pepsy.h
revoke.o: ../../h/psap.h
revoke.o: ../../h/quipu/attr.h
revoke.o: ../../h/quipu/attrvalue.h
revoke.o: ../../h/quipu/authen.h
revoke.o: ../../h/quipu/bind.h
revoke.o: ../../h/quipu/config.h
revoke.o: ../../h/quipu/dsp.h
revoke.o: ../../h/quipu/entry.h
revoke.o: ../../h/quipu/name.h
revoke.o: ../../h/quipu/nrs_info.h
revoke.o: ../../h/quipu/oid.h
revoke.o: ../../h/quipu/util.h
revoke.o:../x500as/AF-types.h
revoke.o:../x500as/AF_defs.h
revoke.o:../x500as/AF_pre_defs.h
revoke.o:../x500as/IF-types.h
revoke.o:../x500as/IF_defs.h
revoke.o:../x500as/IF_pre_defs.h
revoke.o:../x500as/NRS-types.h
revoke.o:../x500as/NRS_defs.h
revoke.o:../x500as/NRS_pre_defs.h
revoke.o:../x500as/Quipu-types.h
revoke.o:../x500as/Quipu_defs.h
revoke.o:../x500as/Quipu_pre_defs.h
revoke.o:../x500as/SA-types.h
revoke.o:../x500as/SA_defs.h
revoke.o:../x500as/SA_pre_defs.h
revoke.o:../x500as/Thorn-types.h
revoke.o:../x500as/Thorn_defs.h
revoke.o:../x500as/Thorn_pre_defs.h
revoke.o:../x500as/af-cdefs.h
revoke.o:../x500as/if-cdefs.h
revoke.o:../x500as/nrs-cdefs.h
revoke.o:../x500as/qu-cdefs.h
revoke.o:../../h/quipu/syntaxes.h
security.o: ../../h/config.h
security.o: ../../h/general.h
security.o: ../../h/isoaddrs.h
security.o: ../../h/logger.h
security.o: ../../h/manifest.h
security.o: ../../h/psap.h
security.o: ../../h/quipu/attr.h
security.o: ../../h/quipu/attrvalue.h
security.o: ../../h/quipu/authen.h
security.o: ../../h/quipu/commonarg.h
security.o: ../../h/quipu/ds_error.h
security.o: ../../h/quipu/dsp.h
security.o: ../../h/quipu/name.h
security.o: ../../h/quipu/oid.h
service.o: ../../h/config.h
service.o: ../../h/general.h
service.o: ../../h/isoaddrs.h
service.o: ../../h/logger.h
service.o: ../../h/manifest.h
service.o: ../../h/psap.h
service.o: ../../h/quipu/attr.h
service.o: ../../h/quipu/attrvalue.h
service.o: ../../h/quipu/authen.h
service.o: ../../h/quipu/commonarg.h
service.o: ../../h/quipu/config.h
service.o: ../../h/quipu/dsp.h
service.o: ../../h/quipu/dua.h
service.o: ../../h/quipu/name.h
service.o: ../../h/quipu/oid.h
service.o: ../../h/quipu/sequence.h
service.o: ../../h/quipu/util.h
soundex.o: ../../h/config.h
soundex.o: ../../h/general.h
soundex.o: ../../h/isoaddrs.h
soundex.o: ../../h/logger.h
soundex.o: ../../h/manifest.h
soundex.o: ../../h/psap.h
soundex.o: ../../h/quipu/attr.h
soundex.o: ../../h/quipu/attrvalue.h
soundex.o: ../../h/quipu/authen.h
soundex.o: ../../h/quipu/commonarg.h
soundex.o: ../../h/quipu/config.h
soundex.o: ../../h/quipu/dap.h
soundex.o: ../../h/quipu/ds_error.h
soundex.o: ../../h/quipu/ds_search.h
soundex.o: ../../h/quipu/dsp.h
soundex.o: ../../h/quipu/name.h
soundex.o: ../../h/quipu/oid.h
soundex.o: ../../h/quipu/util.h
string.o: ../../h/config.h
string.o: ../../h/general.h
string.o: ../../h/isoaddrs.h
string.o: ../../h/logger.h
string.o: ../../h/manifest.h
string.o: ../../h/psap.h
string.o: ../../h/quipu/attr.h
string.o: ../../h/quipu/attrvalue.h
string.o: ../../h/quipu/authen.h
string.o: ../../h/quipu/commonarg.h
string.o: ../../h/quipu/config.h
string.o: ../../h/quipu/dap.h
string.o: ../../h/quipu/ds_error.h
string.o: ../../h/quipu/ds_search.h
string.o: ../../h/quipu/dsp.h
string.o: ../../h/quipu/name.h
string.o: ../../h/quipu/oid.h
string.o: ../../h/quipu/util.h
string.o: ./iso3166.h
sys_init.o: ../../h/config.h
sys_init.o: ../../h/general.h
sys_init.o: ../../h/logger.h
sys_init.o: ../../h/manifest.h
sys_init.o: ../../h/quipu/config.h
sys_init.o: ../../h/quipu/util.h
sys_init.o: ../../h/tailor.h
sys_tai.o: ../../h/cmd_srch.h
sys_tai.o: ../../h/config.h
sys_tai.o: ../../h/general.h
sys_tai.o: ../../h/logger.h
sys_tai.o: ../../h/manifest.h
sys_tai.o: ../../h/quipu/config.h
sys_tai.o: ../../h/quipu/util.h
sys_tai.o: ../../h/tailor.h
tai_args.o: ../../h/config.h
tai_args.o: ../../h/general.h
tai_args.o: ../../h/logger.h
tai_args.o: ../../h/manifest.h
tai_args.o: ../../h/psap.h
tai_args.o: ../../h/quipu/attr.h
tai_args.o: ../../h/quipu/config.h
tai_args.o: ../../h/quipu/oid.h
tai_args.o: ../../h/quipu/util.h
tai_init.o: ../../h/config.h
tai_init.o: ../../h/general.h
tai_init.o: ../../h/logger.h
tai_init.o: ../../h/manifest.h
tai_init.o: ../../h/quipu/config.h
tai_init.o: ../../h/quipu/util.h
tai_init.o: ../../h/tailor.h
teletex.o: ../../h/UNIV-types.h
teletex.o: ../../h/UNIV_defs.h
teletex.o: ../../h/UNIV_pre_defs.h
teletex.o: ../../h/config.h
teletex.o: ../../h/general.h
teletex.o: ../../h/isoaddrs.h
teletex.o: ../../h/logger.h
teletex.o: ../../h/manifest.h
teletex.o: ../../h/pepsy.h
teletex.o: ../../h/psap.h
teletex.o: ../../h/quipu/attr.h
teletex.o: ../../h/quipu/attrvalue.h
teletex.o: ../../h/quipu/authen.h
teletex.o: ../../h/quipu/bind.h
teletex.o: ../../h/quipu/config.h
teletex.o: ../../h/quipu/dsp.h
teletex.o: ../../h/quipu/entry.h
teletex.o: ../../h/quipu/name.h
teletex.o: ../../h/quipu/nrs_info.h
teletex.o: ../../h/quipu/oid.h
teletex.o: ../../h/quipu/util.h
teletex.o:../x500as/AF-types.h
teletex.o:../x500as/AF_defs.h
teletex.o:../x500as/AF_pre_defs.h
teletex.o:../x500as/IF-types.h
teletex.o:../x500as/IF_defs.h
teletex.o:../x500as/IF_pre_defs.h
teletex.o:../x500as/NRS-types.h
teletex.o:../x500as/NRS_defs.h
teletex.o:../x500as/NRS_pre_defs.h
teletex.o:../x500as/Quipu-types.h
teletex.o:../x500as/Quipu_defs.h
teletex.o:../x500as/Quipu_pre_defs.h
teletex.o:../x500as/SA-types.h
teletex.o:../x500as/SA_defs.h
teletex.o:../x500as/SA_pre_defs.h
teletex.o:../x500as/Thorn-types.h
teletex.o:../x500as/Thorn_defs.h
teletex.o:../x500as/Thorn_pre_defs.h
teletex.o:../x500as/af-cdefs.h
teletex.o:../x500as/if-cdefs.h
teletex.o:../x500as/nrs-cdefs.h
teletex.o:../x500as/qu-cdefs.h
teletex.o:../../h/quipu/syntaxes.h
telex.o: ../../h/UNIV-types.h
telex.o: ../../h/UNIV_defs.h
telex.o: ../../h/UNIV_pre_defs.h
telex.o: ../../h/config.h
telex.o: ../../h/general.h
telex.o: ../../h/isoaddrs.h
telex.o: ../../h/logger.h
telex.o: ../../h/manifest.h
telex.o: ../../h/pepsy.h
telex.o: ../../h/psap.h
telex.o: ../../h/quipu/attr.h
telex.o: ../../h/quipu/attrvalue.h
telex.o: ../../h/quipu/authen.h
telex.o: ../../h/quipu/bind.h
telex.o: ../../h/quipu/config.h
telex.o: ../../h/quipu/dsp.h
telex.o: ../../h/quipu/entry.h
telex.o: ../../h/quipu/name.h
telex.o: ../../h/quipu/nrs_info.h
telex.o: ../../h/quipu/oid.h
telex.o: ../../h/quipu/util.h
telex.o:../x500as/AF-types.h
telex.o:../x500as/AF_defs.h
telex.o:../x500as/AF_pre_defs.h
telex.o:../x500as/IF-types.h
telex.o:../x500as/IF_defs.h
telex.o:../x500as/IF_pre_defs.h
telex.o:../x500as/NRS-types.h
telex.o:../x500as/NRS_defs.h
telex.o:../x500as/NRS_pre_defs.h
telex.o:../x500as/Quipu-types.h
telex.o:../x500as/Quipu_defs.h
telex.o:../x500as/Quipu_pre_defs.h
telex.o:../x500as/SA-types.h
telex.o:../x500as/SA_defs.h
telex.o:../x500as/SA_pre_defs.h
telex.o:../x500as/Thorn-types.h
telex.o:../x500as/Thorn_defs.h
telex.o:../x500as/Thorn_pre_defs.h
telex.o:../x500as/af-cdefs.h
telex.o:../x500as/if-cdefs.h
telex.o:../x500as/nrs-cdefs.h
telex.o:../x500as/qu-cdefs.h
telex.o:../../h/quipu/syntaxes.h
test.o: ../../h/UNIV-types.h
test.o: ../../h/UNIV_defs.h
test.o: ../../h/UNIV_pre_defs.h
test.o: ../../h/config.h
test.o: ../../h/general.h
test.o: ../../h/isoaddrs.h
test.o: ../../h/logger.h
test.o: ../../h/manifest.h
test.o: ../../h/pepsy.h
test.o: ../../h/psap.h
test.o: ../../h/quipu/attr.h
test.o: ../../h/quipu/attrvalue.h
test.o: ../../h/quipu/authen.h
test.o: ../../h/quipu/bind.h
test.o: ../../h/quipu/config.h
test.o: ../../h/quipu/dsp.h
test.o: ../../h/quipu/entry.h
test.o: ../../h/quipu/name.h
test.o: ../../h/quipu/nrs_info.h
test.o: ../../h/quipu/oid.h
test.o: ../../h/quipu/util.h
test.o:../x500as/AF-types.h
test.o:../x500as/AF_defs.h
test.o:../x500as/AF_pre_defs.h
test.o:../x500as/IF-types.h
test.o:../x500as/IF_defs.h
test.o:../x500as/IF_pre_defs.h
test.o:../x500as/NRS-types.h
test.o:../x500as/NRS_defs.h
test.o:../x500as/NRS_pre_defs.h
test.o:../x500as/Quipu-types.h
test.o:../x500as/Quipu_defs.h
test.o:../x500as/Quipu_pre_defs.h
test.o:../x500as/SA-types.h
test.o:../x500as/SA_defs.h
test.o:../x500as/SA_pre_defs.h
test.o:../x500as/Thorn-types.h
test.o:../x500as/Thorn_defs.h
test.o:../x500as/Thorn_pre_defs.h
test.o:../x500as/af-cdefs.h
test.o:../x500as/if-cdefs.h
test.o:../x500as/nrs-cdefs.h
test.o:../x500as/qu-cdefs.h
test.o:../../h/quipu/syntaxes.h
time.o: ../../h/config.h
time.o: ../../h/general.h
time.o: ../../h/logger.h
time.o: ../../h/manifest.h
time.o: ../../h/psap.h
time.o: ../../h/quipu/attr.h
time.o: ../../h/quipu/config.h
time.o: ../../h/quipu/oid.h
time.o: ../../h/quipu/util.h
tree_struct.o: ../../h/UNIV-types.h
tree_struct.o: ../../h/UNIV_defs.h
tree_struct.o: ../../h/UNIV_pre_defs.h
tree_struct.o: ../../h/config.h
tree_struct.o: ../../h/general.h
tree_struct.o: ../../h/isoaddrs.h
tree_struct.o: ../../h/logger.h
tree_struct.o: ../../h/manifest.h
tree_struct.o: ../../h/pepsy.h
tree_struct.o: ../../h/psap.h
tree_struct.o: ../../h/quipu/attr.h
tree_struct.o: ../../h/quipu/attrvalue.h
tree_struct.o: ../../h/quipu/authen.h
tree_struct.o: ../../h/quipu/bind.h
tree_struct.o: ../../h/quipu/config.h
tree_struct.o: ../../h/quipu/dsp.h
tree_struct.o: ../../h/quipu/entry.h
tree_struct.o: ../../h/quipu/name.h
tree_struct.o: ../../h/quipu/nrs_info.h
tree_struct.o: ../../h/quipu/oid.h
tree_struct.o: ../../h/quipu/util.h
tree_struct.o:../x500as/AF-types.h
tree_struct.o:../x500as/AF_defs.h
tree_struct.o:../x500as/AF_pre_defs.h
tree_struct.o:../x500as/IF-types.h
tree_struct.o:../x500as/IF_defs.h
tree_struct.o:../x500as/IF_pre_defs.h
tree_struct.o:../x500as/NRS-types.h
tree_struct.o:../x500as/NRS_defs.h
tree_struct.o:../x500as/NRS_pre_defs.h
tree_struct.o:../x500as/Quipu-types.h
tree_struct.o:../x500as/Quipu_defs.h
tree_struct.o:../x500as/Quipu_pre_defs.h
tree_struct.o:../x500as/SA-types.h
tree_struct.o:../x500as/SA_defs.h
tree_struct.o:../x500as/SA_pre_defs.h
tree_struct.o:../x500as/Thorn-types.h
tree_struct.o:../x500as/Thorn_defs.h
tree_struct.o:../x500as/Thorn_pre_defs.h
tree_struct.o:../x500as/af-cdefs.h
tree_struct.o:../x500as/if-cdefs.h
tree_struct.o:../x500as/nrs-cdefs.h
tree_struct.o:../x500as/qu-cdefs.h
tree_struct.o:../../h/quipu/syntaxes.h
turbo_avl.o: ../../h/config.h
turbo_avl.o: ../../h/general.h
turbo_avl.o: ../../h/isoaddrs.h
turbo_avl.o: ../../h/logger.h
turbo_avl.o: ../../h/manifest.h
turbo_avl.o: ../../h/psap.h
turbo_avl.o: ../../h/quipu/attr.h
turbo_avl.o: ../../h/quipu/attrvalue.h
turbo_avl.o: ../../h/quipu/config.h
turbo_avl.o: ../../h/quipu/dsp.h
turbo_avl.o: ../../h/quipu/entry.h
turbo_avl.o: ../../h/quipu/name.h
turbo_avl.o: ../../h/quipu/oid.h
turbo_avl.o: ../../h/quipu/turbo.h
turbo_avl.o: ../../h/quipu/util.h
ufn_aet.o: ../../h/acsap.h
ufn_aet.o: ../../h/config.h
ufn_aet.o: ../../h/general.h
ufn_aet.o: ../../h/isoaddrs.h
ufn_aet.o: ../../h/logger.h
ufn_aet.o: ../../h/manifest.h
ufn_aet.o: ../../h/psap.h
ufn_aet.o: ../../h/psap2.h
ufn_aet.o: ../../h/quipu/abandon.h
ufn_aet.o: ../../h/quipu/add.h
ufn_aet.o: ../../h/quipu/attr.h
ufn_aet.o: ../../h/quipu/attrvalue.h
ufn_aet.o: ../../h/quipu/authen.h
ufn_aet.o: ../../h/quipu/bind.h
ufn_aet.o: ../../h/quipu/common.h
ufn_aet.o: ../../h/quipu/commonarg.h
ufn_aet.o: ../../h/quipu/compare.h
ufn_aet.o: ../../h/quipu/config.h
ufn_aet.o: ../../h/quipu/connection.h
ufn_aet.o: ../../h/quipu/dap.h
ufn_aet.o: ../../h/quipu/ds_error.h
ufn_aet.o: ../../h/quipu/ds_search.h
ufn_aet.o: ../../h/quipu/dsap.h
ufn_aet.o: ../../h/quipu/dsargument.h
ufn_aet.o: ../../h/quipu/dsp.h
ufn_aet.o: ../../h/quipu/dua.h
ufn_aet.o: ../../h/quipu/entry.h
ufn_aet.o: ../../h/quipu/list.h
ufn_aet.o: ../../h/quipu/modify.h
ufn_aet.o: ../../h/quipu/modifyrdn.h
ufn_aet.o: ../../h/quipu/name.h
ufn_aet.o: ../../h/quipu/oid.h
ufn_aet.o: ../../h/quipu/read.h
ufn_aet.o: ../../h/quipu/remove.h
ufn_aet.o: ../../h/quipu/ufn.h
ufn_aet.o: ../../h/quipu/util.h
ufn_aet.o: ../../h/ronot.h
ufn_aet.o: ../../h/rosap.h
ufn_aet.o: ../../h/ssap.h
ufn_parse.o: ../../h/acsap.h
ufn_parse.o: ../../h/config.h
ufn_parse.o: ../../h/general.h
ufn_parse.o: ../../h/isoaddrs.h
ufn_parse.o: ../../h/logger.h
ufn_parse.o: ../../h/manifest.h
ufn_parse.o: ../../h/psap.h
ufn_parse.o: ../../h/psap2.h
ufn_parse.o: ../../h/quipu/abandon.h
ufn_parse.o: ../../h/quipu/add.h
ufn_parse.o: ../../h/quipu/attr.h
ufn_parse.o: ../../h/quipu/attrvalue.h
ufn_parse.o: ../../h/quipu/authen.h
ufn_parse.o: ../../h/quipu/bind.h
ufn_parse.o: ../../h/quipu/common.h
ufn_parse.o: ../../h/quipu/commonarg.h
ufn_parse.o: ../../h/quipu/compare.h
ufn_parse.o: ../../h/quipu/config.h
ufn_parse.o: ../../h/quipu/connection.h
ufn_parse.o: ../../h/quipu/dap.h
ufn_parse.o: ../../h/quipu/ds_error.h
ufn_parse.o: ../../h/quipu/ds_search.h
ufn_parse.o: ../../h/quipu/dsap.h
ufn_parse.o: ../../h/quipu/dsargument.h
ufn_parse.o: ../../h/quipu/dsp.h
ufn_parse.o: ../../h/quipu/dua.h
ufn_parse.o: ../../h/quipu/entry.h
ufn_parse.o: ../../h/quipu/list.h
ufn_parse.o: ../../h/quipu/modify.h
ufn_parse.o: ../../h/quipu/modifyrdn.h
ufn_parse.o: ../../h/quipu/name.h
ufn_parse.o: ../../h/quipu/oid.h
ufn_parse.o: ../../h/quipu/read.h
ufn_parse.o: ../../h/quipu/remove.h
ufn_parse.o: ../../h/quipu/ufn.h
ufn_parse.o: ../../h/quipu/util.h
ufn_parse.o: ../../h/ronot.h
ufn_parse.o: ../../h/rosap.h
ufn_parse.o: ../../h/ssap.h
ufn_parse.o: ../../h/tailor.h
util.o: ../../h/config.h
util.o: ../../h/general.h
util.o: ../../h/isoaddrs.h
util.o: ../../h/logger.h
util.o: ../../h/manifest.h
util.o: ../../h/psap.h
util.o: ../../h/quipu/attr.h
util.o: ../../h/quipu/attrvalue.h
util.o: ../../h/quipu/authen.h
util.o: ../../h/quipu/commonarg.h
util.o: ../../h/quipu/config.h
util.o: ../../h/quipu/dsp.h
util.o: ../../h/quipu/malloc.h
util.o: ../../h/quipu/name.h
util.o: ../../h/quipu/oid.h
util.o: ../../h/quipu/util.h
util.o: ../../h/tsap.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# see make depend above
