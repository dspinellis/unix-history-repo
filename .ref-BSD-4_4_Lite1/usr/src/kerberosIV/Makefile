#	@(#)Makefile	8.1 (Berkeley) 6/1/93

# do compile_et and make_* first, used by other directories

SUBDIR=	compile_et make_fp make_ip make_key_perm make_odd make_p make_p_table \
	make_s_table

SUBDIR+=acl des kdb krb ext_srvtab kdb_destroy kdb_edit kdb_init kdb_util \
	kdestroy kerberos kinit klist ksrvtgt kstash register registerd \
	make_keypair man

IFILES=	des.h kadm.h kadm_err.h kparse.h krb.h krb_db.h krb_err.h \
	mit-copyright.h

beforeinstall:
	cd ${.CURDIR}/include; \
	    install -c -o ${BINOWN} -g ${BINGRP} -m 444 ${IFILES} \
	    ${DESTDIR}/usr/include/kerberosIV

.include <bsd.subdir.mk>
