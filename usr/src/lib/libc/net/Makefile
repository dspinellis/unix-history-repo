# @(#)Makefile	4.4 12/17/82
ALL=	common inet misc

all:
	for i in ${ALL}; do (cd $$i; make ${MFLAGS}); done
	for i in ${ALL}; do (cd $$i; ar cru ../netlib *.o; \
		ar cru ../netlib_p profiled/*.o); done

clean:
	rm -f netlib netlib_p
	for i in ${ALL}; do (cd $$i; make ${MFLAGS} clean); done
