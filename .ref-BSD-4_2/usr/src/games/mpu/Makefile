DESTDIR=

mpu:	constants locales locnames mpu.ddl objects routines transitions verbs
	mv mpu mpu.old
	../ddl/ddlcomp mpu < mpu.ddl > ddlcomp.out

install:
	install mpu ${DESTDIR}/usr/games/lib/mpu
	install frontend ${DESTDIR}/usr/games/mpu

clean:
	rm -f ddlcomp.out
