mpu:	constants locales locnames mpu.ddl objects routines transitions verbs
	mv mpu mpu.old
	../ddl/ddlcomp mpu < mpu.ddl > ddlcomp.out
install:
	cp mpu /usr/games/lib/mpu
	cp frontend /usr/games/mpu
