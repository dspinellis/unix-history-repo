*** Makefile-dist	Fri Nov  5 01:31:00 1993
--- Makefile	Fri Nov  5 01:31:07 1993
***************
*** 3,20 ****
  #
  
  
! OBJSORT=objsort
  AWKFILE=awkfile
  
  
  libc.so:
! 	ld -assert pure-text `${OBJSORT} lorder-sparc tmp`
  	/bin/ls /usr/lib/libc.so.* > TMP_FILE
  	mv a.out libc.so.`cat TMP_FILE | awk -f ${AWKFILE}`
  	echo libc.so.`cat TMP_FILE | awk -f ${AWKFILE}` built.
  
  libcs5.so:
! 	ld -assert pure-text `${OBJSORT} lorder-sparc tmp`
  	/bin/ls /usr/5lib/libc.so.* > TMP_FILE
  	mv a.out libc.so.`cat TMP_FILE | awk -f ${AWKFILE}`
  	echo libc.so.`cat TMP_FILE | awk -f ${AWKFILE}` built.
--- 3,20 ----
  #
  
  
! OBJSORT=./objsort
  AWKFILE=awkfile
  
  
  libc.so:
! 	ld -assert pure-text `${OBJSORT} lorder-sparc tmp` -ldl
  	/bin/ls /usr/lib/libc.so.* > TMP_FILE
  	mv a.out libc.so.`cat TMP_FILE | awk -f ${AWKFILE}`
  	echo libc.so.`cat TMP_FILE | awk -f ${AWKFILE}` built.
  
  libcs5.so:
! 	ld -assert pure-text `${OBJSORT} lorder-sparc tmp` -ldl
  	/bin/ls /usr/5lib/libc.so.* > TMP_FILE
  	mv a.out libc.so.`cat TMP_FILE | awk -f ${AWKFILE}`
  	echo libc.so.`cat TMP_FILE | awk -f ${AWKFILE}` built.
