C
C Copyright (c) 1980 The Regents of the University of California.
C All rights reserved.
C
C %sccs.include.proprietary.f%
C
C	@(#)ertst.f	5.2 (Berkeley) %G%
C

	character*39 errno
	character*32 gerror
	external gerror

	do 100 i = 0, 36
	call serrno(i)
	write(errno,'("[",i3,"] ",a,":")') ierrno(), gerror()
	call perror(errno)
  100	continue

	do 200 i = 99, 123
	call serrno(i)
	write(errno,'("[",i3,"] ",a,":")') ierrno(), gerror()
	call perror(errno)
  200	continue

	write(**,*) "-----"
	call serrno(-1)
	call perror("Last line          ")
	end
