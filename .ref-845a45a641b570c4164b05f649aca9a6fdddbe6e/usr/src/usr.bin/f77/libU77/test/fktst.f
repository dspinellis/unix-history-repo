C
C Copyright (c) 1980 The Regents of the University of California.
C All rights reserved.
C
C %sccs.include.proprietary.f%
C
C	@(#)fktst.f	5.2 (Berkeley) %G%
C

	common/sig/whoami
	character*6 whoami
	integer fork, getpid, wait, tick
	external trap

	call signal(2, trap, -1)
	call signal(3, trap, -1)
	call signal(6, trap, -1)
	call signal(8, trap, -1)
	call signal(15, trap, -1)

	tick = 1
	id = fork()
	if (id .eq. 0) then
	    whoami = "child"
	    write(*,*) "child:", getpid()
	    call flush(6)
   10	    call sleep(1)
	    write(*,'(1x,i2,$)') tick
	    call flush(6)
	    tick = tick + 1
	    goto 10
	else
	    whoami = "parent"
	    call sleep(5)
	    write(*,*) "\nparent:", id
	    call flush(6)
	    istat = kill(id, 15)
	    write(*,*) "\nkill status:", istat
	    call flush(6)
	    iwait = wait(isw)
	    write(*,*) iwait, isw
	endif
	end

	subroutine trap(num)
	common/sig/whoami
	character*6 whoami
	write(*,*) "\ntrap:", whoami, num
	stop
	end
