C
C Copyright (c) 1980 The Regents of the University of California.
C All rights reserved.
C
C %sccs.include.proprietary.f%
C
C	@(#)seektst.f	5.2 (Berkeley) %G%
C

	character*20 word1, word2
	integer ftell, fseek
	open(1, file="/usr/dict/words")
	iend = ftell(1)
	iseek1 = fseek(1, iend/2, 0)
	read(1,'(a)') word1
	iword = ftell(1)
	read(1,'(a)') word1
	rewind 1
	ibeg = ftell(1)
	iseek2 = fseek(1, iword, 0)
	read(1,'(a)') word2
	write(*,*) ibeg, iword, ftell(1), iend
	write(*,*) word1(:lnblnk(word1)), word2(:lnblnk(word2))
	write(*,*) "seek status", iseek1, iseek2, fseek(1, -1, 0)
	end
