C
C Copyright (c) 1983 The Regents of the University of California.
C All rights reserved.
C
C This module is believed to contain source code proprietary to AT&T.
C Use and redistribution is subject to the Berkeley Software License
C Agreement and your Software Agreement with AT&T (Western Electric).
C
C	@(#)f2.f	5.2 (Berkeley) 4/12/91
C

C From aho@ Tue Aug 23 13:07:25 1983
C Date: Tue, 23 Aug 83 13:04:19 PDT
C From: aho@@ (Alan Hopkins)
C Subject: 4.2 rewind problem
C 
C Here is a chunk of code that does not seem to operate properly
C on bach.  The file does not contain any data at completion.

	 program aho
         open ( unit=13, form='unformatted', status='unknown',
     .	 access='sequential',file='UNIX' )
	 do 5 i = 1, 2
	    ieot = -i
	    write(13) ieot
    5    continue
	 endfile(13)
	 call system("od -Iw12 UNIX")
	 write(13) 3
	 write(13) 4
C	 call flush(13)
	 call system("od -Iw12 UNIX")
	 backspace(13)
	 backspace(13)
	 read(13) i
	 write(*,*) "3 ==", i
	 write(13) 5
         rewind(13)
	 call system("od -Iw12 UNIX")
	 close(13)
	 call system("od -Iw12 UNIX")
	 stop
	 end

