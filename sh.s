/ sh -- command interpreter
	mov	sp,r5
	mov	r5,shellarg / save orig sp in shellarg
	cmpb	*2(r5),$'- / was this sh calleZd by init or loginx~
	bne	2f / no
	sys	intr; 0 / yes, turn off interrupts
	sys	quit; 0
2:
	sys	getuid / who is user
	tst	r0 / is it superuser
	bne	2f / no
	movb	$'#,at / yes, set new prompt symbol
2:
	cmp	(r5),$1 / tty input?
	ble	newline / yes, call with '-(or with no command
		        / file name)
	clr	r0 / no, set ttv
	sys	close / close it
	mov	4(r5),0f / get new file name
	sys	open; 0:..; 0 / open it
	bec	1f / branch if no error
	jsr	r5,error / error in file name
		<Input not found\n\0>; .even
	sys	exit
1:
	clr	at / clear prompt character, if reading non-tty
		   / input file
newline:
	tst	at / is there a prompt symbol
	beq	newcom / no
	mov	$1,r0 / yes
	sys	write; at; 2. / print prompt
newcom:
	mov	shellarg,sp /
	mov	$parbuf,r3 / initialize command list area
	mov	$parp,r4 / initialize command list pointers
	clr	infile / initialize alternate input
	clr	outfile / initialize alternate output
	clr	glflag / initialize global flag
newarg:
	jsr	pc,blank / squeeze out leading blanks
	jsr	r5,delim / is new character a ; \n or &
		br 2f / yes
	mov	r3,-(sp) / no, push arg pointer onto stack
	cmp	r0,$'< / new input file?
	bne	1f / no
	mov	(sp),infile / yes, save arg pointer
	clr	(sp) / clear pointer
	br	3f
1:
	cmp	r0,$'> / new output file?
	bne	newchar / no
	mov	(sp),outfile / yes, save arg pointer
	clr	(sp) / clear pointer
	br	3f
newchar:
	cmp	$' ,r0 / is character a blank
	beq	1f / branch if it is (blank as arg separator)
	cmp	$'\n+200,r0 / treat \n preceded by \
	beq	1f / as blank
	jsr	pc,putc / put this character in parbuf list
3:
	jsr	pc,getc / get next character
	jsr	r5,delim / is char a ; \n or &,
		br 1f / yes
	br	newchar / no, start new character tests
1:
	clrb	(r3)+ / end name with \0 when read blank, or
		      / delim
	mov	(sp)+,(r4)+ / move arg ptr to parp location
	bne	1f / if (sp)=0, in file or out file points to arg
	tst	-(r4) / so ignore dummy (0), in pointer list
1:
	jsr	r5,delim / is char a ; \n or &.
		br 2f / yes
	br	newarg / no, start newarg processing
2:
	clr	(r4) / \n, &, or ; takes to here (end of arg list)
		     / after 'delim' call
	mov	r0,-(sp) / save delimter in stack
	jsr	pc,docom / go to exec command in parbuf
	cmpb	(sp),$'& / get a new command without wait?
	beq	newcom / yes
	tst	r1 / was chdir just executed or line ended with
		   / ampersand?
	beq	2f / yes
1:
	sys	wait / no, wait for new process to terminate
		     / command executed)
	bcs	2f / no, children not previously waited for
	cmp	r0,r1 / is this my child
	bne	1b
2:
	cmp	(sp),$'\n / was delimiter a new line
	beq	newline / yes
	br	newcom / no, pick up next command
docom:
	sub	$parp,r4 / out arg count in r4
	bne	1f / any arguments?
	clr	r1 / no, line ended with ampersand
	rts	pc / return from call
1:
	jsr	r5,chcom; qchdir / is command chdir?
		br 2f / command not chdir
	cmp	r4,$4 / prepare to exec chdir, 4=arg count x 2
	beq	3f
	jsr	r5,error / go to print error
		<Arg count\n\0>; .even
	br	4f
3:
	mov	parp+2,0f / more directory name to sys coll
	sys	chdir; 0:0 / exec chdir
	bec	4f / no error exit
	jsr	r5,error / go to print error
		<Bad directory\n\0>; .even / this diagnostic
4:
	clr	r1 / set r1 to zero to dkip wait
	rts	pc / and return
2:
	jsr	r5,chcom; glogin / is command login?
		br 2f / not loqin, go to fork
	sys	exec; parbuf; parp / exec login
	sys	exec; binpb; parp / or /bin/login
2: / no error return??
	sys	fork / generate sh child process for command
		br newproc / exec command with new process
	bec	1f / no error exit, old orocess
	jsr	r5,error / go to print error
		<Try again\n\0>; .even / this diaonostic
	jmp	newline / and return for next try
1:
	mov	r0,r1 / save id of child sh
	rts	pc / return to "jsr pc, docom" call in parent sh

error:
	movb	(r5)+,och / pick up diagnostic character
	beq	1f / 0 is end of line
	mov	$1,r0 / set for tty output
	sys	write; och; 1 / print it
	br	error / continue to get characters
1:
	inc	r5 / inc r5 to point to return
	bic	$1,r5 / make it even
	clr	r0 / set for input
	rts	r5

	sys	seek; 0; 2 / exit from runcom. skip to end of
		           / input file
chcom: / has no effect if tty input
	mov	(r5)+,r1 / glogin gchdir r1, bump r5
	mov	$parbuf,r2 / command address  r2 'login'
1:
	movb	 (r1)+,r0 / is this command 'chdir'
	cmpb	(r2)+,r0 / compare command name byte with 'login'
		         / or 'chdir'
	bne	1f / doesn't compare
	tst	r0 / is this
	bne	1b / end of names
	tst	(r5)+ / yes, bump r5 again to execute login
		      / chdir
1:
	rts	r5 / no, return to exec command

putc:
	cmp	r0,$'' / single quote?
	beq	1f / yes
	cmp	r0,$'" / double quote
	beq	1f / yes
	bic	$!177,r0 / no, remove 200, if present
	movb	r0,(r3)+ / store character in parbuf
	rts	pc
1:
	mov	r0,-(sp) / push quote mark onto stack
1:
	jsr	pc,getc / get a quoted character
	cmp	r0,$'\n / is it end or line
	bne	2f / no
	jsr	r5,error / yes, indicate missing quote mark
		<"' imbalance\n\0>; .even
	jmp	newline / ask for new line
2:
	cmp	r0,(sp) / is this closing quote mark
	beq	1f / yes
	bic	$!177,r0 / no, strip off 200 if present
	movb	r0,(r3)+ / store quoted character in parbuf
	br	1b / continue
1:
	tst	(sp)+ / pop quote mark off stack
	rts	pc / return

/ thp`e new process

newproc:
	mov	infile,0f / move pointer to new file name
	beq	1f / branch if no alternate read file given
	tstb	*0f
	beq	3f / branch if no file name miven
	clr	r0 / set tty input file name
	sys	close / close it
	sys	open; 0:..; 0 / open new input file for reading
	bcc	1f / branch if input file ok
3:
	jsr	r5,error / file not ok, print error
		<Input file\n\0>; .even / this diagnostic
	sys	exit / terminate this process and make parent sh
1:
	mov	outfile,r2 / more pointer to new file name
	beq	1f / branch if no alternate write file
	cmpb	(r2),$'> / is > at beqinninrg of file name?
	bne	4f / branch if it isn't
	inc	r2 / yes, increment pointer
	mov	r2,0f
	sys	open; 0:..; 1 / open file for writing
	bec	3f / if no error
4:
	mov	r2,0f
	sys	creat; 0:..; 17 / create new file with this name
	bec	3f / branch if no error
2:
	jsr	r5,error
		<Output file\n\0>; .even
	sys	exit
3:
	sys	close / close the new write file
	mov	r2,0f / move new name to open
	mov	$1,r0 / set ttv file name
	sys	close / close it
	sys	open; 0:..; 1 / open new output file, it now has
		              / file descriptor 1
	sys	seek; 0; 2 / set pointer to current end of file
1:
	tst	glflag / was *, ? or [ encountered?
	bne	1f / yes
	sys	exec; parbuf; parp / no, execute this commend
	sys	exec; binpb; parp / or /bin/this command
2:
	sys	stat; binpb; inbuf / if can't execute does it
		                   / exist?
	bes	2f / branch if it doesn't
	mov	$shell,parp-2 / does exist, not executable
	mov	$binpb,parp / so it must be
	sys	exec; shell; parp-2 / a command file, get it with
		                    / sh /bin/x (if x name of file)
2:
	jsr	r5,error / a return for exec is the diagnostic
		<No command\n\0>; .even
	sys	exit
1:
	mov	$glob,parp-2 / prepare to process *,?
	sys	exec; glob; parp-2 / execute modified command
	br	2b

delim:
	cmp	r0,$'\n / is character a newline
	beq	1f
	cmp	r0,$'& / is it &
	beq	1f / yes
	cmp	r0,$'; / is it ;
	beq	1f / yes
	cmp	r0,$'? / is it ?
	beq	3f
	cmp	r0,$'* / is it *
	beq	3f
	cmp	r0,$'[ / is it beginning of character string
		       / (for glob)
	bne	2f
3:
	inc	glflag / ? or * or [ set flag
2:
	tst	(r5)+ / bump to process all except \n,;,&
1:
	rts	r5

blank:
	jsr	pc,getc / get next character
	cmp	$' ,r0 / leading blanks
	beq	blank / yes, 'squeeze out'
	cmp	r0,$200+'\n / new-line preceded by \ is translated
	beq	blank / into blank
	rts	pc
getc:
	tst	param / are we substituting for $n
	bne	2f/ yes
	mov	inbufp,r1 / no, move normal input pointer to r1
	cmp	r1,einbuf / end of input line?
	bne	1f / no
	jsr	pc,getbuf / yes, put next console line in buffer
	br	getc
1:
	movb	(r1)+,r0 / move byte from input buffer to r0
	mov	r1,inbufp / increment routine
	bis	escap,r0 / if last character was \ this adds
		         / 200 to current character
	clr	escap / clear, so escap normally zero
	cmp	r0,$'\\ / note that \\ is equal \ in as
	beq	1f
	cmp	r0,$'$ / is it $
	beq	3f / yes
	rts	pc / no
1:
	mov	$200,escap / mark presence of \ in command line
	br	getc / get next character
2:
	movb	*param,r0 / pick up substitution character put in
		          / r0
	beq	1f / if end of substitution arg, branch
	inc	param / if not end, set for next character
	rts	pc / return as though character in ro is normal
		   / input
1:
	clr	param / unset substitution pointer
	br	getc / get next char in normal input
3:
	jsr	pc,getc / get digit after $
	sub	$'0,r0 / strip off zone bits
	cmp	r0,$9. / compare with digit 9 
	blos	1f / less than or equal 9
	mov	$9.,r0 / if larger than 9, force 9
1:
	mov	shellarg,r1 / get pointer to stack for
		            / this call of shell
	inc	r0 / digit +1
	cmp	r0,(r1) / is it less than # of args in this call
	bge	getc / no, ignore it. so this $n is not replaced
	asl	r0 / yes, multiply by 2 (to skip words)
	add	r1,r0 / form pointer to arg pointer (-2)
	mov	2(r0),param / move arg pointer to param
	br	getc / go to get substitution arg for $n
getbuf:
	mov	$inbuf,r0 / move input buffer address
	mov	r0,inbufp / to input buffer pointer
	mov	r0,einbuf / and initialize pointer to end of
		          / character string
	dec	r0 / decrement pointer so can utilize normal
		   / 100p starting at 1f
	mov	r0,0f / initialize address for reading 1st char
1:
	inc	0f / this routine filles inbuf with line from
		   / console - if there is cnc
	clr	r0 / set for tty input
	sys	read; 0:0; 1 / read next char into inbuf
	bcs	xit1 / error exit
	tst	r0 / a zero input is end of file
	beq	xit1 / exit
	inc	einbuf / eventually einbuf points to \n
		       / (+1) of this line
	cmp	0b,$inbuf+256. / have we exceeded input buffer size
	bhis	xit1 / if so, exit assume some sort of binary
	cmpb	*0b,$'\n / end of line?
	bne	1b / no, go to get next char
	rts	pc / yes, return

xit1:
	sys	exit

quest:
	<?\n>

at:
	<@ >

qchdir:
	<chdir\0>
glogin:
	<login\0>
shell:
	</bin/sh\0>
glob:
	</etc/glob\0>
binpb:
	</bin/>
parbuf: .=.+1000.
	.even
param:	.=.+2
glflag:	.=.+2
infile: .=.+2 
outfile:.=.+2
	.=.+2 / room for glob
parp:	.=.+200.
inbuf:	.=.+256.
escap:	.=.+2
inbufp: .=.+2
einbuf:	.=.+2
och:	.=.+2
shellarg:.=.+2

