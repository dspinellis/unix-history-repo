$ !
$ ! Deal with requests for UUCICO
$ !
$ set noon
$ set proc/prio=4
$ set prot=w:re/default
$ assign nla0: sys$print	! Keep those log file coming in	
$ !
$ ! Define Key Parameters & commands
$ !
$ UUCICO_DIR = "e$disk:[eunice.usr.lib.uucp]"
$ UUCICO_RESUBMIT_PERIOD = "+3:00:00"
$ UUCICO_QUEUE = "SYS$SPECIAL"
$ uucico:==$'UUCICO_DIR'uucico uucico
$ status = 1
$ !drain:==$e$disk:[eunice.usr.lib.uucp]drain drain
$ !
$ on error then $ goto error_mail	! If there is a problem go tell the Boss
$ !
$ ! Delete JOBID file (means JOB running)
$ delete 'UUCICO_DIR'uucico.jid;*
$ oncethrough = 0
$ Old_UUCICO_Command := "NEVER LIKELY TO BE IDENTICAL"
$!
$! Main loop.  Get command file arguments and execute UUCICO.
$!
$ANOTHER_CMD_FILE:
$ UUCICO_CMD_FILE = F$SEARCH(UUCICO_DIR + "uucico.dat;*")
$ open/read/error=NO_COMMAND_FILE cmd_file 'UUCICO_DIR'uucico.dat
$ read cmd_file UUCICO_Command /end=EMPTY_COMMAND_FILE
$EMPTY_COMMAND_FILE:
$ close cmd_file
$ delete 'UUCICO_CMD_FILE'	! Delete the UUCICO command file just read
$ goto RUN_CICO
$!
$NO_COMMAND_FILE:
$ if oncethrough .eq. 1 then $ goto RESUBMIT
$ UUCICO_Command:="-r1"		! Default command = "look for work" (Once only)
$!
$RUN_CICO:
$ if Old_UUCICO_Command .eqs. UUCICO_Command then $ goto ANOTHER_CMD_FILE
$ ! allocate dedicated lines
$ !alloc ttd0:
$ uucico 'UUCICO_Command'		!Run UUCICO
$ status = $status
$ ! Clear up terminal lines
$ !drain ttd0:
$ oncethrough = 1
$ Old_UUCICO_Command = UUCICO_Command
$ goto ANOTHER_CMD_FILE
$!
$error_mail:
$ set noon	! Push through regardless...
$ count = 0
$ status = $STATUS
$ subject = ""
$mail_error:
$ subject = subject + F$LOG("SYS$NODE")
$ subject = subject + "UUCICO error: " + F$STRING(STATUS)
$ subject = subject + " = " 
$ subject = subject + F$MESSAGE(STATUS)
$ subject = subject + " " + UUCICO_COMMAND
$ dis_list = UUCICO_DIR + "Disaster.DIS"
$ dis_list = F$PARSE(dis_list)
$ if dis_list .eqs. "" then $ dis_list = "SYSTEM"
$ dis_list = F$SEARCH(dis_list)
$ if dis_list .eqs. "" then $ dis_list = "SYSTEM"
$ if F$LOCATE(".DIS",dis_list) .lt. F$Length(dis_list) then -
	$ dis_list = "@" + dis_list
$ MAIL NL: "''DIS_LIST'" /Subject="''subject'"
$ if .not. $status then $ MAIL nl: SYSTEM -
	/subject="UUCICO error mailing error ''STATUS' / ''$status'"
$ !
$RESUBMIT:
$ if status then $ purge sys$login:uucico.log/keep=2
$ ! Submit the job again later (Delta = defined above)
$ assign 'UUCICO_DIR'uucico.jid sys$output
$ submit 'UUCICO_DIR'uucico -
	/after="''UUCICO_RESUBMIT_PERIOD'" -
	/queue='UUCICO_QUEUE' /keep/noprint
$ status = $status
$ deassign sys$output
$ !
$ if status then $ exit
$ if "''count'" .eqs. "" then $ count = 0
$ if count .gt. 3 then $ exit
$ count = count + 1
$ subject = "Requeue Error:"
$ goto mail_error
$ exit
