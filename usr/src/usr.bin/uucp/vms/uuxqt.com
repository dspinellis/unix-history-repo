$ !
$ ! Deal with requests for UUXQT
$ !
$ set noon
$ set proc/prio=4
$ set prot=w:re/default
$ !
$ UUXQT_DIR = "e$disk:[eunice.usr.lib.uucp]"
$ UUXQT_EXECUTE_DIR = "e$disk:[eunice.usr.spool.uucp]"
$ UUXQT_QUEUE = "SYS$SPECIAL"
$ UUXQT_RESUBMIT_PERIOD = "TOMORROW"
$ uuxqt:==$'UUXQT_DIR'uuxqt uuxqt
$ !
$ ! If there is a problem go tell the Boss 
$ on error then $ goto error_mail	
$ !
$ ! Delete JOBID file (means JOB running)
$ delete 'UUXQT_DIR'uuxqt.jid;*
$ UUXQT_Command:=""
$ UUXQT_CMD_FILE = F$SEARCH(UUXQT_DIR + "uuxqt.dat;*")
$ open/read/error=NO_COMMAND_FILE cmd_file 'UUXQT_DIR'uuxqt.dat
$ read cmd_file UUXQT_Command /end=EMPTY_COMMAND_FILE
$EMPTY_COMMAND_FILE:
$ close cmd_file
$ !
$ ! Delete all UUXQT command files 
$ UUXQT_CMD_FILES = UUXQT_CMD_FILE - F$PARSE(UUXQT_CMD_FILE,,,"VERSION") + ";*"
$ delete 'UUXQT_CMD_FILES'
$ !
$ NO_COMMAND_FILE:
$ assign nla0: sys$print	! Keep those log file coming in	!!???!!
$ set default 'UUXQT_EXECUTE_DIR'
$ uuxqt 'UUXQT_Command'		!Run UUXQT
$ status = $status
$ goto resubmit

$error_mail:
$ set noon	! Push through regardless...
$ count = 0
$ subject = ""
$mail_error:
$ on error then $ goto mail_error
$ set noon
$ if count .gt. 10 then $ exit %X114
$ status = $STATUS
$ subject = subject + F$LOG("SYS$NODE")
$ subject = subject + "UUXQT error: " + F$STRING(STATUS)
$ subject = subject + " = " 
$ subject = subject + F$MESSAGE(STATUS)
$ subject = subject + " " + UUXQT_COMMAND
$ dis_list = UUXQT_DIR + "Disaster.DIS"
$ dis_list = F$PARSE(dis_list)
$ if dis_list .eqs. "" then $ dis_list = "SYSTEM"
$ dis_list = F$SEARCH(dis_list)
$ if dis_list .eqs. "" then $ dis_list = "SYSTEM"
$ if F$LOCATE(".DIS",DIS_LIST) .lt. F$LENGTH(DIS_LIST) then -
	$ dis_list = "@" + dis_list
$ MAIL NL: "''DIS_LIST'" /Subject="''subject'"
$ if .not. $status then $ MAIL nl: SYSTEM -
	/subject="UUXQT error mailing error ''STATUS' / ''$status'"
$ !
$resubmit:
$ if status then $ purge sys$login:uuxqt.log/keep=2
$ uuxqt_command = "submit"
$ ! Submit the job again later (Delta = defined above)
$ assign 'UUXQT_DIR'uuxqt.jid sys$output
$ submit 'UUXQT_DIR'uuxqt -
	/after="''UUXQT_RESUBMIT_PERIOD'" -
	/queue='UUXQT_QUEUE' /keep/NOPRINT
$ status = $status
$ deassign sys$output
$ !
$ if status then $ exit
$ if count .gt. 3 then $ exit
$ count = count + 1
$ subject = "Requeue Error:"
$ goto mail_error
$ exit
