$! UUCP Login File
$ set nocontrol
$ if "''F$Mode()'" .NES. "INTERACTIVE" THEN $ GOTO BATCH
$ nettest:='f$logical("SYS$NET")
$ if nettest .nes. "" then goto batch		! Network task
$ deass sys$input
$ uucico := $e$disk:[eunice.usr.lib.uucp]uucico uucico
$! drain:= $e$disk:[eunice.usr.lib.uucp]drain drain
$ uucico
$! drain
$ assi nl: sys$output
$ assi nl: sys$error
$ logout/brief
$ stop/id=0
$ !
$ BATCH:
$ exit
