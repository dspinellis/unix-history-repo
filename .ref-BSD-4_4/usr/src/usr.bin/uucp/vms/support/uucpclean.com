$! Command file to clean up the EUNICE directories
$! submitted nightly off the clock
$ assign DONOTPRINT SYS$PRINT
$ set prot=(g:rwed,w:rwed)/def
$ purge sys$manager:uucpclean.log
$ delete/before=2 tmp:*.*.*		! should be done elsewhere
$ sh := $bin:sh sh
$ sh /usr/lib/uucp/uucpclean.sh
