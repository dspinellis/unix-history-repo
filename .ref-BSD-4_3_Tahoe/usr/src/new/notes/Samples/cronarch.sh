echo -n "	NOTESFILE ARCHIVAL STARTED AT: " ; date
echo "Notesfile Locks at start:" ; /bin/ls -l /usr/spool/notes/.locks
du -s /usr/spool/notes/. /usr/spool/oldnotes/.
echo -n "Before shorten: "
ls -l /usr/spool/notes/.utilities/net.log
/usr/spool/notes/.utilities/shorten
echo -n "After shorten: "
ls -l /usr/spool/notes/.utilities/net.log
echo " ------"
echo " ------ Mail new stuff to Elsiddig for his analysis"
echo " ------"
/usr/bin/nfxmit -delsiddig general announce sysnews
echo " ------"
echo " ------ Beginning expiration of local notesfiles"
echo " ------"
echo " ---- silly /bin/csh does not behave with exclamation marks"
/usr/spool/notes/.utilities/nfarchive "*" "\!net.*" "\!fa.*" "\!mod.*"
echo " ----"
echo " ----	Archive/Expiration of net.* and fa.*"
echo " ----"
/usr/spool/notes/.utilities/nfarchive -d -10 "net.*"
/usr/spool/notes/.utilities/nfarchive -d -10 "fa.*"
/usr/spool/notes/.utilities/nfarchive -d -10 "mod.*"
echo " ----"
echo " ---- Cleanup and summary"
echo " ----"
du -s /usr/spool/notes/. /usr/spool/oldnotes/.
echo "Notesfile Locks at finish:" ; /bin/ls -l /usr/spool/notes/.locks
echo -n "	NOTESFILE ARCHIVAL COMPLETED AT: " ; date
