date
set nfutil =	/usr/spool/notes/.utilities
du -s /usr/spool/notes /usr/spool/oldnotes
echo -n "Before shorten: "
ls -l /usr/spool/notes/.utilities/net.log
/usr/spool/notes/.utilities/shorten
echo -n "After shorten: "
ls -l /usr/spool/notes/.utilities/net.log
echo " ###"
echo " ### Archvial of Local Notesfiles"
echo " ###"
/usr/spool/notes/.utilities/nfarchive "*" "\!net.*" "\!mod.*" "\!fa.*"
echo " ###"
echo " ### Archival of networked notesfiles"
echo " ###"
/usr/spool/notes/.utilities/nfarchive -d "net.*" -f ${nfutil}/short.names
/usr/spool/notes/.utilities/nfarchive -d "fa.*" -f ${nfutil}/short.names
du -s /usr/spool/notes /usr/spool/oldnotes
date
