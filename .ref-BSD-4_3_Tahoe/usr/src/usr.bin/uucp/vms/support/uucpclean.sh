SPOOL=/usr/spool/uucp

cd /usr/lib/uucp
uuclean -pLTMP. -n24
uuclean -d/usr/spool/uucp/TM. -pTM.
uuclean -d/usr/spool/uucp/X. -pX.
uuclean -d/usr/spool/uucp/C. -pC.
uuclean -d/usr/spool/uucp/D. -pD.
uuclean -d/usr/spool/uucp/D.`uuname -l` -pD.
uuclean -d/usr/spool/uucp/D.`uuname -l`X -pD.

cd $SPOOL
mv LOGFILE o.LOGFILE
mv SYSLOG o.SYSLOG
cp /dev/null LOGFILE
cp /dev/null SYSLOG
chmod 666 LOGFILE SYSLOG
