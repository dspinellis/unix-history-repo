$ ! @(#)euninstal.com	1.2	10/28/86
$ ! This DCL script installs inews and rnews with the necessary
$ ! privileges in a Eunice system.
$ SET PROCESS/PRIV=CMKRNL
$ RUN SYS$SYSTEM:INSTALL
EUN_USR:[USR.BIN]RNEWS. /OPEN/SHARED/PRIV=SYSPRV
EUN_USR:[USR.LIB.NEWS]INEWS. /OPEN/SHARED/PRIV=SYSPRV
$ EXIT
