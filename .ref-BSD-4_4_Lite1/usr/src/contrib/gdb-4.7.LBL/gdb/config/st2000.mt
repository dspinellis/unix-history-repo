# Target: Tandem ST-2000 phone switch
# These defines should give you a gdb running on anything that will be able to
# communicate with a Tandem ST2000 phone switch debug monitor.  Communications
# is facilitated via either a serial line, or a TCP or TELNET connection to
# a serial line on a terminal multiplexor.
TDEPFILES= exec.o m68k-pinsn.o m68k-tdep.o remote-st2000.o
TM_FILE= tm-st2000.h
