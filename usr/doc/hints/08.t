.sh "TERMINAL INTERFACES"
With a VAX you get 8 lines of DZ-11 that provide some modem control
but are not DMA.  We use the Able DH-11 emulator, the 
SuperMAX DH/DM,
or one of the two Emulex DH-11 emulators\- the CS-11 or CS-21.
We also have tried the Intersil DH-11 emulator and know it to
function satisfactorily.
All of these provide DMA on output and modem control.
The CS-11 is unusual in that it provides expansion of up to four
16 line DHs on a single UNIBUS hex module by placing the RS-232
support and UARTS out on the distribution panels and bussing these
panels to the UNIBUS module with one ribbon cable.
The CS-11 is an attractive solution where a very large number of lines will
be connected to one machine since it reduces the number of cables,
and UNIBUS backplane space and power required.  
.LP
4BSD also provide support for the asynchronous serial portion of
the the DEC DMF-32.
This is the standard communications interface for the VAX 11/730 and
has an additional feature of supporting both DMA and programmed
interrupt operation for both input and output.
The 4BSD driver currently does not use all this flexibility,
treating it pretty much like a DH-11.
The DMF-32 driver also works with the Able DMZ-11, a product which
emulates the asynchronous serial portions of two DMF-32s.
.LP
In the area of non-DMA controllers from DEC,
there are the DZ-11 and DZ-32 (a DZ-11 with full modem control).
.LP
Both the DZ's and the DH's have input silo's that UNIX can use
to reduce interrupt load on input.  The DMA output of the DH emulators
is especially important for graphics applications where high-volume and
continuous output occurs.
