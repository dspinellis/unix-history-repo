!
! Load 'boot.' and boot 4.3BSD.
!
! Conversational boot to single user mode
!
!
SET SNAP ON		! Enable ERROR_HALT snapshots
SET FBOX OFF		! System will turn on Fbox
INIT			! SRM processor init
UNJAM 			! UNJAM SBIA's and enable master sbia interrupts
INIT/PAMM		! INIT physical address memory map
DEPOSIT CSWP 8		! Turn off the cache - System will enable cache

DEPOSIT R10 0		! Not used for conversational boot
DEPOSIT R11 3		! Software boot flags (conversational and single)

LOAD/START:0 BOOT.	! Load 'boot.' at memory location 0
START 2			! Start 'boot.' at the address 2
