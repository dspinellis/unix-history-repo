all: mboot uboot

mboot: mboot.s
	as mboot.s
	strip a.out
	dd if=a.out bs=32 skip=1 | dd of=mboot conv=sync

uboot: uboot.s
	as uboot.s
	strip a.out
	dd if=a.out bs=32 skip=1 | dd of=uboot conv=sync

clean:
	rm a.out
