all: mboot uboot

mboot: mboot.s
	as mboot.s
	dd if=a.out of=mboot bs=32 skip=1

uboot: uboot.s
	as uboot.s
	dd if=a.out of=uboot bs=32 skip=1

clean:
	rm a.out
