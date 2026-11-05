# Makefile for LispOS Monolithic Common Lisp Userland

KERNEL ?= $(shell if [ -f /lib/modules/$$(uname -r)/vmlinuz ]; then echo /lib/modules/$$(uname -r)/vmlinuz; elif [ -f /boot/vmlinuz-linux ]; then echo /boot/vmlinuz-linux; else echo vmlinuz; fi)
QEMU ?= qemu-system-x86_64

.PHONY: all clean image initramfs run

all: initramfs

image: build/Init

build/Init: build-image.lisp lispos.asd src/*.lisp
	@mkdir -p build
	@echo "==> Compiling monolithic SBCL image..."
	sbcl --script build-image.lisp

initramfs: build/initramfs.cpio.gz

build/initramfs.cpio.gz: build/Init
	@echo "==> Creating clean non-FHS directory hierarchy..."
	@rm -rf build/initramfs
	@mkdir -p build/initramfs/System/Core \
	         build/initramfs/System/Libraries \
	         build/initramfs/System/Source/LispOS \
	         build/initramfs/System/Configuration \
	         build/initramfs/System/Runtime \
	         build/initramfs/System/Kernel/Processes \
	         build/initramfs/System/Kernel/Hardware \
	         build/initramfs/System/Kernel/Devices \
	         build/initramfs/Applications \
	         build/initramfs/Library/Fonts \
	         build/initramfs/Users/Admin
	@echo "==> Copying monolithic PID 1 executable..."
	@cp build/Init build/initramfs/System/Core/Init
	@chmod 755 build/initramfs/System/Core/Init
	@echo "==> Copying shared libraries into /System/Libraries/..."
	@cp -L /usr/lib64/ld-linux-x86-64.so.2 build/initramfs/System/Libraries/ld-linux-x86-64.so.2
	@cp -L /usr/lib/libc.so.6 build/initramfs/System/Libraries/libc.so.6
	@cp -L /usr/lib/libm.so.6 build/initramfs/System/Libraries/libm.so.6
	@cp -L /usr/lib/libzstd.so.1 build/initramfs/System/Libraries/libzstd.so.1
	@cp -L /usr/lib/libdrm.so.2 build/initramfs/System/Libraries/libdrm.so.2
	@echo "==> Patching ELF dynamic linker and rpath..."
	patchelf --set-interpreter /System/Libraries/ld-linux-x86-64.so.2 \
	         --set-rpath /System/Libraries \
	         build/initramfs/System/Core/Init
	@echo "==> Restoring SBCL core trailer after patchelf..."
	@python3 -c "trailer = open('build/Init', 'rb').read()[-32:]; open('build/initramfs/System/Core/Init', 'ab').write(trailer)"
	@echo "==> Copying OS source tree for live introspection..."
	@cp -r src lispos.asd build/initramfs/System/Source/LispOS/
	@echo "==> Packing build/initramfs.cpio.gz..."
	@cd build/initramfs && find . -mindepth 1 | cpio -o -H newc 2>/dev/null | gzip -9 > ../initramfs.cpio.gz
	@echo "==> Initramfs generated successfully: $$(du -h build/initramfs.cpio.gz | cut -f1)"

run: initramfs
	@echo "==> Booting LispOS in QEMU (windowed) with kernel $(KERNEL)..."
	$(QEMU) \
	  -enable-kvm \
	  -cpu host \
	  -m 1024M \
	  -kernel $(KERNEL) \
	  -initrd build/initramfs.cpio.gz \
	  -append "console=ttyS0 rdinit=/System/Core/Init quiet" \
	  -vga std \
	  -serial mon:stdio \
	  -nic user,hostfwd=tcp::4005-:4005

run-stdio: initramfs
	@echo "==> Booting LispOS in QEMU (terminal stdio) with kernel $(KERNEL)..."
	$(QEMU) \
	  -enable-kvm \
	  -cpu host \
	  -m 1024M \
	  -kernel $(KERNEL) \
	  -initrd build/initramfs.cpio.gz \
	  -append "console=ttyS0 rdinit=/System/Core/Init quiet" \
	  -display none \
	  -serial stdio \
	  -nic user,hostfwd=tcp::4005-:4005

clean:
	rm -rf build
