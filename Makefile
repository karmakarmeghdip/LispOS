# Makefile for LispOS Monolithic Common Lisp Userland

KERNEL ?= $(shell if [ -f /lib/modules/$$(uname -r)/vmlinuz ]; then echo /lib/modules/$$(uname -r)/vmlinuz; elif [ -f /boot/vmlinuz-linux ]; then echo /boot/vmlinuz-linux; else echo vmlinuz; fi)
QEMU ?= qemu-system-x86_64

.PHONY: all clean image initramfs run run-stdio start stop status eval screenshot repl log test

all: initramfs

image: build/Init

build/Init: build-image.lisp lispos.asd src/*.lisp src/commands/*.lisp
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
	@ln -sf libdrm.so.2 build/initramfs/System/Libraries/libdrm.so
	@cp -L /usr/lib/libfreetype.so.6 build/initramfs/System/Libraries/libfreetype.so.6
	@ln -sf libfreetype.so.6 build/initramfs/System/Libraries/libfreetype.so
	@cp -L /usr/lib/libpng16.so.16 build/initramfs/System/Libraries/libpng16.so.16
	@cp -L /usr/lib/libz.so.1 build/initramfs/System/Libraries/libz.so.1
	@cp -L /usr/lib/libbz2.so.1.0 build/initramfs/System/Libraries/libbz2.so.1.0
	@cp -L /usr/lib/libbrotlidec.so.1 build/initramfs/System/Libraries/libbrotlidec.so.1
	@cp -L /usr/lib/libbrotlicommon.so.1 build/initramfs/System/Libraries/libbrotlicommon.so.1
	@echo "==> Copying Cascadia Code Nerd Font into /Library/Fonts/..."
	@mkdir -p build/initramfs/Library/Fonts
	@cp -L /usr/share/fonts/TTF/CascadiaCodeNF.ttf build/initramfs/Library/Fonts/CascadiaCodeNF.ttf
	@echo "==> Copying hardware kernel modules into /System/Libraries/Modules/..."
	@mkdir -p build/initramfs/System/Libraries/Modules
	@if [ -f /lib/modules/$$(uname -r)/kernel/drivers/gpu/drm/tiny/bochs.ko.zst ]; then \
	   zstd -d -c /lib/modules/$$(uname -r)/kernel/drivers/gpu/drm/tiny/bochs.ko.zst > build/initramfs/System/Libraries/Modules/bochs.ko; \
	 elif [ -f /lib/modules/$$(uname -r)/kernel/drivers/gpu/drm/tiny/bochs.ko ]; then \
	   cp /lib/modules/$$(uname -r)/kernel/drivers/gpu/drm/tiny/bochs.ko build/initramfs/System/Libraries/Modules/bochs.ko; \
	 fi
	@if [ -f /lib/modules/$$(uname -r)/kernel/drivers/net/ethernet/intel/e1000/e1000.ko.zst ]; then \
	   zstd -d -c /lib/modules/$$(uname -r)/kernel/drivers/net/ethernet/intel/e1000/e1000.ko.zst > build/initramfs/System/Libraries/Modules/e1000.ko; \
	 elif [ -f /lib/modules/$$(uname -r)/kernel/drivers/net/ethernet/intel/e1000/e1000.ko ]; then \
	   cp /lib/modules/$$(uname -r)/kernel/drivers/net/ethernet/intel/e1000/e1000.ko build/initramfs/System/Libraries/Modules/e1000.ko; \
	 fi
	@echo "==> Patching ELF dynamic linker and rpath..."
	@for f in build/initramfs/System/Libraries/libfreetype*.so* \
	          build/initramfs/System/Libraries/libpng*.so* \
	          build/initramfs/System/Libraries/libz*.so* \
	          build/initramfs/System/Libraries/libbz2*.so* \
	          build/initramfs/System/Libraries/libbrotli*.so* \
	          build/initramfs/System/Libraries/libdrm*.so*; do \
	   if [ -f "$$f" ] && [ ! -L "$$f" ]; then \
	     patchelf --set-rpath /System/Libraries "$$f" 2>/dev/null || true; \
	   fi; \
	 done
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

# Harness Automation Targets
start: initramfs
	@./tools/harness.py start --kernel $(KERNEL)

stop:
	@./tools/harness.py stop

status:
	@./tools/harness.py status

eval:
	@./tools/harness.py eval "$$EXPR"

screenshot:
	@./tools/harness.py screenshot $(OUTPUT)

repl:
	@./tools/harness.py repl

log:
	@./tools/harness.py log

test:
	@echo "==> Running LispOS Headless REPL Shell Test Suite..."
	sbcl --non-interactive \
	     --eval '(require :asdf)' \
	     --eval '(let ((quicklisp-setup (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))) (when (probe-file quicklisp-setup) (load quicklisp-setup)))' \
	     --eval '(pushnew (truename ".") asdf:*central-registry* :test #'\''equal)' \
	     --eval '(ql:quickload :lispos)' \
	     --load tests/test-shell.lisp

clean:
	rm -rf build

