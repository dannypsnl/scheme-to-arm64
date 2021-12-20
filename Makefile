all:
	@echo "compiling..."
	@clang -dynamiclib -lgc c/runtime.c c/representation.c -o libschemeruntime.dylib
	@echo "installing..."
	@mv libschemeruntime.dylib /usr/local/lib/
	@echo "installed"
