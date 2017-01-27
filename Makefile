all: lib/runtime.o latc latc_x86_64

stack:
	wget -O stack.tar.gz https://www.stackage.org/stack/linux-x86_64
	tar xf stack.tar.gz
	rm -rf stack-local
	mv stack-1* stack-local
	./stack-local/stack setup

latc_x86_64:
	cp latc latc_x86_64

lib/runtime.o:
	cd lib && gcc -c runtime.c

latc: bnfc
	./stack-local/stack install

bnfc: stack
	./stack-local/stack install BNFC
	cd src && ../stack-local/stack exec bnfc latte.cf

clean_bnfc:
	rm -f bnfc src/AbsLatte.* src/DocLatte.* src/ErrM.hs src/LexLatte.* src/ParLatte.* src/PrintLatte.* src/SkelLatte.* src/TestLatte.*

clean: clean_bnfc
	./stack-local/stack clean
	rm -f latc latc_x86_64 lib/runtime.o

distclean: clean
	rm -rf .stack-work
	rm -rf stack-local