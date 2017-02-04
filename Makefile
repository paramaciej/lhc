all: lib/runtime.o latc latc_x86_64

latc_x86_64:
	cp latc latc_x86_64

lib/runtime.o:
	cd lib && gcc -c runtime.c

latc: bnfc
	stack install

bnfc:
	stack install BNFC
	cd src && stack exec bnfc latte.cf

clean_bnfc:
	rm -f bnfc src/AbsLatte.* src/DocLatte.* src/ErrM.hs src/LexLatte.* src/ParLatte.* src/PrintLatte.* src/SkelLatte.* src/TestLatte.*

clean: clean_bnfc
	stack clean
	rm -f latc latc_x86_64 lib/runtime.o

distclean: clean
	rm -rf .stack-work