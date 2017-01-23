all: runtime.o latc

runtime.o:
	cd lib && gcc -c runtime.c

latc: bnfc
	stack install

bnfc:
	stack install BNFC
	cd src && stack exec bnfc latte.cf

clean_bnfc:
	rm -f bnfc src/AbsLatte.* src/DocLatte.* src/ErrM.hs src/LexLatte.* src/ParLatte.* src/PrintLatte.* src/SkelLatte.* src/TestLatte.*

clean: clean_bnfc
	rm -f latc latc_x86
