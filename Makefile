SRC_DIR=src
BUILD_DIR=build
BNFC=bnfc

.PHONY: clean

all: grammar llvm

grammar: ${SRC_DIR}/Latte.cf
	cd ${SRC_DIR} && ${BNFC} --haskell --functor -m -d Latte.cf && make
	
llvm: ${SRC_DIR}/Compiler/Main.hs
	ghc ${SRC_DIR}/Compiler/Main.hs -package mtl -package dlist \
	-i${SRC_DIR}/Compiler -i${SRC_DIR} -outputdir ${BUILD_DIR} -o latc_llvm
	
clean:
	rm -rf ${SRC_DIR}/Latte ${SRC_DIR}/Makefile ${BUILD_DIR} latc_llvm
