SRC_DIR=src
BUILD_DIR=build
LIB_DIR=lib
BNFC=bnfc
GHC=ghc
LLVM_AS=llvm-as
DEV_NULL=/dev/null

.PHONY: clean

all: grammar runtime llvm

grammar: ${SRC_DIR}/Latte.cf
	@cd ${SRC_DIR} && \
	${BNFC} --haskell --functor -m -d Latte.cf > ${DEV_NULL} && \
	make > ${DEV_NULL}

runtime: ${LIB_DIR}/runtime.ll
	@${LLVM_AS} -o ${LIB_DIR}/runtime.bc ${LIB_DIR}/runtime.ll
	
llvm: ${SRC_DIR}/Compiler/Main.hs
	@${GHC} ${SRC_DIR}/Compiler/Main.hs -package mtl -package dlist \
	-i${SRC_DIR}/Compiler -i${SRC_DIR} -outputdir ${BUILD_DIR} -o latc_llvm \
	> ${DEV_NULL}
	
clean:
	@rm -rf ${SRC_DIR}/Latte ${SRC_DIR}/Makefile ${LIB_DIR}/runtime.bc \
	${BUILD_DIR} latc_llvm
