SRC_DIR=src
BUILD_DIR=build
LIB_DIR=lib
BNFC=bnfc
GHC=ghc
LLVM_AS=llvm-as
DEV_NULL=/dev/null
TARGET=latc_llvm
TARGET_GEN=latc

.PHONY: clean

all: grammar runtime ${TARGET} ${TARGET_GEN}

grammar: ${SRC_DIR}/Latte.cf
	@cd ${SRC_DIR} && \
	${BNFC} --haskell --functor -m -d Latte.cf > ${DEV_NULL} && \
	make > ${DEV_NULL}

runtime: ${LIB_DIR}/runtime.ll
	@${LLVM_AS} -o ${LIB_DIR}/runtime.bc ${LIB_DIR}/runtime.ll
	
${TARGET}: grammar runtime
	@${GHC} ${SRC_DIR}/Compiler/Main.hs -package mtl -package dlist \
	-i${SRC_DIR}/Compiler -i${SRC_DIR} -outputdir ${BUILD_DIR} -o ${TARGET} \
	> ${DEV_NULL}

${TARGET_GEN}: ${TARGET}
	@echo '#!/bin/bash' > $@
	@echo './${TARGET} $$1' >> $@
	@chmod +x $@
	
clean:
	@rm -rf ${SRC_DIR}/Latte ${SRC_DIR}/Makefile ${LIB_DIR}/runtime.bc \
	${BUILD_DIR} ${TARGET} ${TARGET_GEN}
