ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
ANTLR_VERSION=9

all : download_antlr_jar antlr

download_antlr_jar:
	(if [ ! -e ${ROOT_DIR}/antlr-4.${ANTLR_VERSION}-complete.jar ]; then \
		wget https://www.antlr.org/download/antlr-4.${ANTLR_VERSION}-complete.jar; \
	fi)

antlr :
	(cd ${ROOT_DIR}/src/main/antlr4;  java -jar ${ROOT_DIR}/antlr-4.${ANTLR_VERSION}-complete.jar -o ${ROOT_DIR}/src/main/java -visitor -package org.commonwl.cwl.ecma.v1_2 CwlEcmaStringLexer.g4 CwlEcmaStringParser.g4)
	(cd ${ROOT_DIR}/src/main/antlr4;  java -jar ${ROOT_DIR}/antlr-4.${ANTLR_VERSION}-complete.jar -o ${ROOT_DIR}/src/main/java -visitor -package org.commonwl.cwl.refparser.v1_2 CwlParameterReferenceLexer.g4 CwlParameterReferenceParser.g4)

clean :
	rm -rf src/main/java
