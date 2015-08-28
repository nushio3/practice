#!/bin/bash
POLLY_DIR=/home/nushio/hub/polly
POLLY_BUILD_DIR=${POLLY_DIR}/llvm_build/tools/polly/Debug+Asserts/
export PATH=$PATH:${POLLY_DIR}/llvm_build/Debug+Asserts/bin/
alias pollycc="clang -Xclang -load -Xclang ${POLLY_BUILD_DIR}/lib/LLVMPolly.so"
