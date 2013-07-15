if ! [ -e cc-lookup ]; then
    git clone git@github.com:HongxuChen/cc-lookup.git
fi
cd cc-lookup
make LLVM_CONFIG=llvm-config-3.4
