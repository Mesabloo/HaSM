#!sh
echo -e "> \033[35mEnter some ASM then CTRL-D.\033[0m"
if ! as -o main.bin --64 -O0 ; then
    echo -e "> \033[31mCould not assemble..."
else
    echo -ne "> \033[31mDisassembling...\033[0m"
    objdump -j .text -D main.bin
    rm main.bin
fi