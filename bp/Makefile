main: main.c
	gcc -o main main.c \
		-Wall -Wextra -Wpedantic -std=c23 \
		-Wno-main \
		-Wno-builtin-declaration-mismatch

main.c: main.bp $(shell find src) Makefile
	cargo run main.bp > main.c

