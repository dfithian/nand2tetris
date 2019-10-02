// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed.
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

    // black or white bit
    @R0
    M=0

(INPUT)
    // get keyboard input
    @KBD
    D=M
    // if 0, white
    @WHITE
    D;JEQ
    // if greater than 0, black
    @BLACK
    D;JGT

(RESET)
    @SCREEN
    D=A
    @R1
    M=D
    @INPUT
    0;JMP

(COLOR)
    @R0
    D=M
    @R1
    A=M
    M=D
    @R1
    M=M+1
    D=M
    @24576
    D=D-A
    @INPUT
    D;JLT
    @RESET
    0;JMP

(BLACK)
    @R0
    M=-1
    @COLOR
    0;JMP

(WHITE)
    @R0
    M=0
    @COLOR
    0;JMP
