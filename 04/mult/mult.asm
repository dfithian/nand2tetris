// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)

    // counter r3 which increments by 1 until it equals r1
    @R3
    M=0
    // product r2
    @R2
    M=0

(LOOP)
    // extract the counter and r1
    @R3
    D=M
    @R1
    A=M
    // end if counter is greater or equal to r1
    D=D-A
    @END
    D;JGE

    // otherwise increment r3 by one, add r0 to itself, and stick it in r2
    @R3
    M=M+1
    @R0
    D=M
    @R2
    M=D+M

    // repeat
    @LOOP
    0;JMP
(END)
    @END
    0;JMP
