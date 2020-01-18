SER_BUFSIZE     .EQU     3FH
SER_FULLSIZE    .EQU     30H
SER_EMPTYSIZE   .EQU     5

RTS_HIGH        .EQU     0D6H
RTS_LOW         .EQU     096H

serBuf          .EQU     $8000
serInPtr        .EQU     serBuf+SER_BUFSIZE
serRdPtr        .EQU     serInPtr+2
serBufUsed      .EQU     serRdPtr+2
snakeStackX		.EQU     $8100 ; Start of snake X pos stack
snakeStackY		.EQU     $8200 ; Start of snake X pos stack
snakeDir		.EQU     $8300 ; Last Char input
snakeL			.EQU     $8301 ; Len of snake

snakeStackXPOS	.EQU     $8302 ; The adress of the location of the head of the snake
snakeStackYPOS	.EQU     $8304 ; The adress of the location of the head of the snake

snakeYIPos		.EQU     $8306 ; Poss of the y snake iterator
snakeXIPos		.EQU     $8308 ; Poss of the x snake iterator

appleXPos		.EQU     $830A 
appleYPos		.EQU     $830B

score			.EQU	 $830C

seed			.EQU	 $8400

                .ORG $9000
				
Setup:
				LD		A, $0
				LD		(score), 	A
				LD		A, $20
				LD		(snakeStackX), 	A
				LD		(snakeStackY), 	A
				LD		HL,	snakeStackX
				LD		(snakeStackXPOS), HL
				LD		HL,	snakeStackY
				LD		(snakeStackYPOS), HL
				LD		A, $05
				LD		(snakeL), 	A
				LD		A, 'W'
				LD		(snakeDir), 	A
				CALL	placeApple
GameLoop:
				CALL	input
				CALL	moveSnake
				CALL	appleColide
				CALL	snakeColide
				LD		HL, cls		;Clear screen
				CALL	print
				
				CALL	drawSnake
				CALL	drawApple
				CALL	drawScore
				LD 		BC,$7FFF   ;delay
				CALL 	DELAY
				JP		GameLoop
				
;-------------- HUD --------------------------
drawScore:
								
				LD		HL, $00
				PUSH	HL
				LD		HL, $21	
				PUSH	HL
				CALL	moveCursor
				LD		HL, scoreHUD
				CALL 	print
				LD		A, (score)
				LD		L,A
				LD		H,$00
				CALL	HLToDec
				RET

;-------------- Apple --------------------------		
placeApple:
				CALL	randomA
				AND     %00011111       ; TRUNCATE
				LD		(appleXPos), A
				CALL	randomA
				AND     %00011111       ; TRUNCATE
				LD		(appleYPos), A
				RET
drawApple:
				LD		A, (appleXPos)
				LD		L, A						
				LD		H, $00
				PUSH	HL
				LD		A, (appleYPos)
				LD		L, A	
				LD		H, $00
				PUSH	HL
				CALL	moveCursor
				LD		HL, apple
				CALL 	print
				RET
appleColide:
				LD		HL,(snakeStackXPOS)
				LD		B, (HL)
				LD		A, (appleXPos)
				CP		B
				RET		NZ
				LD		HL,(snakeStackYPOS)
				LD		B, (HL)
				LD		A, (appleYPos)
				CP		B
				RET		NZ
;Apple Caught
				CALL	placeApple
				LD		HL,	snakeL
				INC		(HL)
				LD		HL,	score
				INC		(HL)
				RET

;-------------- Input --------------------------					
input:
				CALL 	RXAA
				RET		Z
				AND     %11011111       ; lower to uppercase
				CP		'W'
				JP		Z,validInput
				CP		'A'
				JP		Z,validInput
				CP		'S'
				JP		Z,validInput
				CP		'D'
				JP		Z,validInput
				RET
validInput:		
				LD		(snakeDir), A
				RET

;-------------- Snake --------------------------				
moveSnake:
				LD		A,(snakeDir)
				CP		'W'
				JP		Z,moveU
				CP		'A'
				JP		Z,moveL
				CP		'S'
				JP		Z,moveD
				JP		moveR

moveU:			
				LD		HL, (snakeStackYPOS)
				LD		A,(HL)
				DEC		A
				INC		L
				LD		(HL), A
				LD		(snakeStackYPOS),HL
				CALL	copyX
				RET
moveD:			
				LD		HL, (snakeStackYPOS)
				LD		A,(HL)
				INC		A
				INC		L
				LD		(HL), A
				LD		(snakeStackYPOS),HL
				CALL	copyX
				RET
moveL:			
				LD		HL, (snakeStackXPOS)
				LD		A,(HL)
				DEC		A
				INC		L
				LD		(HL), A
				LD		(snakeStackXPOS),HL
				CALL	copyY
				RET
moveR:			
				LD		HL, (snakeStackXPOS)
				LD		A,(HL)
				INC		A
				INC		L
				LD		(HL), A
				LD		(snakeStackXPOS),HL
				CALL	copyY
				RET
				
copyX:
				LD		HL, (snakeStackXPOS)
				LD		A,(HL)
				INC		L
				LD		(HL), A
				LD		(snakeStackXPOS),HL
				RET

copyY:
				LD		HL, (snakeStackYPOS)
				LD		A,(HL)
				INC		L
				LD		(HL), A
				LD		(snakeStackYPOS),HL
				RET

snakeColide:
				LD		A, 	(snakeL)				;Load up B, used to calulate snake len
				LD		B,	A
				;INC		B							; Compenates for dec loop
				LD		HL, (snakeStackXPOS)		; Setup the x iterators
				LD      (snakeXIPos), HL 
				LD		HL, (snakeStackYPOS)		; Setup the y iterators
				LD      (snakeYIPos), HL
snakeColideLoop:
				LD		HL, snakeXIPos				;Move I pointer back 1
				DEC		(HL)
				LD		HL, snakeYIPos				;Move I pointer back 1
				DEC		(HL)
				DEC		B							;Are we at the end of the sanke
				LD		A, B
				RET 	Z
				;Compare
				LD		HL,(snakeXIPos)				;If the head of the snake in the stack = current itteration position
				LD		A, (HL)
				LD		HL,(snakeStackXPOS)
				LD		C, (HL)
				CP		C
				JP		NZ, snakeColideLoop
				LD		HL,(snakeYIPos)
				LD		A, (HL)
				LD		HL, (snakeStackYPOS)
				LD		C, (HL)
				CP		C
				JP		NZ, snakeColideLoop
;Snake Caught
				LD		HL, gameOver		;Game over
				CALL	print
				POP		DE
				RET
				
drawSnake:
				LD		A, 	(snakeL)				;Load up B, used to calulate snake len
				LD		B,	A
				LD		HL, (snakeStackXPOS)		; Setup the x iterators
				LD      (snakeXIPos), HL 
				LD		HL, (snakeStackYPOS)		; Setup the y iterators
				LD      (snakeYIPos), HL 
drawSnakeLoop:	LD		HL, (snakeXIPos)			;Load iterator
				LD 		L, (HL)
				LD		H, $00
				PUSH	HL
				LD 		HL, (snakeYIPos)
				LD 		L, (HL)
				LD		H, $00
				PUSH	HL
				CALL	moveCursor
				LD		A,'#'
				RST     08H
				LD		HL, snakeXIPos
				DEC		(HL)
				LD		HL, snakeYIPos
				DEC		(HL)
				DEC		B
				LD		A, B
				JP		NZ,drawSnakeLoop
				RET
				
;--------------LIBS ---------------------				
RXAA:
				LD       A,(serBufUsed)
                CP       $00
                JR       Z, rtNoChar
                PUSH     HL
                LD       HL,(serRdPtr)
                INC      HL
                LD       A,L             ; Only need to check low byte becasuse buffer<256 bytes
                CP       (serBuf+SER_BUFSIZE) & $FF
                JR       NZ, notRdWrap
                LD       HL,serBuf
notRdWrap:      DI
                LD       (serRdPtr),HL
                LD       A,(serBufUsed)
                DEC      A
                LD       (serBufUsed),A
                CP       SER_EMPTYSIZE
                JR       NC,rts1
                LD       A,RTS_LOW
                OUT      ($80),A
rts1:
                LD       A,(HL)
                EI
                POP      HL
                RET                      ; Char ready in A
rtNoChar:
				LD       A,$00
                RET                      ; 00H ready in A
				
				
moveCursor:		
				POP		DE
				LD		A,$1B
				RST     08H
				LD		A,'['
				RST     08H
				POP		HL
				CALL	HLToDec
				LD		A,$3B
				RST     08H
				POP		HL
				CALL	HLToDec
				LD		A,'f'
				RST     08H
				PUSH	DE
				RET
				
HLToDec:
				PUSH	AF
				PUSH	BC
				CALL	DispHL
				POP		BC
				POP		AF
				RET
;Number in hl to decimal ASCII
;Thanks to z80 Bits
;inputs:	hl = number to ASCII
;example: hl=300 outputs '00300'
;destroys: af, bc, hl, used
DispHL:
				ld		bc,-10000
				call	Num1
				ld		bc,-1000
				call	Num1
				ld		bc,-100
				call	Num1
				ld		c,-10
				call	Num1
				ld		c,-1
Num1:			ld		a,'0'-1
Num2:			inc		a
				add		hl,bc
				jr		c,Num2
				sbc		hl,bc
				RST     08H
				ret
				
print:
				ld 		a,(hl)
				or 		a
				ret 	Z

				rst 	08H          ; Tx byte
				inc 	hl
				jr 	print
DELAY:
				NOP
				DEC 	BC
				LD 		A,B
				OR 		C
				RET 	Z
				JR 		DELAY
randomA:				
				ld a, 	(seed)
				ld b, 	a 

				rrca ; multiply by 32
				rrca
				rrca
				xor $1f

				add 	a, b
				sbc a, 255 ; carry

				ld (seed), a
				ret
				
cls:      	  .BYTE 1BH,"[2J",0
apple:        .BYTE 1BH,"[31m@",1BH,"[0m",0
gameOver:     .BYTE "Game over!",0
scoreHUD:     .BYTE "Score: ",0
.END
