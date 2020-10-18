CR              .EQU     0DH
LF              .EQU     0AH

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
speed			.EQU	 $830E

seed			.EQU	 $8400

                .ORG $9000
				
Setup:
				LD		A, $0					;Initial Score
				LD		(score), 	A
				LD		A, $06					;Initial snake pos
				LD		(snakeStackX), 	A
				LD		(snakeStackY), 	A
				LD		HL,	snakeStackX
				LD		(snakeStackXPOS), HL
				LD		HL,	snakeStackY
				LD		(snakeStackYPOS), HL
				LD		A, $05
				LD		(snakeL), 	A			;initial snake lenght
				LD		A, 'D'					;initial direction
				LD		(snakeDir), 	A
				LD		A, $7F
				LD		(speed), 	A		;Initial delay
				CALL	placeApple
				
				LD		HL, cls			;Clear screen
				CALL	print
				LD		HL, hideCursor	;Hide Cursor
				CALL	print
				
				CALL	drawBoard
				
GameLoop:
				CALL	input
				CALL	moveSnake
				CALL	appleColide
				CALL	snakeColide
				
				CALL	drawSnake
				CALL	drawApple
				CALL	drawScore
				LD		A, (speed)
				LD		B ,A
				LD 		C,$FF   ;delay
				CALL 	DELAY
				JR		GameLoop
				
;-------------- HUD --------------------------
drawScore:
								
				LD		HL, $00
				PUSH	HL
				LD		HL, $13
				PUSH	HL
				CALL	moveCursor
				LD		HL, scoreHUD
				CALL 	print
				LD		A, (score)
				LD		L,A
				LD		H,$00
				CALL	HLToDec
				LD		A,LF
				RST     08H
				LD		A,CR
				RST     08H
				RET
				
drawBoard:		LD		HL, gameBoardTop	;Print board top
				CALL	print
				LD		A,	$10
drawBoradMain:	
				LD		HL, gameBoard
				CALL	print
				DEC		A
				JR		NZ,drawBoradMain
				LD		HL, gameBottom		;Print board bottom
				CALL	print
				RET

;-------------- Apple --------------------------		
placeApple:
				CALL	randomA
				AND     00011111b       ; TRUNCATE
				LD		(appleXPos), A
				CALL	randomA
				AND     00001111b       ; TRUNCATE
				LD		(appleYPos), A
				RET
drawApple:
				LD		A, (appleXPos)
				ADD		A,$2						;Make space for the border
				LD		L, A						
				LD		H, $00
				PUSH	HL
				LD		A, (appleYPos)
				ADD		A,$2						;Make space for the border
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
				LD		HL, speed
				DEC		(HL)
				RET

;-------------- Input --------------------------					
input:
				RST		18H
				RET		Z
				RST		10H
				AND     11011111b       ; lower to uppercase
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
				LD		B, A
				AND		11110000b
				JP		NZ,wallHit
				LD		A, B
				INC		L
				LD		(HL), A
				LD		(snakeStackYPOS),HL
				CALL	copyX
				RET
moveD:			
				LD		HL, (snakeStackYPOS)
				LD		A,(HL)
				INC		A
				LD		B, A
				AND		11110000b
				JP		NZ,wallHit
				LD		A, B
				INC		L
				LD		(HL), A
				LD		(snakeStackYPOS),HL
				CALL	copyX
				RET
moveL:			
				LD		HL, (snakeStackXPOS)
				LD		A,(HL)
				DEC		A
				LD		B, A
				AND		11100000b
				JP		NZ,wallHit
				LD		A, B
				INC		L
				LD		(HL), A
				LD		(snakeStackXPOS),HL
				CALL	copyY
				RET
moveR:			
				LD		HL, (snakeStackXPOS)
				LD		A,(HL)
				INC		A
				LD		B, A
				AND		11100000b
				JP		NZ,wallHit
				LD		A, B
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
				
;------- Snake death -------;
wallHit:
				LD		HL, gameOver		;Game over
				CALL	print
				LD		HL, showCursor		;Show cursor
				CALL	print
				POP		DE
				RET

snakeColide:
				LD		A, 	(snakeL)				;Load up B, used to calulate snake len
				LD		B,	A
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
				LD		HL, showCursor		;Show cursor
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
				INC		L							;Make space for the border
				INC		L							;Make space for the border
				LD		H, $00
				PUSH	HL
				LD 		HL, (snakeYIPos)
				LD 		L, (HL)
				INC		L							;Make space for the border
				INC		L							;Make space for the border
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
				JP		NZ,drawSnakeLoop		; Overide last snake pos
				LD		HL, (snakeXIPos)
				LD 		L, (HL)
				INC		L							;Make space for the border
				INC		L							;Make space for the border
				LD		H, $00
				PUSH	HL
				LD 		HL, (snakeYIPos)
				LD 		L, (HL)
				INC		L							;Make space for the border
				INC		L							;Make space for the border
				LD		H, $00
				PUSH	HL
				CALL	moveCursor
				LD		A,' '
				RST     08H
				RET
				
;--------------LIBS ---------------------							
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

print:			PUSH 	AF				; Preserve AF				
printLoop:      LD      A,(HL)          ; Get character
                OR      A               ; Is it $00 ?
                JR     	Z,printRet      ; Then RETurn on terminator
                RST     08H             ; Print it
                INC     HL              ; Next Character
                JR      printLoop       ; Continue until $00
printRet:		
				POP		AF
				RET
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
gameBoardTop: .BYTE 1BH,"[H","#------------*Sna"
			  .BYTE "ke!*------------#",CR,LF,0
gameBoard: 	  .BYTE "|               "
			  .BYTE "                 |",CR,LF,0
gameBottom:   .BYTE "#-----------------"
			  .BYTE "---------------#",0
scoreHUD:     .BYTE "Score: ",0
hideCursor:	  .BYTE	1BH,"[?25l",0
showCursor:	  .BYTE	1BH,"[?25h",0
.END
