StartTextMode MACRO
    mov ah, 00h
	mov al, 03h
	int 10h
ENDM

ClearLine MACRO Y
    GoToXY 0, Y
    DisplayString Empty_Line
	GoToXY 0, Y
ENDM

GoToXY MACRO X, Y
	pusha
	mov ah, 2
	mov bh, 0
	mov dh, Y
	mov dl, X
	int 10h
	popa
ENDM

HideTheCursor MACRO
	GoToXY 80d,25d
ENDM

DisplayChar MACRO char
	pusha
	mov ah, 2
	mov dl, char
	int 21h
	popa
ENDM

DisplayString MACRO string
	pusha
	mov ah, 9
	mov dx, offset string
	int 21h
	popa
ENDM

DisplayCharColored MACRO char, color
	PUSHA
	mov ah,9 		;Display
	mov bh,0 		;Page 0
	mov al,char
	mov cx, 1
	mov bl,color 	;Green (A) on black(0) background
	int 10h 
	popa
ENDM

ReadString MACRO destination
    MOV AH, 0AH
    MOV DX, OFFSET destination
    INT 21H
ENDM

ScrollDown MACRO X1, Y1, X2, Y2
	pusha
	mov ah,6        ; function 6
    mov al,1        ; scroll by 1 line    
    mov bh,7        ; normal video attribute         
    MOV CL, X1		; Upper left row
    MOV CH, Y1		; Upper left column
    MOV DL, X2		; Lower right row
    MOV DH, Y2		; Lower right column
    int 10h           
	popa
ENDM

ScrollUp MACRO X1, Y1, X2, Y2
	pusha
	mov ah,7        ; function 6
    mov al,1        ; scroll by 1 line    
    mov bh,7        ; normal video attribute         
    MOV CL, X1		; Upper left row
    MOV CH, Y1		; Upper left column
    MOV DL, X2		; Lower right row
    MOV DH, Y2		; Lower right column
    int 10h           
	popa
ENDM

InitializeSerialComm MACRO
    ;Set Divisor Latch Access Bit
	mov dx,3fbh 			; Line Control Register
	mov al,10000000b		;Set Divisor Latch Access Bit
	out dx,al			;Out it
	;Set LSB byte of the Baud Rate Divisor Latch register.
	mov dx,3f8h			
	mov al,0ch			
	out dx,al
	;Set MSB byte of the Baud Rate Divisor Latch register.
	mov dx,3f9h
	mov al,00h
	out dx,al
	;Set port configuration
	mov dx,3fbh
	mov al,00011011b
	;0:Access to Receiver buffer, Transmitter buffer
	;0:Set Break disabled
	;011:Even Parity
	;0:One Stop Bit
	;11:8bits
	out dx,al
ENDM

displaybin MACRO
	LOCAL notzero,rep, a
	GoToXY 50, 12
	push cx
	push ax
	mov cx, 16
	rep:
	shl ax, 1
	jc notzero
		DisplayChar '0'
		jmp a
	notzero:
		DisplayChar '1'
	a:
	loop rep
	pop ax
	pop cx
ENDM

inline_Player1 MACRO char
	LOCAL scdwi_player1
	pusha
	mov sendFlag, 1
	mov bx, iterator1_x
	mov di, iterator1_y
	
	GoToXY SX, SY
	
	;Check if enter is pressed
	CMP char, 1CH
	JE enter_inline1
	
	;Check if backspace is pressed
	CMP char, 17H
	JE backspace_inline1
	
	;Check if character not printable so don't transmit
    CMP char, ' '   ;Compare with lowest printable ascii value
    JB not_printable
    CMP char, '~'   ;Compare with highest printable ascii value
    JA not_printable

	DisplayChar char
	mov sender_buffer[bx][di], char

	inc SX
	inc iterator1_x
	;Go the next line if end of line reached or enter pressed
	CMP SX, 80d	
	JNE end_inline1

	enter_inline1:
	MOV SX, 55
	mov iterator1_x, 0
	INC SY
	ADD iterator1_y, 25
	CMP SY, 12
	JNE end_inline1

	;Scroll down here
	;Scrolling down happens by shifting each line in the memory up by one line
	;and then printing a blank line at the end
	mov bx, 0
	mov cx, 9		;size of chatting is 9 lines
	mov sx, 55d		;Positions
	mov sy, 3
	scdwi_player1:			;Scrolling down without interrupts
	push cx
	add bx, 25						;each line is 25 characters
	mov dx, offset sender_buffer	;Putting offset of memory in si
	add dx, bx
	mov si, dx						;Using a temporary string in di
	lea di, smallTemp
	mov cx, 25
	rep MOVSB
	sub bx, 25
	mov dx, offset sender_buffer	;Put string from temp back in memory one line up
	add dx, bx
	mov di, dx
	mov cx, 25
	lea si, smallTemp
	rep movsb
	add bx, 25
	GoToXY sx, sy
	DisplayString smallEmptyLine
	GoToXY sx, sy
	DisplayString smallTemp
	inc sy
	pop cx
	loop scdwi_player1

	mov iterator1_y, 200		;Resetting iterators back
	MOV SY, 11
	JMP end_inline1

	backspace_inline1:
	CMP SX, 55					;If at beginning of line, do nothing
	JE end_inline1
	DEC SX
	DEC iterator1_x
	DEC BX
	GoToXY SX, SY
	DisplayChar ' '
	MOV sender_buffer[BX][DI], '$'
	JMP end_inline1

	not_printable:		;Do not send character if it is not printable
	mov sendFlag, 0
	end_inline1:
	popa
ENDM

inline_Player2 MACRO char
	LOCAL scdwi_player2
	pusha
	mov bx, iterator2_x
	mov di, iterator2_y
	
	GoToXY RX, RY
	
	;Check if enter is pressed
	CMP char, 1CH
	JE enter_inline2
	
	;Check if backspace is pressed
	CMP char, 17H
	JE backspace_inline2

	DisplayChar char
	mov receiver_buffer[bx][di], char

	inc RX
	inc iterator2_x
	;Go the next line if end of line reached or enter pressed
	CMP RX, 80d	
	JNE end_inline2

	enter_inline2:
	MOV RX, 55
	mov iterator2_x, 0
	INC RY
	ADD iterator2_y, 25
	CMP RY, 23
	JNE end_inline2
	
	;Scroll down here
	mov bx, 0
	mov cx, 9
	mov rx, 55d
	mov ry, 14
	scdwi_player2:
	push cx
	add bx, 25
	mov dx, offset receiver_buffer
	add dx, bx
	mov si, dx
	lea di, smallTemp
	mov cx, 25
	rep MOVSB
	sub bx, 25
	mov dx, offset receiver_buffer
	add dx, bx
	mov di, dx
	mov cx, 25
	lea si, smallTemp
	rep movsb
	add bx, 25
	GoToXY rx, ry
	DisplayString smallEmptyLine
	GoToXY rx, ry
	DisplayString smallTemp
	inc ry
	pop cx
	loop scdwi_player2

	mov iterator2_y, 200
	MOV RY, 22
	JMP end_inline2

	backspace_inline2:
	CMP RX, 55		;If at beginning of line, do nothing
	JE end_inline2
	DEC RX
	DEC iterator2_x
	DEC BX
	GoToXY RX, RY
	DisplayChar ' '
	MOV receiver_buffer[BX][DI], '$'

	end_inline2:
	popa
ENDM
