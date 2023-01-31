INCLUDE macro.asm
EXTRN Name1:BYTE
EXTRN Name1Size:BYTE
EXTRN Name2:BYTE
EXTRN Name2Size:BYTE
EXTRN DashedLine:BYTE
EXTRN Empty_Line:BYTE
public Chat, InitializeInlineChat, SX, SY, RX, RY, sender_buffer, receiver_buffer, smallEmptyLine, smallTemp
public iterator1_x, iterator1_y, iterator2_x, iterator2_y
.model small
.386
.stack 64
.data
Notification_0 db ' - To end chatting with $'
Notification_1 db ' Press F3$'
Notification_2 db ' To continue chatting, scroll down to last message or press F3 to end chatting$'
Notification_3 db 'To end the game, Press F4$'		;For inline chat
SX db 0d
SY db 1d
RX db 0d
RY db 13d
Temp db 80 dup('$')
LastIndex dw 0
char db '$'
flag db 0
smallDashedLine db 28 dup('-'), '$'
sender_buffer db 250 dup('$')
receiver_buffer db 250 dup('$')
smallEmptyLine db 25 dup(' '), '$'
smallTemp db 26 dup('$')
iterator1_x dw 0
iterator1_y dw 0
iterator2_x dw 0
iterator2_y dw 0
Memory db 10000 dup('$')
.code

InitializeInlineChat PROC FAR
    ;Display dashed vertical line between chess and inline chat
	mov bl, 0
	draw_vertical_line:
	GoToXY 51, bl
	DisplayChar '|'
	inc bl
	cmp bl, 24d
	jle draw_vertical_line

	;Draw line under timer
	GoToXY 52, 1
	DisplayString smallDashedLine

	;Display Name1 with a char ':' at the end
	GoToXY 52,2
	DisplayString name1
	DisplayChar ':'

	;Display dashed line between sender and receiver
	GoToXY 52,12d
	DisplayString smallDashedLine
	
	;Display Name2 with a char ':' at the end
	GoToXY 52,13d
	DisplayString name2
	DisplayChar ':'

	;Display dashed line at the end
	GoToXY 52, 23d
	DisplayString smallDashedLine

	;Display message at the end
	GoToXY 52, 24d
	DisplayString Notification_3

	;Empty buffers
	MOV CX,250d
    MOV AL,'$'
    LEA DI,sender_buffer
    REP STOSB

	MOV CX,250d
    MOV AL,'$'
    LEA DI,receiver_buffer
    REP STOSB

	;Initialize cursors
	MOV SX,55d
	MOV SY,3d
	MOV RX,55d
	MOV RY,14d

	;Initialize iterators in buffers' array
    MOV iterator1_x , 0
    MOV iterator1_y , 0
    MOV iterator2_x , 0
    MOV iterator2_y , 0

    ret
InitializeInlineChat ENDP

InitializeChat proc
    
	StartTextMode
    
	;Display Name1 colored with a char ':' at the end
	GoToXY 02,00
	mov cl, name1Size[1]
	mov ch, 0
	mov bx, 0
	mov SX, 3
	colored_name:
    mov dl, name1[bx]
    mov char, dl
    GoToXY SX, 0
	DisplayCharColored char, 0Eh
	inc SX
	inc bx
	cmp bx, cx
	jle colored_name
	DisplayChar ':'

	;Display dashed line between sender and receiver
	GoToXY 00,11d
	DisplayString DashedLine
	
	;Display Name2 colored with a char ':' at the end
	GoToXY 02, 12d
	mov cl, name2Size[1]
	mov ch, 0
	mov bx, 0
	mov RX, 3
	colored_name1:
    mov dl, name2[bx]
    mov char, dl
    GoToXY RX, 12
	DisplayCharColored char, 0AH
	inc RX
	inc bx
	cmp bx, cx
	jle colored_name1
	DisplayChar ':'

	;Display Notification bar at the bottom of the screen
	GoToXY 0, 23d
	DisplayString DashedLine
	DisplayString Notification_0
	DisplayString name2
	DisplayString Notification_1

	HideTheCursor

	;Empties the memory to prevent ambiguity from quitting and re entering chat
	MOV CX,5000d
    MOV AX,'$'
    LEA DI,Memory
    REP STOSW

	;Initialize cursors
	MOV SX,5d
	MOV SY,1d
	MOV RX,5d
	MOV RY,13d

	;Initialize iterators in memory array
	MOV DI, 0
	MOV BX, 4
	MOV LastIndex, 0

    ret
InitializeChat ENDP

Chat PROC FAR

	call InitializeChat

	;This label is done because if user scrolled up and typed a letter, I should prohibit that and
	;inform him to go down to last message entered to prevent any ambiguity in storing messages.
	refreshNotification:
	ClearLine 24d
	DisplayString Notification_0
	DisplayString name2
	DisplayString Notification_1
	CMP flag, 1
	MOV flag, 0
	JE can_continue_chatting		;If flag = 1, scrolling down has reached the last line so go back with the character entered already

	start:
	GoToXY SX, SY
	;sender
	;If no character in buffer to send, go to receive
	MOV AH,1
	INT 16h
	JZ receive
	
	mov dx , 3FDH		; Line Status Register
 	In al , dx 			;Read Line Status
	AND al , 00100000b
	JZ receive

	MOV AH, 0			;Get the key entered in the buffer
	INT 16h

	;Check if F3 is pressed before transmitting
	CMP AH, 3DH
	JE end_chat1

	;Check if UP key is pressed to scroll up to previous messages
	CMP AH, 48H
	JE pressedUP

	;Check if DOWN key is pressed to scroll back down to last message sent
	CMP AH, 50H
	JE pressedDOWN

	CMP DI, LastIndex				;If user scrolled up and did not scroll back down
	JE can_continue_chatting
	GoToXY 0, 24d
	DisplayString Notification_2
	JMP start

	can_continue_chatting:
	CMP flag, 1
	JE refreshNotification			;Resets the notification bar
	
	;Check if enter is pressed
	CMP AL, 0DH
	JE enter0

	;Check if backspace is pressed
	CMP AL, 08H
	JE backspace

	;Check if character not printable so don't transmit
    CMP AL, ' '   	;Compare with lowest printable ascii value
    JB start
    CMP AL, '~'   	;Compare with highest printable ascii value
    JA start
	CMP AL, '$'		;When scrolling up and down, this prevents the entire message from being displayed
	JE start		;, so do not allow it to be printed

	;There is a printable character to transmit and display
	GoToXY SX, SY
	DisplayChar al
	MOV Memory[BX][DI], al

	inc SX
	inc BX
	;Go the next line if end of line reached or enter pressed
	CMP SX, 80d	
	JNE skip		

	enter0:
	MOV SX, 5				;Reset the cursors to the start of the line
	MOV BX, 4
	INC SY
	ADD DI, 80d
	MOV LastIndex, DI		;This is the index of the last line currently being entered
	;If end of sender section of the chat reached, scroll down by one line
	CMP SY, 11d
	JNE skip
	ScrollDown 5, 1, 79d, 10d 
	MOV SY, 10d				;adjust cursor position after scrolling
	JMP skip

	backspace:
	CMP SX, 5				;If at beginning of line, do nothing
	JE skip
	PUSH AX
	DEC SX
	DEC BX
	GoToXY SX, SY
	DisplayChar ' '
	MOV Memory[BX][DI], '$'
	POP AX
	
	skip:

	mov dx , 3F8H			;Transmit data register
	out dx , al
	jmp receive

	pressedUP:
	CMP DI, 720d			;No more lines to scroll up
	JLE start
	MOV flag, 1				;Flag that a scrolling has happened
	ScrollUp 5, 1, 79d, 10d ;Scroll the screen by one line, now one message left to retrieve from memory
	SUB DI, 80				;Move UP one line
	PUSH DI
	SUB DI, 720				;Move up one screen's worth of messages
	MOV DX, OFFSET Memory	;Load the messages into SI
	ADD DX, DI				;Actual Y of message in 2D array of messages
	ADD DX, 4				;Actual X of message in 2D array of messages
	MOV SI, DX
	LEA DI, Temp			;Load the message into a temp variable to print the message
	mov cx, 75				;Message is 75 characters maximum
	REP MOVSB
	GoToXY 5, 1				
	DisplayString Temp
	POP DI
	JMP start

	pressedDOWN:
	CMP LastIndex, DI
	JE refreshNotification
	ScrollDown 5, 1, 79d, 10d 	;Scroll the screen by one line, now one message left to retrieve from memory
	ADD DI, 80					;Move DOWN one line
	PUSH DI
	MOV DX, OFFSET Memory		;Load the messages into SI
	ADD DX, DI					;Actual Y of message in 2D array of messages
	ADD DX, 4					;Actual X of message in 2D array of messages
	MOV SI, DX
	LEA DI, Temp				;Load the message into a temp variable to print the message
	mov cx, 75					;Message is 75 characters maximum
	REP MOVSB
	GoToXY 5, 10				
	DisplayString Temp
	POP DI
	JMP start

	;Transmit dollar sign instead of F3 scan code because ASCII of '=' is the same as scan code of F3.
	;Since we receive value in AL only, there was no easier way to distinguish between them, so we opted 
	;to send dollar sign instead
	end_chat1:					;Transmitting scan code of F3
	mov dx , 3F8H				; Transmit data register
	mov al, '$'
	out dx , al
	jmp end_chat

	;reciever
	;Check that Data Ready
	receive:
		mov dx , 3FDH		; Line Status Register	
		in al , dx 
		AND al , 1
		JZ start

	;If Ready read the VALUE in Receive data register
		mov dx , 03F8H
		in al , dx 
		;mov VALUE , al

		;Check if F3 is pressed before receiving
		CMP al, '$'
		JE end_chat

		;Check if enter is pressed
		CMP al, 0DH
		JE enter1

		;Check if backspace is pressed
		CMP al, 08H
		JE backspace1

		;Printable character to be displayed
		GoToXY RX, RY
		DisplayChar al
		INC RX

		;Check if end of line reached or enter key pressed
		CMP RX, 80d
		JNE skip1

		enter1:
		MOV RX, 5
		INC RY
		;If end of receiver section in chat reached, scroll down by one line
		CMP RY, 23d
		JNE skip1
		ScrollDown 5, 13d, 79d, 22d
		MOV RY, 22d			;adjust cursor position
		JMP skip1
		
		backspace1:
		;If at beginning of the line, do nothing
		CMP RX, 5
		JE skip1
		DEC RX
		GoToXY RX, RY
		DisplayChar ' '

		skip1:
		JMP start

		end_chat:
 		Ret
Chat ENDP
END