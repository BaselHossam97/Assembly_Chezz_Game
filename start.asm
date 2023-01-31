INCLUDE macro.asm

EXTRN MainMenu:FAR
PUBLIC name1Size, name1, name2Size, name2, actualSize, Empty_Line

.model large
.386
.stack 64
.data  
actualSize db ?
message1 db "Please Enter your name: ",'$' 
message2 db 'The Username should not exceed 15 characters and start with a letter$'
name1Size db 16d, ?
name1 db 16 dup('$') 
name2Size db 16d, ?
name2 db 16 dup('$')
message3 db "Press Enter to continue",'$'
waiting_message db 'Waiting for Player 2 to connect...$'
Empty_Line db 79 dup(' '), '$'
.code

Connect PROC FAR
    StartTextMode
    GoToXY 5d, 5d
    DisplayString waiting_message
    HideTheCursor

    mov BX, 1d
    ;Send character 
    Send_Char:
    mov dx , 3FDH		; Line Status Register
 	In al , dx 			;Read Line Status
	AND al , 00100000b
	JZ Send_Char
    mov dx , 3F8H		; Transmit data register
	mov al, name1Size[BX]
    out dx , al
	
    ;Receive character
    Wait_To_Receive:
    mov dx , 3FDH		; Line Status Register	
	in al , dx 
	AND al , 1
	JZ Wait_To_Receive
    mov dx , 03F8H
	in al , dx 
    mov name2Size[BX], AL
    inc bx
    cmp bx, 16d
    jle Send_Char

    Ret
Connect ENDP

MAIN PROC FAR
    mov ax, @data
    mov ds, ax
    mov es,ax

    StartTextMode

    GoToXY 0, 1d
    DisplayString message2
    GoToXY 0, 0d
    DisplayString message1
    
    GetName:
    ;Clear username
    MOV BX, 0
    Name_Clear:
    MOV name1[BX], '$'
    INC BX
    CMP BX, 15d
    JLE Name_Clear

    ClearLine 0
    GoToXY 0, 0
    DisplayString message1
    ReadString name1Size
    ;Check that the name starts with a letter
    CMP name1[0], 'A'
    JB  GetName
    CMP name1[0], 'Z'
    JBE NameReceived

    CMP name1[0], 'a'
    JB  GetName
    CMP name1[0], 'z'
    JA  GetName

    NameReceived:
    ;Remove enter key from name
    mov bl,name1Size[1]
    mov bh,0
    mov dl,"$"
    mov name1[bx],dl

    GoToXY 5, 20d
    DisplayString message3

    HideTheCursor
    l:
    mov ah,0
    int 16h
    ;Go to main menu when Enter key pressed
    cmp ah, 1CH
    jz connect_with_player_two
    jmp l
    connect_with_player_two:
    ;Connect with player 2
    InitializeSerialComm
    call connect

    goToMenu:
    call MainMenu

    hlt
MAIN ENDP 
END MAIN
