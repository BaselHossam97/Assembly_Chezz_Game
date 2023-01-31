INCLUDE macro.asm
EXTRN name1:BYTE
EXTRN name1Size:BYTE
EXTRN name2:BYTE
EXTRN name2Size:BYTE
EXTRN Chat:FAR
EXTRN Game:FAR
PUBLIC MainMenu, DashedLine, PlayerSent

.model small
.386
.stack 64
.data  
ESC_ScanCode                EQU     01H
F1_ScanCode                 EQU     3BH
F2_ScanCode                 EQU     3CH
welcome_1 db 'Welcome $'
welcome_2 db 'You''re playing with $'
option1 db "Press F1 to enter chat mode",'$'
option2 db "Press F2 to enter Chess",'$'
option3 db "Press ESC to end the program",'$'
DashedLine db 80 dup('-'),'$'
ChatSend db " - You sent a chat invitation to $"
ChatReceive db " - Press F1 to accept chat invitation from $"
ChessSend db " - You sent a game invitation to $"
ChessReceive db " - Press F2 to accept game invitation from $"
GoodBye db 'GoodBye $'
notificationline db 23d
ChatFlag db 0
ChatSent db 0
ChatReceived db 0
ChessFlag db 0
ChessSent db 0
ChessReceived db 0
PlayerSent db 0
.code

InitializeMainMenu proc
    
	StartTextMode

    GoToXY 10d, 0d
    DisplayString welcome_1
    DisplayString name1
    DisplayChar '!'

    GoToXY 10d, 2d
    DisplayString welcome_2
    DisplayString name2
    DisplayChar '!'

    GoToXY 20d, 4d
    DisplayString option1

    GoToXY 20d, 10d
    DisplayString option2

    GoToXY 20d, 16d
    DisplayString option3

    GoToXY 0, 22d                
    DisplayString DashedLine

    HideTheCursor

    ret
InitializeMainMenu ENDP

MainMenu PROC FAR

    call InitializeMainMenu
    
    send:
    HideTheCursor
    mov ah, 1                   ;Check if there is a character to send
    int 16h
    jz receive                  ;No character to send, go to receive
    mov ah,0
    int 16h
    push ax                     ;Check which key is pressed
    cmp ah,F1_ScanCode
    jz pressedF1
    cmp ah,F2_ScanCode
    jz pressedF2
    cmp ah,ESC_ScanCode
    jz pressedEsc
    jmp receive                 ;Not a valid key pressed, check if a key is received

    pressedF1:                  ;Chat key pressed
    mov ChessFlag, 0            
    mov ChessSent, 0
    mov ChessReceived, 0
    mov ChatSent, 1             
    mov PlayerSent,0  
    cmp ChatReceived, 1
    jne go_on

    mov ChatFlag, 1
    jmp Send_Char

    go_on:
    cmp notificationline, 25d   ;If notification bar full, scroll down
    jne update_notification
    dec notificationline
    ScrollDown 0, 23d, 79d, 24d
    
    update_notification:        ;Update notification bar
    GoToXY 0, notificationline
    DisplayString ChatSend
    DisplayString name2
    inc notificationline

    jmp Send_Char               ;Send the key pressed to player 2
    
    pressedF2:                  ;Chess key pressed
    mov ChatSent, 0
    mov ChatReceived, 0
    mov ChatFlag, 0
    mov ChessSent, 1             
    mov PlayerSent,1    
    cmp ChessReceived, 1
    jne go_on1

    mov ChessFlag, 1
    jmp Send_Char

    go_on1:
    cmp notificationline, 25d
    jne update_notification1
    dec notificationline
    ScrollDown 0, 23d, 79d, 24d
    
    update_notification1:
    GoToXY 0, notificationline
    DisplayString ChessSend
    DisplayString name2
    inc notificationline

    jmp Send_Char

    pressedEsc:                 ;Send the key then end

    ;Send to player 2
    Send_Char:
    pop ax
    mov dx , 3FDH		        ;Line Status Register
 	In al , dx 			        ;Read Line Status
	AND al , 00100000b
	JZ receive                  
    mov dx , 3F8H		        ;Transmit data register
	mov al, ah                  ;Send the scan code which was in ah
    out dx , al

    cmp ah,ESC_ScanCode         ;If esc pressed, end without receiving
    je receivedEsc

    cmp ChatFlag, 1             ;Check if Chat is to be started now
    je StartChat

    cmp ChessFlag, 1            ;Check if Chess is to be started now
    je StartChess

    jmp receive

    receive:
    mov dx , 3FDH		        ; Line Status Register	
	in al , dx 
	AND al , 1
	JZ send                     ;Nothing to receive, try to send
    mov dx , 03F8H
	in al , dx 

    push ax
    cmp al,F1_ScanCode          ;Received key in al
    jz receivedF1
    cmp al,F2_ScanCode
    jz receivedF2
    cmp al,ESC_ScanCode
    jz receivedEsc
    jmp send                    ;Not a valid character received, try to send

    receivedF1:
    mov ChessFlag, 0
    mov ChessSent, 0
    mov PlayerSent,0
    mov ChessReceived, 0
    mov ChatReceived, 1
    cmp ChatSent, 1
    je StartChat

    cmp notificationline, 25d
    jne update_notification2
    dec notificationline
    ScrollDown 0, 23d, 79d, 24d
    
    update_notification2:
    GoToXY 0, notificationline
    DisplayString ChatReceive
    DisplayString name1
    inc notificationline

    jmp send
    
    receivedF2:
    mov ChatFlag, 0
    mov ChatSent, 0
    mov ChatReceived, 0
    mov playerSent,0
    mov ChessReceived, 1
    cmp ChessSent, 1
    je StartChess

    cmp notificationline, 25d
    jne update_notification3
    dec notificationline
    ScrollDown 0, 23d, 79d, 24d
    
    update_notification3:
    GoToXY 0, notificationline
    DisplayString ChessReceive
    DisplayString name1
    inc notificationline

    jmp send

    StartChat:
    call chat
    call InitializeMainMenu
    jmp restart

    StartChess:
    call Game
    call InitializeMainMenu
    jmp restart

    restart:
    mov ChatSent, 0
    mov ChatReceived, 0
    mov ChatFlag, 0
    mov PlayerSent,0
    mov ChessSent, 0
    mov ChessReceived, 0
    mov ChessFlag, 0
    jmp send

    receivedEsc:
    StartTextMode
    GoToXY 0, 0
    DisplayString GoodBye
    DisplayString name1
    DisplayChar '!'
    ; return control to operating system
    MOV AH , 4ch
    INT 21H
    ret
MainMenu ENDP 
END 