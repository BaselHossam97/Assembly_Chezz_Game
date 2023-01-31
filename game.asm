Include macro.asm
EXTRN DrawBoard:FAR
EXTRN DrawTile:FAR
EXTRN DrawTimer:FAR
EXTRN moveCursor1Right:FAR
EXTRN moveCursor1Down:FAR
EXTRN moveCursor1Left:FAR
EXTRN moveCursor1Up:FAR
EXTRN moveCursor2Right:FAR
EXTRN moveCursor2Down:FAR
EXTRN moveCursor2Left:FAR
EXTRN moveCursor2Up:FAR
EXTRN Pawn:FAR
EXTRN rook:FAR
EXTRN bishop:FAR
EXTRN king:FAR
EXTRN horse:FAR
EXTRN check:FAR
EXTRN name1:BYTE
EXTRN name2:BYTE
EXTRN name1Size:BYTE
EXTRN name2Size:BYTE
EXTRN PlayerSent:BYTE
EXTRN InitializeInlineChat:FAR
EXTRN SX:BYTE
EXTRN SY:BYTE
EXTRN RX:BYTE
EXTRN RY:BYTE
EXTRN sender_buffer:BYTE
EXTRN receiver_buffer:BYTE
EXTRN smallEmptyLine:BYTE
EXTRN smallTemp:BYTE
EXTRN iterator1_x:WORD
EXTRN iterator1_y:WORD
EXTRN iterator2_x:WORD
EXTRN iterator2_y:WORD
PUBLIC X, Y, CX1, CX2 , CY1 , CY2, Filename, refresh, Cursor1, Cursor2, convertToArray, GameBoard, PossibleMoves, convertToBoard, selectedX, selectedY,colourOfPiece, Game
PUBLIC kingCheck,kingX,kingY,printTime,convertToLoc,lineClear,notification,ignoreFlag,oppositecolour,sendFlag

.model huge
.386
.stack 64
.Data
    TileDim EQU 45
    X DW 0
    Y DW 0
	Filename DB 'B1', 0
	Board DW 'B1', 'B2', 'B3', 'B4'
    OriginalBoardWhite DW 'BR', 'BH', 'BB', 'BQ', 'BK', 'BB', 'BH', 'BR', 8 dup ('BP'), 32 dup(0), 8 dup ('WP'), 'WR', 'WH', 'WB', 'WQ', 'WK', 'WB', 'WH', 'WR'
	OriginalBoardBlack DW 'WR', 'WH', 'WB', 'WK', 'WQ', 'WB', 'WH', 'WR', 8 dup ('WP'), 32 dup(0), 8 dup ('BP'), 'BR', 'BH', 'BB', 'BK', 'BQ', 'BB', 'BH', 'BR'
	GameBoard DW 64 dup (0)
    myColour db 0
    oppositeColour db 0
    myTimer db 3
    opponentTimer db 3
    bonusMinute db 0
    bonusSecond db 0
    bonusDone db 0
    bonusX db 0
    bonusY db 8
    bonusTile dw 'CT'

    KilledBoard DW  'BP', 'BR', 'BH', 'BB', 'BQ', 'WP', 'WR', 'WH', 'WB', 'WQ'
    KilledCounter DW 10 dup('00')
    Timers Dw 'T1', 'T2', 'T3'
    selectedTile dw 'ST'
    possibleTile dw 'PT'
    enemyTile dw 'RT'
    numberKilled db 'x $'

    animatedPieceRed dw 0
    animatedPieceBlue dw 0
    colourOfPiece db 0
    ignoreFlag db 0
    animateFlag db 0
    stepXRed dw 0
    stepYRed dw 0
    stepXBlue dw 0
    stepYBlue dw 0
    oldXRed dw 0
    oldYRed dw 0
    oldXBlue dw 0
    oldYBlue dw 0
    gamecounter dw 0
    ;red
    Cursor1 DW 'C1'
    CX1 DW 20
    CY1 DW 20
    kingX1 dw 08d
    kingY1 dw 00d
    selectedRedPiece db 0

    ;AvailableRedMoves dw 36 dup('K')
    StartXRed dw 00
    StartYRed dw 00
    EndXRed dw 00
    EndYRed dw 00
    StartXBlue dw 00
    StartYBlue dw 00
    EndXBlue dw 00
    EndYBlue dw 00
    Message db 0
    sendFlag db 0
    partFlag db 0

    selectedX2 dw 200h
    selectedY2 dw 200h
    kingX2 dw 08d
    kingY2 dw 112d
    selectedBluePiece db 0h
    Cursor2 DW 'C2'
    CX2 DW 335
    CY2 DW 335
    BluePossibleMoves DW 64 dup (00h)

    PossibleMoves DW 64 dup(00h)
    Timer DW 64 dup(00h)
    Seconds DB 00h
    OldTime DB 00h
    Oldms DB 00h
    Minutes DB 00h
    selectedX dw 200h
    selectedY dw 200h
    XPosition dw 0
    YPosition dw 0
    SelectedPiece db 0h
    kingX dw 200h
    kingY dw 200h
    Black DW 'BT'
    White DW 'WT' 
    WinFlag DB 0
    selectedKilledPiece db 0h
    two_seconds db 0
    Won db ' Won!!!!$'
    kingCheck DB "'s King is in danger$"
    StatusLine db 80 dup('-'),'$'
    lineClear db 36 dup(' '),'$'
    notification db 36 dup('$')
    pawnIsKilled db "'s pawn is killed$"
    horseIsKilled db "'s knight is killed$"
    rookIsKilled db "'s rook is killed$"
    bishopisKilled db "'s bishop is killed$"
    queenIsKilled db "'s queen is killed$"

.Code

set PROC FAR
    push di
    push si
    push cx
    push ax

    ;set the gameboard to get original positions of pieces
    ;check if the player will play with white or black colour
    ;depending on whether he sent the invitation or not
    cmp PlayerSent,1
    jnz WhitePlayer
    lea si,OriginalBoardBlack
    mov myColour,'B'
    mov oppositeColour, 'W'
    mov kingX1,06d
    mov kingY1,00d
    mov kingX2,06d
    mov kingY2,112d
    jmp copyBoard

	WhitePlayer:
	lea si, OriginalBoardWhite
    mov myColour, 'W'
    mov oppositeColour, 'B'
    mov kingX1,08d
    mov kingY1,00d
    mov kingX2,08d
    mov kingY2,112d
    copyBoard:
    lea di,GameBoard
    mov cx,64
    REP MOVSW

    ;set the cursor position and king position for player 2
    mov CX2,335
    mov CY2,335
    mov selectedX2, 200h
    mov selectedY2, 200h
	mov selectedBluePiece, 0h
    
    ;reset all the possible moves of the blue pieces to zero
    mov cx,64
    mov ax,00
    lea di,BluePossibleMoves
    REP STOSW

    ;reset all the possible moves of the pieces to zero
    mov cx,64
    mov ax,00
    lea di,PossibleMoves
    REP STOSW

    ;reset all the timers to zero
    mov cx,64
    mov ax,0
    lea di,Timer
    REP STOSW

    ;reset the variables for the animation
    mov animatedPieceRed, 0
    mov animatedPieceBlue, 0
    mov colourOfPiece, 0
    mov animateFlag, 0
    mov ignoreFlag, 0
    mov stepXRed, 0
    mov stepYRed, 0
    mov stepXBlue, 0
    mov stepYBlue, 0
    mov oldXRed, 0
    mov oldYRed, 0
    mov oldXBlue, 0
    mov oldYBlue, 0
    mov StartXRed, 00
    mov StartYRed, 00
    mov EndXRed, 00
    mov EndYRed, 00
    mov StartXBlue, 00
    mov StartYBlue, 00
    mov EndXBlue, 00
    mov EndYBlue, 00
    mov gamecounter, 00

    ;reset the x and y position of the bonus and the timers of players 1 and 2
    mov bonusY,8
    mov bonusX,0
    mov myTimer,3
    mov opponentTimer,3

    ;reset the 'Clock' to zero,win condition and the message for serial communication
    mov Seconds,0
    mov Minutes,0
    mov bonusMinute,0
    mov bonusSecond,0
    mov bonusDone,0
    mov WinFlag,0
    mov X,0
    mov Y,0
    mov message,0

    ;reset the killed pieces' counter to 0
    mov cx,10
    mov ax,'00'
    lea di,KilledCounter
    REP STOSW

    pop ax
    pop cx
    pop si
    pop di
    RET
set ENDP

printTime PROC FAR
    push dx
    push bx
    push ax

    ;print the minutes on the screen
    mov ax,0
    mov bl,10
    mov al,Minutes
    div bl
    mov dh,ah
    mov ah,0eh
    add al,30h
    int 10h
    mov al,dh
    add al,30h
    int 10h

    mov al,':'
    int 10h

    ;print the seconds on the screen
    mov ax,0
    mov bl,10
    mov al,Seconds
    div bl
    mov dh,ah
    mov ah,0eh
    add al,30h
    int 10h
    mov al,dh
    add al,30h
    int 10h

    pop ax
    pop bx
    pop dx
    RET
printTime ENDP

convertToLoc  PROC FAR
    push ax
    push cx
    
    mov cx,0
    mov ax,Y
    mov cl,16d                      ;we multiplied by 16 because the array is actually made up of words so we actually have 8 rows and each column consists of two bytes hence one word
    mul cl
    mov bx,ax

    mov ax,X
    mov cl,2                        ;we multiplied by 2 because each column is one word 
    mul cl
    mov di,ax

    pop cx
    pop ax
    ret
convertToLoc ENDP

convertToArray PROC FAR
    push ax
    push bx

    mov ax, X       ;getting board position on x from X
    sub ax, 20       ;remove frame offset
    mov bl, TileDim    
    div bl          ;convert to array index by dividing by tile dim
    mov X, ax       ;update by x index
    mov ax, Y       ;getting board position on y from Y
    sub ax, 20       ;remove frame offset
    div bl          ;convert to tile index by dividing by tile dim
    mov Y, ax       ;update by x index

    pop bx
    pop ax
    RET
convertToArray ENDP

convertToBoard PROC FAR
    push ax
    push bx

    mov ax, X       ;getting array index position
    mov bl, TileDim
    mul bl          ;convert to board x position by multiplying by tile width
    add ax, 20       ;adding frame as offset
    mov X, ax       ;update by x position on board

    mov ax, Y       ;getting array index position
    mul bl          ;convert to board y position by multiplying by tile width
    add ax, 20       ;adding frame as offset
    mov Y, ax       ;update by x position on board

    pop bx
    pop ax
    RET
convertToBoard ENDP

displayNotification PROC FAR
    push si
    push di
    push cx

    ;reset the notification to be able to use the interrupt 21/9 to print a string easily
    mov cx,36
    mov al,'$'
    lea di,notification
    REP STOSB

    ;check which player lost a piece
    mov al,oppositeColour
    cmp colourOfPiece,al
    jz player2

    ;move the name of player 1 to the notification
    mov cx,0
    mov cl,name1Size+1
    lea si,name1
    lea di,notification
    REP MOVSB

    ;move the position that DI is pointing to in the notification to be after the name
    lea di,notification
    mov cx,0
    mov cl,name1Size+1
    add di,cx
    jmp whatIsKilled

    ;move the name of player 2 to the notification
    player2:
    mov cx,0
    mov cl,name2Size+1
    lea si,name2
    lea di,notification
    REP MOVSB

    ;move the position that DI is pointing to in the notification to be after the name
    lea di,notification
    mov cx, 0
    mov cl, name2Size+1
    add di, cx
    
    ;check what type of piece was killed
    whatIsKilled:
    cmp selectedKilledPiece,'P'
    jnz rookKilled
    mov cx,17
    lea si,pawnIsKilled
    jmp display
    rookKilled:
    cmp selectedKilledPiece,'R'
    jnz horseKilled
    mov cx,17
    lea si,rookIsKilled
    jmp display
    horseKilled:
    cmp selectedKilledPiece,'H'
    jnz bishopKilled
    mov cx,19
    lea si,horseIsKilled
    jmp display
    bishopKilled:
    cmp selectedKilledPiece,'B'
    jnz queenKilled
    mov cx,19
    lea si,bishopIsKilled
    jmp display
    queenKilled:
    cmp selectedKilledPiece,'Q'
    jnz kingIsDead
    mov cx,18
    lea si,queenIsKilled
    
    ;move the rest of the text to the notification 
    display:
    REP MOVSB

    ;move the cursor to the position of the status bar and clear it in case there was a notification present 
    mov dh, 29
    mov dl, 0
    mov bh,00h
    mov ah,2
    int 10h

    lea dx, lineClear
    mov ah,9
    int 21h
    
    ;print the notification at the status bar
    mov dh, 29
    mov dl, 0
    mov bh,00h
    mov ah,2
    int 10h

    lea dx, notification
    mov ah,9
    int 21h

    ;print the time at which the notification occurred
    mov dh, 29
    mov dl, 75
    mov bh,00h
    mov ah,2
    int 10h

    call printTime

    kingIsDead:

    pop cx
    pop di
    pop si
    RET
displayNotification ENDP

;refreshing cells
refresh PROC FAR
    push ax
    push bx
    push cx
    push dx
    push di

    ;draw animated piece only at selected point on its path and exit refresh
    cmp animateFlag, 1
    jne not_red_animated_piece
    mov ah, byte ptr animatedPieceRed
    mov al, byte ptr animatedPieceRed+1
    mov word ptr Filename, ax
    call DrawTile
    jmp endRefresh
    not_red_animated_piece:
    cmp animateFlag, 2
    jne normal
    mov ah, byte ptr animatedPieceBlue
    mov al, byte ptr animatedPieceBlue+1
    mov word ptr Filename, ax
    call DrawTile
    jmp endRefresh
    normal:

    call convertToArray             ;to get x and y in the array
    mov ax, X
    add ax, Y
    mov cl,2
    div cl                          ;we divide by 2 to let us know if there is a black or white tile (white if even, black if odd)

    call convertToBoard             ;convert the x and y back to their original form on the screen in pixels to draw the tile

    cmp ah,1
    jz blackTile
    mov BH, byte ptr White          ;get picture of white tile
    mov BL, byte ptr White+1
    mov word ptr Filename, BX
    jmp checkIfSelected2
    blackTile:
    mov BH, byte ptr Black          ;get picture of black tile
    mov BL, byte ptr Black+1
    mov word ptr Filename, BX
    mov ignoreFlag,1

    checkIfSelected2:                       ;check if selected by cursor 2
    mov ax, selectedX2
    cmp X, ax
    jnz drawtheTile
    mov ax, selectedY2
    cmp Y, ax
    jnz drawtheTile
    mov BH, byte ptr selectedTile          ;get picture of selected tile
    mov BL, byte ptr selectedTile+1
    mov word ptr Filename, BX
    
    drawtheTile:
    call drawTile                   ;drawing the tile

    call convertToArray             ;to get x and y in the array to know if there is a chess piece on the tile
    call convertToLoc

    call convertToBoard             ;convert the x and y back to their original form on the screen in pixels to draw the tile 

    checkForBlue:
    cmp byte ptr BluePossibleMoves[BX][DI], 1   ;check if possible move for blue
    jnz DrawBonus
    cmp byte ptr GameBoard[BX][DI],0            ;check if there is an opponent piece to draw a red tile instead of green tile   
    jz noenemy
    mov ch, byte ptr enemyTile
    mov cl, byte ptr enemyTile+1
    jmp loadTile
    noenemy:
    mov ch, byte ptr possibleTile
    mov cl, byte ptr possibleTile+1
    loadTile:
    mov word ptr Filename, CX
    call DrawTile

    DrawBonus:
    call convertToArray                         ;get the position as an array to compare with x and y positions of the bonus
    mov al,byte ptr X
    mov ah,byte ptr Y
    call convertToBoard
    cmp bonusX,al                               ;check if you need to draw the bonus or not
    jnz DrawPiece
    cmp bonusY,ah
    jnz DrawPiece
    mov ch,byte ptr bonusTile
    mov cl,byte ptr bonusTile+1
    mov word ptr Filename,CX
    add X,10                                    ;move the x and y to draw the bonus in the middle of the tile
    add Y,10
    call drawTimer
    sub X,10
    sub Y,10

    DrawPiece:

    mov ch, byte ptr GameBoard[BX][Di]
    mov cl, byte ptr GameBoard[BX][Di]+1
    mov word ptr Filename, CX
    cmp ch,0                        ;check if there is a chess piece on the tile or not (if 0 then there is no chess piece)
    jz  endPiece
    call DrawTile                   ;draw chess piece if needed
    endPiece:

    ;check if there is Cursor2 on the tile or not (if 0 then there is no chess piece)
    mov cx, CX2
    cmp cx, X
    jnz endCursor2
    mov cx, CY2
    cmp cx, Y
    jnz endCursor2 
    mov ch, byte ptr Cursor2
    mov cl, byte ptr Cursor2+1
    mov word ptr Filename, CX
    call DrawTile                   ;draw cursor2 if needed
    endCursor2:

    ;draw the timer for a piece if needed
    cmp byte ptr Timer[BX][DI],0
    jz endRefresh
    add X, 10d
    add Y, 10d
    
    cmp byte ptr Timer[BX][DI], 3
    jne check3
    mov word ptr Filename, '3T'
    check3:
    cmp byte ptr Timer[BX][DI], 2
    jne check2
    mov word ptr Filename, '2T'
    check2:
    cmp byte ptr Timer[BX][DI], 1
    jne check1
    mov word ptr Filename, '1T'
    check1:
    CALL drawTimer

    sub X, 10d
    sub Y, 10d

    endRefresh:
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    RET
refresh ENDP

time PROC FAR
    push ax
    push cx
    push dx
    push di

    ;get system time
    mov ah,2ch
    int 21h
    mov oldms, dl
    cmp dh,OldTime              ;if oldtime is the same as before then do not do anything
    jz endTime
    inc Seconds

    lea di,Timer
    mov X, 20
    mov Y, 20
    decTimer:                   ;loop to decrement timer for recently moved pieces
    cmp byte ptr [di], 0
jz refreshOldTimer
    dec byte ptr [di]
    call refresh                ;refresh the timer drawn on the piece
refreshOldTimer:
    add X, TileDim
    add di, 2
    cmp X, 380d         ;check if we reached end of board
    JNE decTimer
    mov X, 20
    add Y, TileDim
    cmp Y, 380d
jnz decTimer

    ;update the old time, then increment the seconds and check if seconds reached 60
    ;then increment the minutes and reset seconds to 0
    mov OldTime,dh
    cmp Seconds,60d
    jz incrementMinutes
    jmp print

    incrementMinutes:
    inc Minutes
    mov Seconds,00h

    ;print the new time on the screen
    print:
    mov dh, 0
    mov dl, 62
    mov bh,00h
    mov ah,2
    int 10h

    call printTime

    endTime:
    pop di
    pop dx
    pop cx
    pop ax
    RET
time ENDP

refreshPossibleBlue PROC FAR
    push ax
    push bx
    push cx
    push di
    push si

    MOV X, 20
    MOV Y, 20
    mov di, offset BluePossibleMoves
zeroArray1:                 ;loop to empty possible moves array
    cmp byte ptr [di], 1
jnz refreshOldBlue
    mov byte ptr [di], 0
    call refresh            ;remove the possible move (Green) tile
refreshOldBlue:
    add X, TileDim
    add di, 2
    cmp X, 380d         ;check if we reached end of board
    JNE zeroArray1
    mov X, 20
    add Y, TileDim
    cmp Y, 380d
jnz zeroArray1

    ;colour of piece as well as its X and Y positions are needed to get its possible moves
    mov bl,myColour
    mov colourOfPiece,bl

    mov bx,selectedX2
    mov selectedX,bx
    mov bx,selectedY2
    mov selectedY,bx

    ;check what type of piece is selected
    cmp selectedBluePiece,'P' 
    jnz blueRook
    call Pawn
    jmp blueEnd

    blueRook:
    cmp selectedBluePiece,'R'
    jnz blueBishop
    call rook
    jmp blueEnd

    blueBishop:
    cmp selectedBluePiece,'B'
    jnz blueHorse
    call bishop
    jmp blueEnd

    blueHorse:
    cmp selectedBluePiece,'H'
    jnz blueQueen
    call horse
    jmp blueEnd

    blueQueen:
    cmp selectedBluePiece,'Q'
    jnz blueKing
    call rook
    call bishop
    jmp blueEnd

    blueKing:
    cmp selectedBluePiece,'K'
    jnz blueEnd
    call king
    jmp blueEnd

    blueEnd:

    MOV X, 20
    MOV Y, 20
    mov di, offset BluePossibleMoves
    mov si, offset PossibleMoves
DrawPossibleBlue:            ;redraw possible move tiles to show green background
    cmp byte ptr [si], 1
jnz notPossibleBlue
    mov byte ptr [di], 1
    mov byte ptr [si], 0
    call refresh
notPossibleBlue:
    add X, TileDim
    add di, 2
    add si, 2
    cmp X, 380d         ;check if we reached end of board
    JNE DrawPossibleBlue
    mov X, 20
    add Y, TileDim
    cmp Y, 380d
jnz DrawPossibleBlue
    
    pop si
    pop di
    pop cx
    pop bx
    pop ax
    RET
refreshPossibleBlue ENDP

UpdateKilledCounter PROC FAR
    push cx
    push di
    push bx

    mov ah, byte ptr GameBoard[BX][Di]+1
    mov al, byte ptr GameBoard[BX][Di]
    cmp al,'K'                      ;check if king is killed then no counter is required to be incremented
    jz kingKilled

    ;get first position of KilledBoard (needed in both di and bx to get the index of the piece killed)
    lea di,KilledBoard
    lea bx,KilledBoard
    mov cx,0
    mov cl,10
    REPNE SCASW                         ;finding which piece is killed
    sub di,bx                           ;finding index
    mov bx,di
    dec bx                          
    inc byte ptr KilledCounter[bx]      ;increment the number of killed pieces
    mov cl,byte ptr KilledCounter[bx]   ;number to be printed
    shl bl,2                            ;we know it is wrong according to our syllabus, but it works 
                                        ;shl to multiply by 4 to know position in screen as text mode
    add bl,3
    
    GoToXY bl,26                        ;move the cursor to the required position
    DisplayChar cl                      

    kingKilled:
    pop bx
    pop di
    pop cx
    ret
UpdateKilledCounter ENDP

createMessage PROC FAR
    pusha

    mov dx,0h

    ;convert the X and Y positions to array coordinates (0 - 7) to be able to send both in one message  
    mov ax,selectedX                
    mov X,ax
    mov ax,selectedY
    mov Y,ax
    call convertToArray

    xor dx,x

    mov cl,3
    rol Y,cl
    xor dx,Y

    ;return the position of the piece in X and Y as it is needed later in selectBlue
    mov ax,selectedX2
    mov X,ax
    mov ax,selectedY2
    mov Y,ax
    call convertToArray
    
    ;check if you are sending the old or new position of the piece to check the 7th bit at the receiving end
    cmp partFlag,0
    jnz sendEnd
    or dx,0080h
    jmp sendMessage
    sendEnd:
    or dx,00c0h

    sendMessage:
    mov Message,dl

    popa
    ret
createMessage ENDP

decryptMessage Proc FAR
    pusha
    ;check if the message contains position of piece, character for chatting or any other function 
    ;if the 8th bit is 1 then it is a position of a piece, if it is 0 then it is chat or any other function
    mov cl, 80H
    mov dx, ax
    and dl, cl
    jnz not_chat
    ;check if this is a position to draw the bonus 
    ;(0 - 7) is X position and (8 - 15) is Y position + 8 to differentiatie between X and Y
    cmp al, 15
    ja inline
    cmp al, 7
    ja Yrange
    mov bonusX,7                ;as the board is flipped
    sub bonusX, al
    jmp receiveMessage
    Yrange:
    sub al, 8                   ;remove the extra offset (8)
    mov bonusY,7                ;as the board is flipped
    sub bonusY, al
    mov al,bonusY
    
    mov byte ptr Y, al
    mov al, bonusX
   
    mov byte ptr X, al      
    call convertToBoard
    call refresh
    jmp receiveMessage
    inline:
    inline_Player2 al           ;this macro handles the receiving section of the inline chat
    jmp receiveMessage
    not_chat:
    ;convert the message into X and Y positions
    ;we subtract X or Y from 7 as the board is flipped
    mov cl, 3
    
    mov X,ax
    and X,7
    mov dx,7
    sub dx,X
    mov X,dx

    ror ax,cl
    mov Y,ax
    and Y,7
    mov dx,7
    sub dx,Y
    mov Y,dx

    ;check if the position is a start or end position of the piece
    call convertToBoard
    ror ax,4
    jc endPart
    ;we moved the positions into start and end here to stop a wrong animation from ocurring
    mov dx,X
    mov startXRed,dx
    mov EndXRed, dx
    mov dx,Y
    mov startYRed,dx
    mov EndYRed, dx
    jmp receiveMessage

    endPart:
    mov dx,X
    mov EndXRed,dx 
    mov dx,Y
    mov EndYRed,dx
    
    receiveMessage:
    popa
    ret
decryptMessage ENDP

SelectBlue PROC FAR
    push ax
    push bx
    push cx
    push di

    mov ax,CX2
    mov X,ax
    mov ax,CY2
    mov Y,ax
    
    call convertToArray
    call convertToLoc
    
    mov cl, byte ptr GameBoard[BX][Di]+1    ;check if a piece of the player's colour is being selected
    cmp cl,myColour
    jnz tryMoveBlue                         ;if not then check if user is trying to move an already selected piece to another location
    cmp byte ptr Timer[BX][DI],0
    ja endSelectBlue
    mov cl, byte ptr GameBoard[BX][DI]      ;get the type of the piece
    mov selectedBluePiece, cl                
    cmp selectedX2, 200h                    ;to check if there was a piece already selected
    jz notRefreshBefore2
    mov ax, selectedX2                      ;if there was already a selected piece
    mov X, ax                               ;then you need to refresh the background of the previous piece
    mov ax, selectedY2                      ;as you have the position of the old piece in selectedX1 and selectedY1
    mov Y, ax
    mov ax, CX2
    mov selectedX2, ax
    mov ax, CY2
    mov selectedY2, ax
    call refresh
    notRefreshBefore2:                      ;if there was no already selected piece
    mov ax, CX2                             ;then refresh the new position that the cursor is pointing to
    mov selectedX2, ax                      ;to a yellow background to mark the piece as selected
    mov X,ax
    mov ax, CY2
    mov selectedY2, ax
    mov Y,ax
    call refresh                
    call refreshPossibleBlue                ;marks the possible moves of the new selected piece

    mov ax,selectedX2                       ;creates the message of the starting X and Y positions to send to the other player
    mov selectedX,ax
    mov ax,selectedY2
    mov selectedY,ax
    mov partflag,0                          ;as it is a starting position
    call createMessage
    mov sendflag,1
    
    jmp endSelectBlue
    tryMoveBlue:
    cmp selectedBluePiece,0                 ;check if a piece is selected by player 0:not selected
    jz endSelectBlue1
    mov ax,CX2
    mov X,ax
    mov ax,CY2
    mov Y,ax
    call convertToArray
    call convertToLoc
    cmp byte ptr BluePossibleMoves[BX][DI],0    ;check if the selected move is possible or not
    endSelectBlue1:                             ;we had a problem of (jump out of range by 2 bytes) so we resorted to this solution (dividing the jumps) 
    jz endSelectBlue
    mov ch,selectedBluePiece
    cmp Y,0
    jnz notPromoteBlue                          ;check if a pawn reaches end of board and if yes, promote to queen
    cmp selectedBluePiece,'P'
    jnz notPromoteBlue
    mov ch, 'Q'
    notPromoteBlue:
    cmp selectedBluePiece,'K'                   ;check if the king moved then update its X and Y position
    jnz con2
    mov kingX2,di
    mov kingY2,bx
    con2:
    
    mov al,bonusX                               ;apply bonus affect when player steps on bonus tile
    cmp al,byte ptr X            
    jnz opposite
    mov al,bonusY
    cmp al,byte ptr Y
    jnz opposite
    dec myTimer
    mov bonusY,8                                ;to remove bonus from board as 8 is an invalid position
    opposite:
    
    mov al,oppositeColour
    cmp byte ptr GameBoard[BX][DI]+1, al       ;check if an opponent piece is killed
    jnz moveNewBluePiece
    cmp byte ptr GameBoard[BX][DI],'K'          ;if the opponent piece killed is the king
    jnz blueNotWin
    mov WinFlag,2
    blueNotWin:
    mov cl,byte ptr GameBoard[BX][DI]           ;get colour and type of piece to display its related notification
    mov selectedKilledPiece,cl
    mov colourOfPiece, al
    call displayNotification

    call UpdateKilledCounter

    moveNewBluePiece:

    mov byte ptr animatedPieceBlue, ch      ;move piece name to animatedPiece for reference when drawing animation frames
    mov al,myColour
    mov byte ptr animatedPieceBlue+1, al
    mov dx, X
    mov stepXBlue, dx
    mov dx, Y
    mov stepYBlue, dx
    call convertToBoard

    mov cx, X                                 ;Board X and Y for end of animation
    mov EndXBlue, cx
    mov cx, Y                                 ;Board X and Y for end of animation
    mov EndYBlue, cx

    mov ax,selectedX2
    mov X,AX
    mov StartXBlue, AX                               ;Board X and Y for Start of animation
    mov ax,selectedY2
    mov Y,AX
    mov StartYBlue, AX
    call convertToArray
    call convertToLoc

    mov cx, X                                ;taking absolute difference between array indices as step for animation
    cmp cx, stepXBlue
    ja absoluteXBlue
    mov cx, stepXBlue
    sub cx, X
    mov stepXBlue, cx
    jmp calcStepYBlue
    absoluteXBlue:                                   
    mov cx, X
    sub cx, stepXBlue
    mov stepXBlue, cx

    calcStepYBlue:
    mov ax, stepXBlue
    mov cl, 5
    mul cl
    mov stepXBlue, ax
    mov cx, Y
    cmp cx, stepYBlue                         ;taking absolute difference between array indices as step for animation
    ja absoluteYBlue
    mov cx, stepYBlue
    sub cx, Y
    mov stepYBlue, cx
    jmp DeselectOldBlue
    absoluteYBlue:                                   
    mov cx, Y
    sub cx, stepYBlue
    mov stepYBlue, cx

    DeselectOldBlue:
    mov ax, stepYBlue
    mov cl, 5
    mul cl
    mov stepYBlue, ax

    mov ax,CX2                                  ;create a message for the end position of the piece
    mov selectedX,ax
    mov ax,CY2
    mov selectedY,ax

    mov partflag,1                              ;1 as it is end position
    call createMessage
    mov sendFlag,1

    mov byte ptr GameBoard[BX][DI],0        ;erase piece from its old loc
    mov byte ptr GameBoard[BX][DI]+1,0
    mov selectedX2,200h                     ;remove selected tile from board until user selects another one
    mov selectedY2,200h
    mov selectedBluePiece,0

    call convertToBoard
    call refresh

    call refreshPossibleBlue                ;erase old possible moves and update board
    
    mov ax, EndXBlue                        
    mov X, ax
    mov ax, EndYBlue
    mov Y, ax
    call convertToArray
    call convertToLoc
    mov byte ptr GameBoard[BX][DI], ch      ;move piece name to its new loc in array 
    mov al, myColour
    mov byte ptr GameBoard[BX][DI]+1, al
    call convertToBoard

    endSelectBlue:
    pop di
    pop cx
    pop bx
    pop ax
    RET
SelectBlue ENDP

SelectRed MACRO 
    push ax
    push bx
    push cx
    push dx
    push di
 
    call decryptMessage                     ;decrypt the message to get end X and Y positions of the opponent piece 

    mov ax,startXRed                        ;using the start positions to get colour and type of piece
    mov X,ax
    mov ax,startYRed
    mov Y,ax

    call convertToArray
    call convertToLoc

    mov ch,byte ptr GameBoard[BX][DI]
	mov selectedRedPiece, ch	

    mov ax,EndXRed
    mov X,ax
    mov ax,EndYRed
    mov Y, ax
    call convertToArray
    call convertToLoc
    
    cmp Y,7
    jnz notPromoteRed                           ;check if a pawn reaches end of board and if yes, promote to queen
    cmp selectedRedPiece,'P'
    jnz notPromoteRed
    mov ch, 'Q'
    notPromoteRed:
    cmp selectedRedPiece,'K'                    ;check if the king moved then update its X and Y position
    jnz con1
    mov kingX1,di
    mov kingY1,bx
    con1:

    mov al,bonusX                               ;apply bonus effect 
    cmp al,byte ptr X
    jnz me
    mov al,bonusY
    cmp al,byte ptr Y
    jnz me
    dec opponentTimer
    mov bonusY,8                                ;remove bonus tile from board
    
    me:
    mov al,myColour
    cmp byte ptr GameBoard[BX][DI]+1,al        ;check if a piece is killed
    jnz moveNewRedPiece
    cmp byte ptr GameBoard[BX][DI],'K'          ;if the piece killed is the player's king
    jnz redNotWin
    mov WinFlag,1
    redNotWin:
    mov cl,byte ptr GameBoard[BX][DI]           ;get colour and type of piece to display its related notification
    mov selectedKilledPiece,cl
    mov colourOfPiece,al
    call displayNotification

    call UpdateKilledCounter                    

    ;check if the destination is a selected blue piece to remove its selection
    mov ax,EndXRed
    cmp ax,selectedX2
    jnz moveNewRedPiece
    mov ax,EndYRed
    cmp ax,selectedY2
    jnz moveNewRedPiece

    ;remove my selection because it was killed
    mov selectedBluePiece,0
    mov selectedX2,200h
    mov selectedY2,200h

    moveNewRedPiece:
    mov byte ptr GameBoard[BX][DI], ch      ;move piece name to its new loc in array 
    mov al, oppositeColour
    mov byte ptr GameBoard[BX][DI]+1, al

    mov byte ptr animatedPieceRed, ch      ;move piece name to animatedPiece for reference when drawing animation frames
    mov al,oppositeColour
    mov byte ptr animatedPieceRed+1, al
    mov dx, X
    mov stepXRed, dx
    mov dx, Y
    mov stepYRed, dx
    call convertToBoard

    mov ax,startXRed
    mov X,AX                              ;Board X and Y for Start of animation
    mov ax,startYRed
    mov Y,AX
    call convertToArray
    call convertToLoc

    mov cx, X                             ;taking absolute difference between array indices as step for animation
    cmp cx, stepXRed
    ja absoluteXRed
    mov cx, stepXRed
    sub cx, X
    mov stepXRed, cx
    jmp calcStepYRed
    absoluteXRed:                                   
    mov cx, X
    sub cx, stepXRed
    mov stepXRed, cx

    calcStepYRed:
    mov ax, stepXRed                        ;multiply step by 5 so reducing frame 45/5 = 9 frames per animation
    mov cl, 5
    mul cl
    
    mov stepXRed, ax
    mov cx, Y
    cmp cx, stepYRed                       ;taking absolute difference between array indices as step for animation
    ja absoluteYRed
    mov cx, stepYRed
    sub cx, Y
    mov stepYRed, cx
    jmp DeselectOldRed
    absoluteYRed:                                   
    mov cx, Y
    sub cx, stepYRed
    mov stepYRed, cx

    DeselectOldRed:
    mov ax, stepYRed                        ;multiply step by 5 so reducing frame 45/5 = 9 frames per animation
    mov cl, 5
    mul cl
    mov stepYRed, ax

    mov byte ptr GameBoard[BX][DI],0            ;erase piece from its old loc
    mov byte ptr GameBoard[BX][DI]+1,0

    mov selectedRedPiece,0
    call convertToBoard
    call refresh

    call refreshPossibleBlue                ;update possible moves

    endSelectRed:
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
ENDM

AnimateRed PROC FAR
    push ax
    push bx
    push cx
    push dx
    

    ;check if start reached end which means that there is no current animation
    mov ax, StartXRed
    cmp ax, EndXRed
    jnz  AnimateNowRed

    mov ax, StartYRed
    cmp ax, EndYRed
    jnz AnimateNowRed
    
    jmp noAnimationRed
    AnimateNowRed:
    
    ;erase old frame

    ;20 -> Cell1 -> Cell2
    ;In the next four blocks, the four corners of the last animated frame are rounded down by X and Y to refresh all tiles that were overlapped by
    ;the animated frame

    ;top left corner
    mov dl,45
    mov ax,startXRed
    sub ax, 20d
    div dl
    cmp ah,0
    mul dl
    add ax, 20
    mov X,ax
    mov oldXRed, ax

    mov dl,45
    mov ax,startYRed
    sub ax, 20d 
    div dl
    mov ah,0
    mul dl
    add ax, 20
    mov Y,ax
    mov oldYRed, ax
    call refresh

    ;top right corner
    mov dl,45
    mov ax,startXRed
    add ax,44                 
    sub ax, 20d
    div dl
    mov ah,0
    mul dl
    add ax, 20
    mov X,ax
    cmp oldXRed, ax
    je BottomleftRed

    mov dl,45
    mov ax,startYRed
    sub ax, 20d
    div dl
    mov ah,0
    mul dl
    add ax, 20
    mov Y,ax  
    call refresh

    BottomLeftRed:
    ;bottom left corner
    mov dl, 45d
    mov ax, startXRed
    sub ax, 20d
    div dl
    mov ah,0
    mul dl
    add ax, 20
    mov X,ax

    mov dl,45d
    mov ax,startYRed
    add ax,44
    sub ax, 20d
    div dl
    mov ah,0
    mul dl
    add ax, 20
    mov Y,ax
    cmp oldYRed, ax
    je BottomRightRed
    call refresh

    BottomRightRed:
    ;bottom right corner
    mov dl,45
    mov ax,startXRed
    add ax,44
    sub ax, 20d
    div dl
    mov ah,0
    mul dl
    add ax, 20
    mov X,ax
    cmp oldXRed, ax
    je ErasedOldRed

    mov dl,45
    mov ax,startYRed
    add ax,44
    sub ax, 20d
    div dl
    mov ah,0
    mul dl
    add ax, 20
    mov Y,ax
    cmp oldYRed, ax
    je ErasedOldRed
    call refresh

    ErasedOldRed:
    ;animate x by updating x with step
    mov ax, StartXRed
    cmp ax, EndXRed
    je checkyRed
    cmp ax, EndXRed
    ja animateleftRed
        mov bx, StartXRed
        add bx, stepXRed
        mov startXRed, bx
        jmp checkyRed
    animateleftRed:
        mov bx, startXRed
        sub bx, stepXRed
        mov startXRed, bx


    ;animate y by updating y with step
    checkyRed:
    mov ax, StartYRed
    cmp ax, EndYRed
    je animatepieceRed
    cmp ax, EndYRed
    ja animateUpRed
        mov bx, StartYRed
        add bx, stepYRed
        mov startYRed, bx
        jmp animatepieceRed
    animateUpRed:
        mov bx, StartYRed
        sub bx, stepYRed
        mov startYRed, bx

    animatepieceRed:

    ;draw piece at current animated frame position
    mov ax, StartXRed         
    mov X, ax
    mov ax, StartYRed
    mov Y, ax

    ;draw piece only
    mov animateFlag, 1
    call refresh
    mov animateFlag, 0
    
    cmp ax, EndYRed
    jne continueAnimationRed
    
    mov ax, StartXRed
    cmp ax, EndXRed
    jne continueAnimationRed
    
    call convertToArray
    call convertToLoc
    mov ch, byte ptr animatedPieceRed
    mov byte ptr GameBoard[BX][DI], ch      ;move piece name to its new loc in array overwriting killed piece and redrawing the tile
    mov ch, oppositeColour
    mov byte ptr animatedPieceRed+1, ch
    mov byte ptr GameBoard[BX][DI]+1, ch
    call convertToBoard

    ;push X
    ;push Y
    ;call refreshPossibleBlue
    ;pop Y
    ;pop X

    cmp WinFlag,1                           ;if the king has been killed then there is no need to set the timer
    jz notimer  
    mov al, opponentTimer
    mov byte ptr Timer[BX][DI], al
    notimer:

    call refresh

    cmp WinFlag,0
    jnz continueAnimationRed
    mov ax,kingX1                               ;check if black king is in check
    mov kingX,ax
    mov ax,kingY1
    mov kingY,ax
    mov al,oppositeColour
    mov colourOfPiece,al
    call check

    mov ax,kingX2                               ;check if white king is in check
    mov kingX,ax
    mov ax,kingY2
    mov kingY,ax
    mov al,myColour
    mov colourOfPiece,al
    call check

    noAnimationRed:
    mov animatedPieceRed, 0
    continueAnimationRed:

    pop dx
    pop cx
    pop bx
    pop ax
    ret
AnimateRed ENDP

AnimateBlue PROC FAR
    push ax
    push bx
    mov ax, StartXBlue
    cmp ax, EndXBlue
    jnz  AnimateNowBlue
    mov ax, StartYBlue
    cmp ax, EndYBlue
    jnz AnimateNowBlue

    jmp noAnimationBlue
    AnimateNowBlue:

    ;erase old frame
    ; 20   -> Cell1 -> Cell2
    ;In the next four blocks, the four corners of the last animated frame are rounded down by X and Y to refresh all tiles that were overlapped by
    ;the animated frame

    ;top left corner
    mov dl,45
    mov ax,startXBlue
    sub ax, 20d
    div dl
    cmp ah,0
    mul dl
    add ax, 20
    mov X,ax
    mov oldXBlue, ax

    mov dl,45
    mov ax,startYBlue
    sub ax, 20d 
    div dl
    mov ah,0
    mul dl
    add ax, 20
    mov Y,ax
    mov oldYBlue, ax
    call refresh

    ;top right corner
    mov dl,45
    mov ax,startXBlue
    add ax,44                 
    sub ax, 20d
    div dl
    mov ah,0
    mul dl
    add ax, 20
    mov X,ax
    cmp oldXBlue, ax
    je BottomleftBlue

    mov dl,45
    mov ax,startYBlue
    sub ax, 20d
    div dl
    mov ah,0
    mul dl
    add ax, 20
    mov Y,ax
    call refresh

    BottomLeftBlue:
    ;bottom left corner
    mov dl, 45d
    mov ax, startXBlue
    sub ax, 20d
    div dl
    mov ah,0
    mul dl
    add ax, 20
    mov X,ax

    mov dl,45d
    mov ax,startYBlue
    add ax,44
    sub ax, 20d
    div dl
    mov ah,0
    mul dl
    add ax, 20
    mov Y,ax
    cmp oldYBlue, ax
    je BottomRightBlue
    call refresh

    BottomRightBlue:
    mov dl,45
    mov ax,startXBlue
    add ax,44
    sub ax, 20d
    div dl
    mov ah,0
    mul dl
    add ax, 20
    mov X,ax
    cmp oldXBlue, ax
    je ErasedOldBlue

    mov dl,45
    mov ax,startYBlue
    add ax,44
    sub ax, 20d
    div dl
    mov ah,0
    mul dl
    add ax, 20
    mov Y,ax
    cmp oldYBlue, ax
    je ErasedOldBlue
    call refresh

    ErasedOldBlue:
    
    ;animate x by updating X with step
    mov ax, StartXBlue
    cmp ax, EndXBlue
    je checkyBlue
    cmp ax, EndXBlue
    ja animateleftBlue
        mov bx, StartXBlue
        add bx, stepXBlue
        mov startXBlue, bx
        jmp checkyBlue
    animateleftBlue:
        mov bx, startXBlue
        sub bx, stepXBlue
        mov startXBlue, bx


    ;animate y by updating y with step
    checkyBlue:
    mov ax, StartYBlue
    cmp ax, EndYBlue
    je animatepieceBlue
    cmp ax, EndYBlue
    ja animateUpBlue
        mov bx, StartYBlue
        add bx, stepYBlue
        mov startYBlue, bx
        jmp animatepieceBlue
    animateUpBlue:
        mov bx, StartYBlue
        sub bx, stepYBlue
        mov startYBlue, bx

    animatepieceBlue:

    ;draw piece at current animated frame position
    mov ax, StartXBlue        
    mov X, ax
    mov ax, StartYBlue
    mov Y, ax

    ;draw Piece only
    mov animateFlag, 2
    call refresh
    mov animateFlag, 0

    cmp ax, EndYBlue
    jne continueAnimationBlue
    mov ax, StartXBlue
    cmp ax, EndXBlue
    jne continueAnimationBlue
    call convertToArray
    call convertToLoc
    mov ch, byte ptr animatedPieceBlue
    mov byte ptr GameBoard[BX][DI], ch      ;move piece name to its new loc in array overwriting killed piece and redrawing the tile
    mov ch, myColour
    mov byte ptr animatedPieceBlue+1, ch
    mov byte ptr GameBoard[BX][DI]+1, ch
    
    cmp WinFlag,2                           ;if the king has been killed then there is no need to set the timer
    jz notimer1
    mov al, myTimer
    mov byte ptr Timer[BX][DI], al
    notimer1:
    call convertToBoard
    call refresh

    cmp WinFlag,0
    jnz continueAnimationBlue
    
    mov ax,kingX1                               ;check if black king is in check
    mov kingX,ax
    mov ax,kingY1
    mov kingY,ax
    mov al,oppositeColour
    mov colourOfPiece,al
    call check

    mov ax,kingX2                               ;check if white king is in check
    mov kingX,ax
    mov ax,kingY2
    mov kingY,ax
    mov al,myColour
    mov colourOfPiece,al
    call check

    noAnimationBlue:
    mov animatedPieceBlue, 0
    continueAnimationBlue:
    pop bx
    pop ax
    ret
AnimateBlue ENDP

Bonus PROC FAR
    pusha
    push X
    push Y
     
    cmp bonusDone, 1                            ;check if bonus is already generated then do not generate any more
    JE noBonus                          
    cmp playerSent, 0                           ;we make sure that only one player generates the bonus                 
    jne noBonus

    ;check if game time reached the time that was generated at the start of the game to create the bonus
    mov al, bonusMinute                         
    cmp Minutes, al
    ja createBonus
    jb noBonus
    mov al, bonusSecond
    cmp seconds, al
    jae createBonus
    jmp noBonus

    createBonus:
    mov ah,2ch                                  ;y position of the bonus is randomly selected by taking mod 8 of the seconds
    int 21h
    mov al,dh
    mov ah,0
    mov cl,8
    div cl
    mov al, ah
    mov ah, 0
    mov Y,ax            
    mov al, bonusX
    mov X, ax
    call convertToLoc
    cmp byte ptr GameBoard[BX][DI],0            ;check that this random position does not contain a piece as the bonus is drawn in an empty tile
    jnz noBonus
    mov Message,0
    mov ax,Y
    mov bonusY,al

    add al,8                                    ;add 8 on Y position to differentiate it between the X and Y positions of bonus
    mov Message,al
    mov sendFlag,1
    mov bonusDone,1                             ;so that no more bonus is generated
    call convertToBoard
    call refresh    

    noBonus:
    pop Y
    pop X
    popa
    ret
Bonus ENDP

Game PROC FAR

    call set
    ;start the timer
    mov ah,2ch                  
    int 21h
    mov OldTime,dh
    mov bonusSecond,dh

    mov al,cl
    mov ah,0
    mov cl,1
    div cl
    mov bonusMinute,ah

    cmp PlayerSent,0
    jne setVideo
    mov al,OldTime
    mov ah,0
    mov cl,8
    div cl
    mov bonusX, ah
    mov Message,0
    mov Message,ah
    mov sendflag,1

    setVideo:
    ;set to graphics mode (640 x 480)
    mov ax,4f02h
    mov bx,0101h
    int 10h

    ;print the time
    mov dh, 0
    mov dl, 62
    mov bh,00h
    mov ah,2
    int 10h
    call printTime

    ; set board filename and draw the board
    mov BH, byte ptr Board
    mov BL, byte ptr Board+1
    mov word ptr Filename, BX

    call DrawBoard

    mov BH, byte ptr Board+2
    mov BL, byte ptr Board+3
    mov word ptr Filename, BX
    mov X,200
    mov Y,0
    call DrawBoard

    mov BH, byte ptr Board+4
    mov BL, byte ptr Board+5
    mov word ptr Filename, BX
    mov X,0
    mov Y,200
    call DrawBoard
    
    mov BH, byte ptr Board+6
    mov BL, byte ptr Board+7
    mov word ptr Filename, BX
    mov X,200
    mov Y,200
    call DrawBoard

    ;draw a line for the status bar
    mov dh,28
    mov dl,0
    mov bh,0
    mov ah,2
    int 10h
    lea dx,StatusLine
    mov ah,9
    int 21h

    ;Draw Pieces
    MOV X, 20
    MOV Y, 20
    mov di, offset GameBoard
DrawStart:
    cmp bx, 0
jz skip
    call refresh
skip:
    add X, TileDim
    add di, 2
    cmp X, 380d         ;check if we reached end of board
    JNE DrawStart
    mov X, 20
    add Y, TileDim
    cmp Y, 380d
jnz DrawStart

;Draw Killed Pieces
    MOV X, 0
    MOV Y, 405
    mov di,0
    mov bl,5
    mov si,0
DrawKilled:
    mov ch, byte ptr KilledBoard[di]
    mov cl, byte ptr KilledBoard[di]+1
    mov word ptr Filename, CX
    call drawTile

    GoToXY bl,26

    mov ah,9
    lea dx,numberKilled
    int 21h

    mov dl,byte ptr KilledCounter[si]

    DisplayChar dl
    
    add bl,8
    add X, 64
    add di, 2
    cmp di,20
jl DrawKilled

    call InitializeInlineChat

    ;Game Loop
    game_loop:
    ;update time and check bonus generation
    call time
    call Bonus

    mov  ah, 01h           
    int  16h                
    jz   done               ;No key waiting aka buffer is empty
    mov  ah, 00h            
    int  16h                

    cmp ah, 3EH             ;Pressed F4 to end the game and go back to the main menu
    jne not_pressedF4
    mov Message,30d
    mov sendFlag,1
    jmp done
    not_pressedF4:

    cmp ah, 4dh             ;pressed right arrow key for cursor 2
    jnz notRight2
    call moveCursor2Right
    jmp done
    notRight2:
    
    cmp ah, 50h             ;pressed down arrow key for cursor 2    
    jnz notDown2
    call moveCursor2Down
    jmp done
    notDown2:

    cmp ah, 48h             ;pressed up arrow key for cursor 2
    jnz notUp2
    call moveCursor2Up
    jmp done
    notUp2:

    cmp ah, 4bh             ;pressed left arrow key for cursor 2
    jnz notLeft2
    call moveCursor2Left
    jmp done
    notLeft2:

    cmp al,9                ;pressed tab key to try selecting or moving a piece
    jnz notSelectedBlue
    call SelectBlue
    jmp done
    notSelectedBlue:

    ;Check if enter is pressed
	CMP al, 0DH             ;Since the values from 0 to 15 are used to transmit data regarding the bonus
	JNE not_enter           ;and the lowest printable character's ascii is ' ' which is 32
	add al, 15d             ;We added 15 to the asciis of enter and backspace to distinguish them from bonus
                            ;and printable characters
    not_enter:
	;Check if backspace is pressed
	CMP al, 08H
	JNE not_backspace
    add al, 15d

    not_backspace:
    mov Message, al   
    inline_Player1 al       ;This macro handles the sender's section of inline chat and handles whether
    ;mov sendFlag, 1        ;the character is printable or not to transmit it to the receiver

    done:

    cmp sendFlag,1
    jnz receive
    mov dx , 3FDH		; Line Status Register
    In al , dx 			;Read Line Status
    AND al , 00100000b
    JZ receive

    mov dx , 3F8H		; Transmit data register
    mov al,Message
	out dx , al
    mov sendFlag, 0
    cmp Message, 30d
    JE pressedF4

    receive:
    mov dx , 3FDH		; Line Status Register	
    in al , dx 
    AND al , 1
    JZ done1
	  ;If Ready read the VALUE in Receive data register
    mov dx , 03F8H
    in ax , dx
    mov dx,0040h
    and dx,ax
    jz getStart 
    mov dx, 80H
    and dx, ax
    jz getStart
    SelectRed
    jmp done1
    getStart:
    CMP ax, 30d
    JE pressedF4
    call decryptMessage
    
    done1:
    ;animate every 1000 game loop cycle adding delay for animation
    inc gamecounter
    cmp gamecounter, 1000
    jne skipAnimation
    mov gamecounter, 0
    call AnimateRed
    call AnimateBlue
    skipAnimation:
    cmp WinFlag, 0
    jz game_loop

    mov dh, 29
    mov dl, 0
    mov bh,00h
    mov ah,2
    int 10h

    lea dx, lineClear
    mov ah,9
    int 21h

    mov dh, 29
    mov dl, 0
    mov bh,00h
    mov ah,2
    int 10h

    cmp WinFlag,2                   ;Win flag 2 means you won (Player 2 is you)
    jnz player2Wins
    lea dx,name1
    mov ah,9
    int 21h
    jmp win
    player2Wins:
    lea dx,name2
    mov ah,9
    int 21h
    jmp win
    win:
    lea dx, won
    mov ah,9
    int 21h

    mov dh, 29
    mov dl, 75
    mov bh,00h
    mov ah,2
    int 10h

    call printTime

    mov two_seconds, 0
    ;get system time
    repeatFinal:
    mov ah,2ch
    int 21h           ;if oldtime is the same as before then do not do anything
    call animateBlue
    call animateRed
    cmp dh,OldTime 
    je repeatFinal 
    inc two_seconds
    mov OldTime, dh
    cmp two_seconds, 2
    je pressedF4
    jmp repeatFinal

    pressedF4:
    
    ret
Game ENDP
END