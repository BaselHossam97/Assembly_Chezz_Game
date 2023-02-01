EXTRN convertToArray:FAR
EXTRN convertToBoard:FAR
EXTRN convertToLoc:FAR
EXTRN PossibleMoves:WORD
EXTRN GameBoard:WORD
EXTRN X:WORD
EXTRN Y:WORD
EXTRN selectedX:WORD
EXTRN selectedY:WORD
EXTRN colourOfPiece:BYTE
EXTRN kingCheck:BYTE
EXTRN kingX:WORD
EXTRN kingY:WORD
EXTRN printTime:FAR
EXTRN lineClear:BYTE
EXTRN name1:BYTE
EXTRN name2:BYTE
EXTRN name1Size:BYTE
EXTRN name2Size:BYTE
EXTRN notification:BYTE
EXTRN oppositeColour:BYTE
EXTRN playerSent:BYTE

PUBLIC redPawn,rook,bishop,king,horse,Pawn,check
.Model Small
.386
.Stack 64
.Data
inY EQU 16d
inX EQU 2d

.code

RedPawn PROC FAR
    push ax
    push cx
    push bx
    push di

    mov ax,selectedX
    mov X,ax
    mov ax,selectedY
    mov Y,ax
    call convertToArray

    inc Y
    mov cx,0
    mov ax,Y
    mov cl,16d                      ;we multiplied by 16 because the array is actually made up of words so we actually have 8 rows and each column consists of two bytes hence one word
    mul cl
    mov bx,ax

    mov ax,X
    mov cl,2                        ;we multiplied by 2 because each column is one word 
    mul cl
    mov di,ax

    cmp GameBoard[BX][DI],0         ;check if pawn can move one step forward
    jnz checkDiagonal1
    mov byte ptr PossibleMoves[BX][DI],1     ;mark the position in the PossibleMoves array as a possible move

    cmp Y,2                         ;if pawn in its starting position, check if it can move two steps
    jnz checkDiagonal1
    cmp GameBoard[BX][DI]+16,0      ;we added 16 because bx currently is one step in front of the pawn so adding 16 moves it one more extra step
    jnz checkDiagonal1
    mov byte ptr PossibleMoves[BX][DI]+16,1

    checkDiagonal1:
    add di,2                        ;check diagonal right (added 2 because we multiplied di by 2 because the GamedBoard array is in words)
    cmp di,16d                      ;check if position is out of the board (array)
    jz checkDiagonal2
    mov al,oppositeColour
    cmp byte ptr GameBoard[BX][DI]+1,al    ;if there is a white piece on the diagonal then it is a possible move
    jnz checkDiagonal2
    mov byte ptr PossibleMoves[BX][DI],1
    checkDiagonal2:

    sub di,4                        ;move two steps to the left
    cmp di,0                        ;end of the array to the left
    jl endRedPawn 
    mov al,oppositeColour
    cmp byte ptr GameBoard[BX][DI]+1,al
    jnz endRedPawn
    mov byte ptr PossibleMoves[BX][DI],1

    endRedPawn:
    call convertToBoard

    pop di
    pop bx
    pop cx
    pop ax
    RET
RedPawn ENDP

Pawn PROC FAR
    push ax
    push cx
    push bx
    push di

    mov ax,selectedX
    mov X,ax
    mov ax,selectedY
    mov Y,ax
    call convertToArray

    dec Y
    mov cx,0
    mov ax,Y
    mov cl,16d                      ;we multiplied by 16 because the array is actually made up of words so we actually have 8 rows and each column consists of two bytes hence one word
    mul cl
    mov bx,ax

    mov ax,X
    mov cl,2                        ;we multiplied by 2 because each column is one word 
    mul cl
    mov di,ax

    cmp GameBoard[BX][DI],0         ;check if pawn can move one step forward
    jnz checkDiagonal3
    mov byte ptr PossibleMoves[BX][DI],1     ;mark the position in the PossibleMoves array as a possible move

    cmp Y,5                         ;if pawn in its starting position, check if it can move two steps
    jnz checkDiagonal3
    cmp GameBoard[BX][DI]-16,0      ;we added 16 because bx currently is one step in front of the pawn so adding 16 moves it one more extra step
    jnz checkDiagonal3
    mov byte ptr PossibleMoves[BX][DI]-16,1

    checkDiagonal3:
    add di,2                        ;check diagonal right (added 2 because we multiplied di by 2 because the GamedBoard array is in words)
    cmp di,16d                      ;check if position is out of the board (array)
    jz checkDiagonal4
    mov al,oppositeColour
    cmp byte ptr GameBoard[BX][DI]+1,al    ;if there is a white piece on the diagonal then it is a possible move
    jnz checkDiagonal4
    mov byte ptr PossibleMoves[BX][DI],1
    checkDiagonal4:

    sub di,4                        ;move two steps to the left
    cmp di,0                        ;end of the array to the left
    jl endBluePawn 
    mov al,oppositeColour
    cmp byte ptr GameBoard[BX][DI]+1,al
    jnz endBluePawn
    mov byte ptr PossibleMoves[BX][DI],1

    endBluePawn:
    call convertToBoard

    pop di
    pop bx
    pop cx
    pop ax
    RET
Pawn ENDP

rook PROC FAR
    push ax
    push cx
    push bx
    push di
    push dx

    mov ax,selectedX
    mov X,ax
    mov ax,selectedY
    mov Y,ax
    call convertToArray
    ;call convertToLoc               ;Y * (16) in BX and X * (2) in DI

    mov cx,0
    mov ax,Y
    mov cl,16d                      ;we multiplied by 16 because the array is actually made up of words so we actually have 8 rows and each column consists of two bytes hence one word
    mul cl
    mov bx,ax

    mov ax,X
    mov cl,2                        ;we multiplied by 2 because each column is one word 
    mul cl
    mov di,ax

    mov ax,bx                       ;Y * (16) in AX
    mov cx,di                       ;X * (2) in DI
    
    mov dl,colourOfPiece


    upRook:
    sub bx,inY                              ;decrement bx by inY to move up one row in the Gameboard array
    cmp bx,00h                              ;check if reached end of board
    jl startDownRook                        
    cmp byte ptr GameBoard[BX][DI]+1,dl     ;check if there is a piece with the same colour in my path
    jz startDownRook
    mov byte ptr PossibleMoves[BX][DI],1    ;count the tile as a possible move
    cmp byte ptr GameBoard[BX][DI]+1,0      ;check if there is a piece with the opposite colour then my path has ended
    jz upRook
    
    startDownRook:
    mov bx,ax                               ;reset to the original position

    downRook:
    add bx,inY
    cmp bx,112d
    jg startLeftRook
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz startLeftRook
    mov byte ptr PossibleMoves[BX][DI],1
    cmp byte ptr GameBoard[BX][DI]+1,0
    jz downRook

    startLeftRook:
    mov bx,ax

    leftRook:
    sub di,inX                              ;decrement di by inX to move left one column in the Gameboard array
    cmp di,00
    jl startRightRook
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz startRightRook
    mov byte ptr PossibleMoves[BX][DI],1
    cmp byte ptr GameBoard[BX][DI]+1,0
    jz leftRook

    startRightRook:
    mov di,cx

    rightRook:
    add di,inX
    cmp di,16d
    jz endRook
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz endRook
    mov byte ptr PossibleMoves[BX][DI],1
    cmp byte ptr GameBoard[BX][DI]+1,0
    jz rightRook

    endRook:
    call convertToBoard

    pop dx
    pop di
    pop bx
    pop cx
    pop ax
    RET
rook ENDP

bishop PROC FAR
    push ax
    push cx
    push bx
    push di
    push dx

    mov ax,selectedX
    mov X,ax
    mov ax,selectedY
    mov Y,ax
    call convertToArray
    ;call convertToLoc               ;Y * (16) in BX and X * (2) in DI

    mov cx,0
    mov ax,Y
    mov cl,16d                      ;we multiplied by 16 because the array is actually made up of words so we actually have 8 rows and each column consists of two bytes hence one word
    mul cl
    mov bx,ax

    mov ax,X
    mov cl,2                        ;we multiplied by 2 because each column is one word 
    mul cl
    mov di,ax

    mov ax,bx                       ;Y * (16) in AX
    mov cx,di                       ;X * (2) in DI

    mov dl,colourOfPiece

    upLeftBishop:                   
    sub bx,inY                      ;decrement bx by inY to move up one row in the Gameboard array
    cmp bx,00h                      ;check if reached end of board
    jl startUpRightBishop
    sub di,inX                      ;decrement di by inX to move left one column in the Gameboard array
    cmp di,00h                      ;check if reached end of board
    jl startUpRightBishop
    cmp byte ptr GameBoard[BX][DI]+1,dl     ;check if there is a piece with the same colour in my path
    jz startUpRightBishop
    mov byte ptr PossibleMoves[BX][DI],1    ;count the tile as a possible move
    cmp byte ptr GameBoard[BX][DI]+1,0      ;check if there is a piece with the opposite colour then my path has ended
    jz upLeftBishop
    
    startUpRightBishop:
    mov bx,ax
    mov di,cx

    upRightBishop:
    sub bx,inY
    cmp bx,00h
    jl startDownLeftBishop
    add di,inX
    cmp di,16d
    jz startDownLeftBishop
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz startDownLeftBishop
    mov byte ptr PossibleMoves[BX][DI],1
    cmp byte ptr GameBoard[BX][DI]+1,0
    jz upRightBishop
    
    startDownLeftBishop:
    mov bx,ax
    mov di,cx

    downLeftBishop:
    add bx,inY
    cmp bx,112d
    jg startDownRightBishop
    sub di,inX
    cmp di,00h
    jl startDownRightBishop
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz startDownRightBishop
    mov byte ptr PossibleMoves[BX][DI],1
    cmp byte ptr GameBoard[BX][DI]+1,0
    jz downLeftBishop

    startDownRightBishop:
    mov bx,ax
    mov di,cx

    downRightBishop:
    add bx,inY
    cmp bx,112d
    jg endBishop
    add di,inX
    cmp di,16d
    jz endBishop
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz endBishop
    mov byte ptr PossibleMoves[BX][DI],1
    cmp byte ptr GameBoard[BX][DI]+1,0
    jz downRightBishop

    endBishop:
    call convertToBoard
    
    pop dx
    pop di
    pop bx
    pop cx
    pop ax
    RET
bishop ENDP

king PROC FAR
    push ax
    push cx
    push bx
    push di
    push dx

    mov ax,selectedX
    mov X,ax
    mov ax,selectedY
    mov Y,ax
    call convertToArray
    ;call convertToLoc               ;Y * (16) in BX and X * (2) in DI

    mov cx,0
    mov ax,Y
    mov cl,16d                      ;we multiplied by 16 because the array is actually made up of words so we actually have 8 rows and each column consists of two bytes hence one word
    mul cl
    mov bx,ax

    mov ax,X
    mov cl,2                        ;we multiplied by 2 because each column is one word 
    mul cl
    mov di,ax

    mov ax,bx                       ;Y * (16) in AX
    mov cx,di                       ;X * (2) in DI

    mov dl,colourOfPiece

    sub di,inX                      ;move to the left of the king
    cmp di,00h                      ;check if reached end of board
    jl midRight
    cmp byte ptr GameBoard[BX][DI]+1,dl     ;check if there is a piece with the same colour
    jz midRight
    mov byte ptr PossibleMoves[BX][DI],1    ;count the tile as a possible move

    midRight:
    add di,inX * 2                      
    cmp di,16d
    jz startUpKing
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz startUpKing
    mov byte ptr PossibleMoves[BX][DI],1
    
    startUpKing:
    mov di,cx
    sub bx,inY
    cmp bx,00h
    jl startDownKing
    
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz upLeft
    mov byte ptr PossibleMoves[BX][DI],1

    upLeft:
    sub di,inX
    cmp di,00h
    jl upRight
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz upRight
    mov byte ptr PossibleMoves[BX][DI],1

    upRight:
    add di,inX * 2                      
    cmp di,16d
    jz startDownKing
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz startDownKing
    mov byte ptr PossibleMoves[BX][DI],1

    startDownKing:
    mov di,cx
    add bx,inY * 2
    cmp bx,112d
    jg endKing
    
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz downLeft
    mov byte ptr PossibleMoves[BX][DI],1

    downLeft:
    sub di,inX
    cmp di,00h
    jl downRight
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz downRight
    mov byte ptr PossibleMoves[BX][DI],1

    downRight:
    add di,inX * 2                      
    cmp di,16d
    jz endKing
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz endKing
    mov byte ptr PossibleMoves[BX][DI],1

    endKing:
    call convertToBoard
    
    pop dx
    pop di
    pop bx
    pop cx
    pop ax
    RET
king ENDP

check PROC FAR
    push ax
    push cx
    push bx
    push di
    push dx
    push si

    mov di,kingX
    mov bx,kingY

    mov ax,bx                       ;Y * (16) in AX
    mov cx,di                       ;X * (2) in DI

    mov dl,colourOfPiece

    cmp playerSent,0
    jnz receivedPlayer

    cmp dl,'W'                      ;check which colour of king i am checking the condition of check on
    jz kingIsWhite                  

    ;the pawns are a special case that needs to be checked independently
    ;for black king, the pawn need to be checked if it is below the king
    add bx,inY
    cmp bx,112d
    jg checkHorse
    add di,inX
    cmp di,16d
    jz bottomLeftPawn
    cmp byte ptr GameBoard[BX][DI]+1,'W'
    jnz bottomLeftPawn
    cmp byte ptr GameBoard[BX][DI],'P'
    jz kingIsInDanger

    bottomLeftPawn:
    sub di,inX*2
    cmp di,00h
    jl checkHorse
    cmp byte ptr GameBoard[BX][DI]+1,'W'
    jnz checkHorse
    cmp byte ptr GameBoard[BX][DI],'P'
    jz kingIsInDanger
    jmp checkHorse

    ;for white king, the pawn need to be checked if it is above the king
    kingIsWhite:
    sub bx,inY
    cmp bx,00h
    jl checkHorse
    add di,inX
    cmp di,16d
    jz topLeftPawn
    cmp byte ptr GameBoard[BX][DI]+1,'B'
    jnz topLeftPawn
    cmp byte ptr GameBoard[BX][DI],'P'
    jz kingIsInDanger

    topLeftPawn:
    sub di,inX*2
    cmp di,00h
    jl checkHorse
    cmp byte ptr GameBoard[BX][DI]+1,'B'
    jnz checkHorse
    cmp byte ptr GameBoard[BX][DI],'P'
    jz kingIsInDanger
    jmp checkHorse

    receivedPlayer:
    
    cmp dl,'B'                      ;check which colour of king i am checking the condition of check on
    jz kingIsBlack                  

    ;the pawns are a special case that needs to be checked independently
    ;for black king, the pawn need to be checked if it is below the king
    add bx,inY
    cmp bx,112d
    jg checkHorse
    add di,inX
    cmp di,16d
    jz bottomLeftPawn1
    cmp byte ptr GameBoard[BX][DI]+1,'B'
    jnz bottomLeftPawn1
    cmp byte ptr GameBoard[BX][DI],'P'
    jz kingIsInDanger

    bottomLeftPawn1:
    sub di,inX*2
    cmp di,00h
    jl checkHorse
    cmp byte ptr GameBoard[BX][DI]+1,'B'
    jnz checkHorse
    cmp byte ptr GameBoard[BX][DI],'P'
    jz kingIsInDanger
    jmp checkHorse

    ;for white king, the pawn need to be checked if it is above the king
    kingIsBlack:
    sub bx,inY
    cmp bx,00h
    jl checkHorse
    add di,inX
    cmp di,16d
    jz topLeftPawn1
    cmp byte ptr GameBoard[BX][DI]+1,'W'
    jnz topLeftPawn1
    cmp byte ptr GameBoard[BX][DI],'P'
    jz kingIsInDanger

    topLeftPawn1:
    sub di,inX*2
    cmp di,00h
    jl checkHorse
    cmp byte ptr GameBoard[BX][DI]+1,'W'
    jnz checkHorse
    cmp byte ptr GameBoard[BX][DI],'P'
    jz kingIsInDanger
    
    checkHorse:
    mov bx,ax
    mov di,cx

    sub bx,inY * 2
    cmp bx,00h
    jl startDownHorseCheck

    sub di,inX
    cmp di,00h
    jl upRightHorseCheck
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz upRightHorseCheck
    cmp byte ptr GameBoard[BX][DI],'H'
    jz kingIsInDanger

    upRightHorseCheck:
    add di,inX * 2                      
    cmp di,16d
    jz startDownHorseCheck
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz startDownHorseCheck
    cmp byte ptr GameBoard[BX][DI],'H'
    jz kingIsInDanger

    startDownHorseCheck:
    mov di,cx
    mov bx,ax
    add bx,inY * 2
    cmp bx,112d
    jg startLeftHorseCheck

    sub di,inX
    cmp di,00h
    jl downRightHorseCheck
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz downRightHorseCheck
    cmp byte ptr GameBoard[BX][DI],'H'
    jz kingIsInDanger

    downRightHorseCheck:
    add di,inX * 2                      
    cmp di,16d
    jz startLeftHorseCheck
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz startLeftHorseCheck
    cmp byte ptr GameBoard[BX][DI],'H'
    jz kingIsInDanger

    startLeftHorseCheck:
    mov di,cx
    mov bx,ax
    sub di,inX * 2
    cmp di,00h
    jl startRightHorseCheck

    sub bx,inY
    cmp bx,00h
    jl leftUpHorseCheck
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz leftUpHorseCheck
    cmp byte ptr GameBoard[BX][DI],'H'
    jz kingIsInDanger

    leftUpHorseCheck:
    add bx,inY * 2                      
    cmp bx,112d
    jg startRightHorseCheck
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz startRightHorseCheck
    cmp byte ptr GameBoard[BX][DI],'H'
    jz kingIsInDanger

    startRightHorseCheck:
    mov di,cx
    mov bx,ax
    add di,inX * 2
    cmp di,14d
    jg startUpCheck

    sub bx,inY
    cmp bx,00h
    jl RightUpHorseCheck
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz RightUpHorseCheck
    cmp byte ptr GameBoard[BX][DI],'H'
    jz kingIsInDanger

    RightUpHorseCheck:
    add bx,inY * 2                      
    cmp bx,112d
    jg startUpCheck
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz startUpCheck
    cmp byte ptr GameBoard[BX][DI],'H'
    jz kingIsInDanger

    startUpCheck:
    mov bx,ax
    mov di,cx

    upCheck:
    sub bx,inY
    cmp bx,00h
    jl startDownCheck
    cmp byte ptr GameBoard[BX][DI],0
    jz upCheck
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz startDownCheck
    mov si,ax
    sub si,inY
    cmp bx,si
    jnz upRQ
    cmp byte ptr GameBoard[BX][DI],'K'
    jz kingIsInDanger
    upRQ:
    cmp byte ptr GameBoard[BX][DI],'R'
    jz kingIsInDanger
    cmp byte ptr GameBoard[BX][DI],'Q'
    jz kingIsInDanger
    
    startDownCheck:
    mov bx,ax

    downCheck:
    add bx,inY
    cmp bx,112d
    jg startLeftCheck
    cmp byte ptr GameBoard[BX][DI],0
    jz downCheck
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz startLeftCheck
    mov si,ax
    add si,inY
    cmp bx,si
    jnz downRQ
    cmp byte ptr GameBoard[BX][DI],'K'
    jz kingIsInDanger
    downRQ:
    cmp byte ptr GameBoard[BX][DI],'R'
    jz kingIsInDanger
    cmp byte ptr GameBoard[BX][DI],'Q'
    jz kingIsInDanger

    startLeftCheck:
    mov bx,ax

    leftCheck:
    sub di,inX
    cmp di,00
    jl startRightCheck
    cmp byte ptr GameBoard[BX][DI],0
    jz leftCheck
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz startRightCheck
    mov si,cx
    sub si,inX
    cmp di,si
    jnz leftRQ
    cmp byte ptr GameBoard[BX][DI],'K'
    jz kingIsInDanger
    leftRQ:
    cmp byte ptr GameBoard[BX][DI],'R'
    jz kingIsInDanger
    cmp byte ptr GameBoard[BX][DI],'Q'
    jz kingIsInDanger

    startRightCheck:
    mov di,cx

    rightCheck:
    add di,inX
    cmp di,16d
    jz startUpLeftCheck
    cmp byte ptr GameBoard[BX][DI],0
    jz rightCheck
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz startUpLeftCheck
    mov si,cx
    add si,inX
    cmp di,si
    jnz rightRQ
    cmp byte ptr GameBoard[BX][DI],'K'
    jz kingIsInDanger
    rightRQ:
    cmp byte ptr GameBoard[BX][DI],'R'
    jz kingIsInDanger
    cmp byte ptr GameBoard[BX][DI],'Q'
    jz kingIsInDanger

    startUpLeftCheck:
    mov di,cx

    upLeftCheck:
    sub bx,inY
    cmp bx,00h
    jl startUpRightCheck
    sub di,inX
    cmp di,00h
    jl startUpRightCheck
    cmp byte ptr GameBoard[BX][DI],0
    jz upLeftCheck
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz startUpRightCheck
    mov si,ax
    sub si,inY
    cmp bx,si
    jnz upLeftBQ
    mov si,cx
    sub si,inX
    cmp di,si
    jnz upLeftBQ
    cmp byte ptr GameBoard[BX][DI],'K'
    jz kingIsInDanger
    upLeftBQ:
    cmp byte ptr GameBoard[BX][DI],'B'
    jz kingIsInDanger
    cmp byte ptr GameBoard[BX][DI],'Q'
    jz kingIsInDanger
    
    startUpRightCheck:
    mov bx,ax
    mov di,cx

    upRightCheck:
    sub bx,inY
    cmp bx,00h
    jl startDownLeftCheck
    add di,inX
    cmp di,16d
    jz startDownLeftCheck
    cmp byte ptr GameBoard[BX][DI],0
    jz upRightCheck
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz startDownLeftCheck
    mov si,ax
    sub si,inY
    cmp bx,si
    jnz upRightBQ
    mov si,cx
    add si,inX
    cmp di,si
    jnz upRightBQ
    cmp byte ptr GameBoard[BX][DI],'K'
    jz kingIsInDanger
    upRightBQ:
    cmp byte ptr GameBoard[BX][DI],'B'
    jz kingIsInDanger
    cmp byte ptr GameBoard[BX][DI],'Q'
    jz kingIsInDanger
    
    startDownLeftCheck:
    mov bx,ax
    mov di,cx

    downLeftCheck:
    add bx,inY
    cmp bx,112d
    jg startDownRightCheck
    sub di,inX
    cmp di,00h
    jl startDownRightCheck
    cmp byte ptr GameBoard[BX][DI],0
    jz downLeftCheck
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz startDownRightCheck
    mov si,ax
    add si,inY
    cmp bx,si
    jnz downLeftBQ
    mov si,cx
    sub si,inX
    cmp di,si
    jnz downLeftBQ
    cmp byte ptr GameBoard[BX][DI],'K'
    jz kingIsInDanger
    downLeftBQ:
    cmp byte ptr GameBoard[BX][DI],'B'
    jz kingIsInDanger
    cmp byte ptr GameBoard[BX][DI],'Q'
    jz kingIsInDanger

    startDownRightCheck:
    mov bx,ax
    mov di,cx

    downRightCheck:
    add bx,inY
    cmp bx,112d
    jg endCheck
    add di,inX
    cmp di,16d
    jz endCheck
    cmp byte ptr GameBoard[BX][DI],0
    jz downRightCheck
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz endCheck
    mov si,ax
    add si,inY
    cmp bx,si
    jnz downRightBQ
    mov si,cx
    add si,inX
    cmp di,si
    jnz downRightBQ
    cmp byte ptr GameBoard[BX][DI],'K'
    jz kingIsInDanger
    downRightBQ:
    cmp byte ptr GameBoard[BX][DI],'B'
    jz kingIsInDanger
    cmp byte ptr GameBoard[BX][DI],'Q'
    jz kingIsInDanger

    jmp endCheck                            ;clearing the notification bar and creating the notification

    kingIsInDanger:
    mov dh, 29
    mov dl, 0
    mov bh,00h
    mov ah,2
    int 10h

    lea dx, lineClear
    mov ah,9
    int 21h

    mov cx,36
    mov al,'$'
    lea di,notification
    REP STOSB
    
    mov al,oppositeColour
    cmp colourOfPiece,al
    jz player2Check

    mov cx,0
    mov cl,name1Size+1
    lea si,name1
    lea di,notification
    REP MOVSB

    lea di,notification
    mov cx,0
    mov cl,name1Size+1
    add di,cx
    jmp printCheck

    player2Check:
    mov cx,0
    mov cl,name2Size+1
    lea si,name2
    lea di,notification
    REP MOVSB

    lea di,notification
    mov cx,0
    mov cl,name2Size+1
    add di,cx

    printCheck:
    lea si,kingCheck
    mov cx,20
    REP MOVSB

    mov dh, 29
    mov dl, 0
    mov bh,00h
    mov ah,2
    int 10h

    lea dx, notification
    mov ah,9
    int 21h

    mov dh, 29
    mov dl, 75
    mov bh,00h
    mov ah,2
    int 10h

    call printTime
    endCheck:
    pop si
    pop dx
    pop di
    pop bx
    pop cx
    pop ax  
    RET
check ENDP

horse PROC FAR
    push ax
    push cx
    push bx
    push di
    push dx

    mov ax,selectedX
    mov X,ax
    mov ax,selectedY
    mov Y,ax
    call convertToArray
    ;call convertToLoc               ;Y * (16) in BX and X * (2) in DI

    mov cx,0
    mov ax,Y
    mov cl,16d                      ;we multiplied by 16 because the array is actually made up of words so we actually have 8 rows and each column consists of two bytes hence one word
    mul cl
    mov bx,ax

    mov ax,X
    mov cl,2                        ;we multiplied by 2 because each column is one word 
    mul cl
    mov di,ax

    mov ax,bx                       ;Y * (16) in AX
    mov cx,di                       ;X * (2) in DI

    mov dl,colourOfPiece

    sub bx,inY * 2
    cmp bx,00h
    jl startDownHorse

    sub di,inX
    cmp di,00h
    jl upRightHorse
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz upRightHorse
    mov byte ptr PossibleMoves[BX][DI],1

    upRightHorse:
    add di,inX * 2                      
    cmp di,16d
    jz startDownHorse
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz startDownHorse
    mov byte ptr PossibleMoves[BX][DI],1

    startDownHorse:
    mov di,cx
    mov bx,ax
    add bx,inY * 2
    cmp bx,112d
    jg startLeftHorse

    sub di,inX
    cmp di,00h
    jl downRightHorse
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz downRightHorse
    mov byte ptr PossibleMoves[BX][DI],1

    downRightHorse:
    add di,inX * 2                      
    cmp di,16d
    jz startLeftHorse
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz startLeftHorse
    mov byte ptr PossibleMoves[BX][DI],1

    startLeftHorse:
    mov di,cx
    mov bx,ax
    sub di,inX * 2
    cmp di,00h
    jl startRightHorse

    sub bx,inY
    cmp bx,00h
    jl leftUpHorse
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz leftUpHorse
    mov byte ptr PossibleMoves[BX][DI],1

    leftUpHorse:
    add bx,inY * 2                      
    cmp bx,112d
    jg startRightHorse
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz startRightHorse
    mov byte ptr PossibleMoves[BX][DI],1

    startRightHorse:
    mov di,cx
    mov bx,ax
    add di,inX * 2
    cmp di,14d
    jg endHorse

    sub bx,inY
    cmp bx,00h
    jl RightUpHorse
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz RightUpHorse
    mov byte ptr PossibleMoves[BX][DI],1

    RightUpHorse:
    add bx,inY * 2                      
    cmp bx,112d
    jg endHorse
    cmp byte ptr GameBoard[BX][DI]+1,dl
    jz endHorse
    mov byte ptr PossibleMoves[BX][DI],1

    endHorse:
    call convertToBoard
    
    pop dx
    pop di
    pop bx
    pop cx
    pop ax
    RET
horse ENDP
END