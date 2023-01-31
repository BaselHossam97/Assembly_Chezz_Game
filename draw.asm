    EXTRN Filename:BYTE
    EXTRN X:WORD
    EXTRN Y:WORD
    EXTRN ignoreFlag:BYTE
    PUBLIC DrawBoard, DrawTile, DrawTimer
.Model Small
.Stack 64
.Data

TileDim EQU 45
BoardDim EQU 200
TimerDim EQU 25
Filehandle DW ?
TileData DB TileDim*TileDim dup(0)
BoardData DB BoardDim*BoardDim dup(0)
TimerData DB TimerDim*TimerDim dup(0)

.Code

OpenFile PROC FAR    
    push ax
    push dx

    MOV AH, 3Dh
    MOV AL, 0 
    LEA DX, Filename
    INT 21h
    
    MOV [Filehandle], AX
    
    pop dx
    pop ax
    RET
OpenFile ENDP

ReadBoardData PROC FAR
    push ax
    push bx
    push cx
    push dx
    
    MOV AH,3Fh
    MOV BX, [Filehandle]
    MOV CX,BoardDim*BoardDim 
    LEA DX, BoardData
    INT 21h

    pop dx
    pop cx
    pop bx
    pop ax
    RET
ReadBoardData ENDP 

ReadTileData PROC FAR
    push ax
    push bx
    push cx
    push dx
    
    MOV AH,3Fh
    MOV BX, [Filehandle]
    MOV CX,TileDim*TileDim
    LEA DX, TileData
    INT 21h

    pop dX
    pop cx
    pop bx
    pop ax
    RET
ReadTileData ENDP

ReadTimerData PROC FAR
    push ax
    push bx
    push cx
    push dx
    
    MOV AH,3Fh
    MOV BX, [Filehandle]
    MOV CX,TimerDim*TimerDim
    LEA DX, TimerData
    INT 21h

    pop dX
    pop cx
    pop bx
    pop ax
    RET
ReadTimerData ENDP

CloseFile PROC FAR
    push ax
    push bx
    
	MOV AH, 3Eh
	MOV BX, [Filehandle]
	INT 21h

    pop bx
    pop ax
	RET
CloseFile ENDP

DrawBoard PROC FAR  
    push ax
    push bx
    push cx
    push dx
    push di
    push si
    
    CALL OpenFile
    CALL ReadBoardData
	
    LEA BX , BoardData ; BL contains index at the current drawn pixel
	
    MOV CX, X ;Column
    MOV DX, Y ;Row
    MOV AH,0ch
	MOV SI, X
    ADD SI, BoardDim
    MOV DI, Y
    ADD DI, BoardDim
	
; Drawing loop
drawBoardLoop:
    MOV AL,[BX]
    INT 10h 
    INC CX
    INC BX
    CMP CX, SI
    JNE drawBoardLoop 
	
    MOV CX , X
    INC DX
    CMP DX, DI
    JNE drawBoardLoop
    
    call CloseFile

    pop si
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    RET
DrawBoard ENDP

DrawTile PROC FAR  

    push ax
    push bx
    push cx
    push dx
    push di
    push si

    CALL OpenFile
    CALL ReadTileData
	
    LEA BX , TileData ; BL contains index at the current drawn pixel
	
    MOV CX, X ;Column
    MOV DX, Y ;Row
    MOV AH,0ch
	MOV SI, X
    ADD SI, TileDim
    MOV DI, Y
    ADD DI, TileDim
	
; Drawing loop
drawTileLoop:

    MOV AL,[BX]
    cmp ignoreFlag,1
    jz ignore
    CMP AL, 0
    jz skipDrawing
    ignore:
    INT 10h
    skipDrawing:
    INC CX
    INC BX
    CMP CX, SI
    JNE drawTileLoop 
	
    MOV CX , X
    INC DX
    CMP DX, DI
    JNE drawTileLoop
    
    call CloseFile
    mov ignoreFlag,0

    pop si
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    RET
DrawTile ENDP

DrawTimer PROC FAR  
    push ax
    push bx
    push cx
    push dx
    push di
    push si

    CALL OpenFile
    CALL ReadTimerData
	
    LEA BX , TimerData ; BL contains index at the current drawn pixel
	
    MOV CX, X ;Column
    MOV DX, Y ;Row
    MOV AH,0ch
	MOV SI, X
    ADD SI, TimerDim
    MOV DI, Y
    ADD DI, TimerDim
	
; Drawing loop
drawTimerLoop:

    MOV AL,[BX]
    CMP AL, 0
    jz skipDrawing1
    INT 10h
    skipDrawing1:
    INC CX
    INC BX
    CMP CX, SI
    JNE drawTileLoop 
	
    MOV CX , X
    INC DX
    CMP DX, DI
    JNE drawTimerLoop
    
    call CloseFile

    pop si
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    RET
DrawTimer ENDP
END