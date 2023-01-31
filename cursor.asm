EXTRN CX1:word
EXTRN CY1:word
EXTRN CX2:word
EXTRN CY2:word
EXTRN X:word
EXTRN Y:word
EXTRN Filename:Byte
EXTRN Cursor1:word
EXTRN Cursor2:word
EXTRN refresh:FAR
EXTRN DrawTile:FAR

PUBLIC moveCursor1Right, moveCursor1Down, moveCursor1Left, moveCursor1Up, moveCursor2Right, moveCursor2Down, moveCursor2Left, moveCursor2Up

.Model Small
.Stack 64
.code
moveCursor1Right PROC FAR
    push ax
    push bx

    cmp CX1, 335d       ;max pixel of the start of a tile
    jz maxRight         ;if no more tiles to move, do not move any further
    mov bx,CX1          ;place cursor position in x and y variables to refresh the current tile selected
    mov X,bx
    mov bx,CY1
    mov Y,bx
    ADD CX1, 45         ;move the cursor by 1 tile to the right
    call refresh        ;to refresh the tile and place the piece if exists
    mov BH, byte ptr Cursor1    ;place the file name in bx
    mov BL, byte ptr Cursor1+1
    mov word ptr Filename, BX   ;place the file name in Filename
    mov ax, CX1         ;place new cursor coordinates in x and y
    mov X, ax
    mov ax, CY1
    mov Y, ax
    call DrawTile       ;draw the new tile with the cursor frame on it
    maxRight: pop bx
    pop ax
    RET      
moveCursor1Right ENDP

moveCursor1Down PROC FAR
    push ax
    push bx

    cmp CY1, 335d       
    jz maxDown          
    mov bx,CX1          
    mov X,bx
    mov bx,CY1
    mov Y,bx        
    add CY1, 45
    call refresh         
    mov BH, byte ptr Cursor1
    mov BL, byte ptr Cursor1+1
    mov word ptr Filename, BX
    mov ax, CX1
    mov X, ax
    mov ax, CY1
    mov Y, ax
    call DrawTile
    maxDown: pop bx
    pop ax
    RET      
moveCursor1Down ENDP

moveCursor1Left PROC FAR
    push ax
    push bx

    cmp CX1, 20d       ;max pixel of the start of a tile
    jz maxLeft
    mov bx,CX1
    mov X,bx
    mov bx,CY1
    mov Y,bx
    SUB CX1, 45
    call refresh
    mov BH, byte ptr Cursor1
    mov BL, byte ptr Cursor1+1
    mov word ptr Filename, BX
    mov ax, CX1
    mov X, ax
    mov ax, CY1
    mov Y, ax
    call DrawTile
    maxLeft: pop bx
    pop ax
    RET      
moveCursor1Left ENDP

moveCursor1Up PROC FAR
    push ax
    push bx

    cmp CY1, 20d       ;max pixel of the start of a tile
    jz maxUp
    mov bx,CX1
    mov X,bx
    mov bx,CY1
    mov Y,bx
    SUB CY1, 45
    call refresh
    mov BH, byte ptr Cursor1
    mov BL, byte ptr Cursor1+1
    mov word ptr Filename, BX
    mov ax, CX1
    mov X, ax
    mov ax, CY1
    mov Y, ax
    call DrawTile
    maxUp: pop bx
    pop ax
    RET      
moveCursor1Up ENDP

moveCursor2Right PROC FAR
    push ax
    push bx

    cmp CX2, 335d       ;max pixel of the start of a tile
    jz maxRight2
    mov bx,CX2
    mov X,bx
    mov bx,CY2
    mov Y,bx
    ADD CX2, 45
    call refresh
    mov BH, byte ptr Cursor2
    mov BL, byte ptr Cursor2+1
    mov word ptr Filename, BX
    mov ax, CX2
    mov X, ax
    mov ax, CY2
    mov Y, ax
    call DrawTile
    maxRight2: pop bx
    pop ax
    RET      
moveCursor2Right ENDP

moveCursor2Down PROC FAR
    push ax
    push bx

    cmp CY2, 335d       ;max pixel of the start of a tile
    jz maxDown2
    mov bx,CX2
    mov X,bx
    mov bx,CY2
    mov Y,bx
    add CY2, 45
    call refresh
    mov BH, byte ptr Cursor2
    mov BL, byte ptr Cursor2+1
    mov word ptr Filename, BX
    mov ax, CX2
    mov X, ax
    mov ax, CY2
    mov Y, ax
    call DrawTile
    maxDown2: pop bx
    pop ax
    RET      
moveCursor2Down ENDP

moveCursor2Left PROC FAR
    push ax
    push bx

    cmp CX2, 20d       ;max pixel of the start of a tile
    jz maxLeft2
    mov bx,CX2
    mov X,bx
    mov bx,CY2
    mov Y,bx
    SUB CX2, 45
    call refresh
    mov BH, byte ptr Cursor2
    mov BL, byte ptr Cursor2+1
    mov word ptr Filename, BX
    mov ax, CX2
    mov X, ax
    mov ax, CY2
    mov Y, ax
    call DrawTile
    maxLeft2: pop bx
    pop ax
    RET      
moveCursor2Left ENDP

moveCursor2Up PROC FAR
    push ax
    push bx

    cmp CY2, 20d       ;max pixel of the start of a tile
    jz maxUp2
    mov bx,CX2
    mov X,bx
    mov bx,CY2
    mov Y,bx
    SUB CY2, 45
    call refresh
    mov BH, byte ptr Cursor2
    mov BL, byte ptr Cursor2+1
    mov word ptr Filename, BX
    mov ax, CX2
    mov X, ax
    mov ax, CY2
    mov Y, ax
    call DrawTile
    maxUp2: pop bx
    pop ax
    RET      
moveCursor2Up ENDP

END