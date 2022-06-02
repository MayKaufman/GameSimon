IDEAL
MODEL small

;Bmp's dimensions
BMP_WIDTH = 320 
BMP_HEIGHT = 200

STACK 100h

FILE_NAME_IN  equ 'Open.bmp' ;File's name
FILE_NAME_IN2  equ 'Black1.bmp'
FILE_NAME_IN3  equ 'Instruc.bmp'
FILE_NAME_IN4  equ 'Loser.bmp'
FILE_NAME_IN5  equ 'Winner.bmp'

DATASEG
; --------------------------
; Your variables here
; --------------------------

;Positions for the game board:
Xboard dw 75
Yboard dw 40
HeightBoard dw 50
WidthBoard dw 60

;Sounds 
note dw 139Fh
SoundRed1 	equ 1809      ;Real Frequency Hz #-659.26 ;very low NoteE3
SoundGreen2 equ 7670     ;Real Frequency Hz #-155.56 ;very high NoteD_D1
SoundYellow3 equ 1917     ;Real Frequency Hz #-622.25 ;high NoteD_D3
SoundBlue4 	equ 6833      ;Real Frequency Hz #-174.61 ;low NoteF1

;Random 
RndCurrentPos dw start
Move db ? ;1-red,2-green,3-yellow,4-blue

;Variables
CorrectPlayer db ?
TotalComputerSeqPos db ? 
ThisComputerSeqPos db ?  ;Resets each time in the loop

BiggestCorrect db 12 ;The biggest correct a player can get.

;Compare the rectangles
ColorPlayer db ?
MovePlayer db ?

;Flag
IsOver db 1 ;if IsOver=1: the game continue, if IsOver=0: the game stop

;Colors: (After the changes in the pallete)
RedColor db 1
GreenColor db 16
YellowColor db 3
BlueColor db 4

;Bmp files:
ScrLine 	db BMP_WIDTH dup (0)  ; One Color line read buffer

	;BMP File data
FileOpeningScreen 	db FILE_NAME_IN ,0
FileBlackBackground db FILE_NAME_IN2 ,0
FileInstructions db FILE_NAME_IN3 ,0
FileLoser db FILE_NAME_IN4,0
FileWinner db FILE_NAME_IN5,0

FileHandle	dw ?
Header 	    db 54 dup(0)
Palette 	db 400h dup (0)
	
;BmpFileErrorMsg    	db 'Error At Opening Bmp File ',FILE_NAME_IN, 0dh, 0ah,'$'
ErrorFile           db 0
	
BmpLeft dw ?
BmpTop dw ?
BmpColSize dw ?
BmpRowSize dw ?

;Print points
PointsMessage db "Points:$"
UnitsDigit db ?
TensDigit db ?

;Print Record
RecordMessage db "The Record:$"
TheRecord db "? $"

;Set Cursor
XCursorPosition db ?
YCursorPosition db ?

;Check record
FileRecord db "Record.txt",0
BestRecord db ?
LengthRecordFile dw ?
Char db ?
PointerFilePosition db ? ;The pointer in the file: 0-the beginning,1-current position,2-end of the file

;Exit message 
ExitMessage db " Press any key to exit$"

;The array:
SequenceArray db ?

CODESEG

;An action that sets the graphic.
proc SetGraphic
	push ax
	mov ax,13h    				  
	int 10h
	pop ax
	ret
endp SetGraphic

;An action that draws an horizontal line.
;in si: the length of the line
;in cx: the x position
proc DrawHorizontalLine	near 
	push si
	push cx
DrawLine:
	cmp si,0
	jz ExitDrawLine	
	 
    mov ah,0ch	
	int 10h    ; put pixel
	 	
	inc cx
	dec si
	jmp DrawLine
		
ExitDrawLine:
	pop cx
    pop si
	ret
endp DrawHorizontalLine

;An action that draws a vertical line.
;in si: the length of the line
;in dx: the y position
proc DrawVerticalLine	near
	push si
	push dx
 
DrawVertical:
	cmp si,0
	jz @@ExitDrawLine	
	 
    mov ah,0ch	
	int 10h    ; put pixel
		
	inc dx
	dec si
	jmp DrawVertical
		
@@ExitDrawLine:
	pop dx
    pop si
	ret
endp DrawVerticalLine

;An action that draws a rectangle.
;cx =the number of columns, dx=the number of rows ,al = color, si = height, di = width
proc Rect
	push cx
	push di
NextVerticalLine:	
	
	cmp di,0
	jz @@EndRect
	
	cmp si,0
	jz @@EndRect
	call DrawVerticalLine
	inc cx
	dec di
	jmp NextVerticalLine
	
	
@@EndRect:
	pop di
	pop cx
	ret
endp Rect

;An action that draws the game board. 
proc DrawBoard
	push ax
	push cx
	push si
	push di
	push dx
	
	;Draw the green rectangle:
	mov al,[GreenColor]
	mov cx,[Xboard]
	mov dx,[Yboard]
	mov si,[HeightBoard]
	mov di,[WidthBoard]
	call Rect
	
	;Draw the red rectangle:
	mov al,[RedColor]
	add cx,[WidthBoard]
	add cx,10
	call rect
	
	;Draw the blue rectangle:
	mov al,[BlueColor]
	add dx,[HeightBoard]
	add dx,10
	call rect
	
	;Draw the yellow rectangle:
	mov al,[YellowColor]
	sub cx,[WidthBoard]
	sub cx,10
	call rect
	
	pop dx
	pop di
	pop si
	pop cx
	pop ax
	ret
endp DrawBoard

;An action that does a delay for 1 second.
proc LoopDelay1Sec
	push cx
	
	mov cx ,1000 
@@Self1:
	
	push cx
	mov cx,3000 

@@Self2:	
	loop @@Self2
	
	pop cx
	loop @@Self1
	
	pop cx
	ret
	
endp LoopDelay1Sec

;An action that does a delay for 200 miliseconds.
proc _200MiliSecDelay
	push cx
	
	mov cx ,1000 
@@Self1:
	
	push cx
	mov cx,600 

@@Self2:	
	loop @@Self2
	
	pop cx
	loop @@Self1
	
	pop cx
	ret
endp _200MiliSecDelay

;An action that makes one beep according to the variable note. In note-the frequency.
proc OneBeep
	push ax
			
	mov al,10110110b ; 10110110b the magic number (Prepare the speaker for the note)   
	out 43h,al  ;init port 43h timer 2
	mov ax, [note] ;Frequency value
	
	out 42h,al ;low significant byte to port 42h.
	mov al,ah
	out 42h,al ; send most significant byte to port 42h.
	in al, 61h  ;Get the value of port 61h (Turn on note)
	    
    or al,00000011b ;Or al to this value, forcing first two bits high.
	out 61h,al ;Turn on the speaker
	
	;Delay
	call LoopDelay1Sec
	
	;Turn speaker off	
	in al, 61h ;Get the value of port 61h (turn off note)
	and al,11111100b ;Resets bits 1 and 0
	out 61h,al      ;send new value
	
    pop ax	
    ret
endp OneBeep

; make mask acording to bh size 
; output Si = mask put 1 in all bh range
; example  if bh 4 or 5 or 6 or 7 si will be 7
; 		   if Bh 64 till 127 si will be 127
Proc MakeMask    
    push bx

	mov si,1
    
@@again:
	shr bh,1
	cmp bh,0
	jz @@EndProc
	
	shl si,1 ; add 1 to si at right
	inc si
	
	jmp @@again
	
@@EndProc:
    pop bx
	ret
endp  MakeMask

; Description  : get RND between any bl and bh includs (max 0 -255)
; Input        : 1. Bl = min (from 0) , BH , Max (till 255)
; 			     2. RndCurrentPos a  word variable,   help to get good rnd number
; 				 	Declre it at DATASEG :  RndCurrentPos dw ,0
;				 3. EndOfCsLbl: is label at the end of the program one line above END start		
; Output:        Al - rnd num from bl to bh  (example 50 - 150)
; More Info:
; 	Bl must be less than Bh 
; 	in order to get good random value again and agin the Code segment size should be 
; 	at least the number of times the procedure called at the same second ... 
; 	for example - if you call to this proc 50 times at the same second  - 
; 	Make sure the cs size is 50 bytes or more 
; 	(if not, make it to be more) 
proc RandomByCs
    push es
	push si
	push di
	
	mov ax, 40h
	mov	es, ax
	
	sub bh,bl  ; we will make rnd number between 0 to the delta between bl and bh
			   ; Now bh holds only the delta
	cmp bh,0
	jz @@ExitP
 
	mov di, [word RndCurrentPos]
	call MakeMask ; will put in si the right mask according the delta (bh) (example for 28 will put 31)
	
RandLoop: ;  generate random number 
	mov ax, [es:06ch] ; read timer counter
	mov ah, [byte cs:di] ; read one byte from memory (from semi random byte at cs)
	xor al, ah ; xor memory and counter
	
	; Now inc di in order to get a different number next time
	inc di
	cmp di,(EndOfCsLbl - start - 1)
	jb @@Continue
	mov di, offset start
@@Continue:
	mov [word RndCurrentPos], di
	
	and ax, si ; filter result between 0 and si (the nask)
	cmp al,bh    ;do again if  above the delta
	ja RandLoop
	
	add al,bl  ; add the lower limit to the rnd num
		 
@@ExitP:	
	pop di
	pop si
	pop es
	ret
endp RandomByCs

;An action that gets a random number between 1-4 and save it in the sequence array.
proc RandomMove
	push bx
	push ax
	
	mov bl,1
	mov bh,4
	call RandomByCs
	mov [Move],al
	
	mov bl,[TotalComputerSeqPos]
	mov bh,0
	mov [SequenceArray+bx],al
	
	pop ax
	pop bx
	ret
endp RandomMove

;An action that plays the computer's sequence 
proc PlayComputerSeq
	push cx
	push si
	push ax
	
	mov cl,[CorrectPlayer]
	inc cl
	mov ch,0
	mov si,0
	
TheLoop:
	mov al,[SequenceArray+si]
	mov [Move],al
	call PlayOneRect
	inc si 
loop TheLoop

	pop ax
	pop si
	pop cx
	ret 
endp PlayComputerSeq

;An action that play one rect (blink and sound) accourding to the move.
proc PlayOneRect
	push ax
	push cx
	push si
	push di
	push dx
	
	;The rectangle's dimensions
	mov si,[HeightBoard]
	mov di,[WidthBoard]
	
	cmp [Move],1
	je RedRect
	
	cmp [Move],2
	je GreenRect
	
	cmp [Move],3
	je YellowRect1 ;I can't jump to directly YellowRect because it's out of range.
	
;else-blue rect	
; blink:	
	mov al,0
	mov cx,[Xboard]
	add cx,[WidthBoard]
	add cx,10
	mov dx,[Yboard]
	add dx,[HeightBoard]
	add dx,10
	call Rect
	
	call _200MiliSecDelay
	
	mov al,[BlueColor]
	call Rect
	
	call _200MiliSecDelay

	mov ax,[SoundBlue4]
	mov [note],ax
	call OneBeep
	jmp @@exit
	
YellowRect1:
	jmp YellowRect
	
RedRect:	
;blink:	
	mov al,0
	mov cx,[Xboard]
	add cx,[WidthBoard]
	add cx,10
	mov dx,[Yboard]
	call Rect
		
	call _200MiliSecDelay
	
	mov al,[RedColor]
	call Rect
	
	call _200MiliSecDelay
	
	mov ax,[SoundRed1]
	mov [note],ax
	call OneBeep
	jmp @@exit	
	
GreenRect:	
	; blink2:
	mov al,0
	mov cx,[Xboard]
	mov dx,[Yboard]
	call Rect
		
	call _200MiliSecDelay
		
	mov al,[GreenColor]
	call Rect
	
	call _200MiliSecDelay

	mov ax,[SoundGreen2]
	mov [note],ax
	call OneBeep
	jmp @@exit

YellowRect:	
	;blink:
	mov al,0
	mov cx,[Xboard]
	mov dx,[Yboard]
	add dx,[HeightBoard]
	add dx,10
	call Rect
		
	call _200MiliSecDelay
		
	mov al,[YellowColor]
	call Rect
	
	call _200MiliSecDelay

	mov ax,[SoundYellow3]
	mov [note],ax
	call OneBeep
	
@@exit:
	pop dx
	pop di
	pop si
	pop cx
	pop ax
	ret
endp PlayOneRect

;An action that compares the player's color to the computer's.
proc CompareColorRect
	push bx 
	push ax
	
	mov al,[RedColor]
	cmp [ColorPlayer],al ;The actual color is in the variable ColorPlayer
	je ItsRed
	
	mov al,[GreenColor]
	cmp [ColorPlayer],al
	je ItsGreen
	
	mov al,[YellowColor]
	cmp [ColorPlayer],al
	je ItsYellow
	
	;Else-Its blue
	mov [MovePlayer],4 ;4-blue
	jmp @@continue
	
	ItsRed:
	mov [MovePlayer],1 ;1-red
	jmp @@continue
	
	ItsGreen:
	mov [MovePlayer],2 ;2-green
	jmp @@continue
	
	ItsYellow:
	mov [MovePlayer],3 ;3-yellow
	
	@@continue:
	mov bl,[ThisComputerSeqPos]
	mov bh,0
	
	;Compare the player's move to the computer's.
	mov al,[MovePlayer]
	cmp al,[SequenceArray+bx]
	jne NotOk
	
	inc [ThisComputerSeqPos]
	jmp @@exit
	
NotOk:
	mov [IsOver],0 ;The game is over
	
@@exit:	
	pop ax
	pop bx
	ret
endp CompareColorRect

;An action that shows on the screen a bmp file.
proc OpenShowBmp near	 
	call OpenBmpFile
	cmp [ErrorFile],1
	je @@ExitProc
	
	call ReadBmpHeader
	
	call ReadBmpPalette
	
	call CopyBmpPalette
	
	call  ShowBmp
		 
	call CloseBmpFile

@@ExitProc:
	ret
endp OpenShowBmp

; An action that opens a bmp file :in dx-filename to open
proc OpenBmpFile	near						 
	mov ah, 3Dh
	xor al, al
	int 21h
	jc @@ErrorAtOpen ;If the carry flag is on- there is a problem with opening the file.
	mov [FileHandle], ax
	jmp @@ExitProc
	
@@ErrorAtOpen:
	mov [ErrorFile],1
@@ExitProc:	
	ret
endp OpenBmpFile

;An action that close the file (with the file's handle)
proc CloseBmpFile near
	mov ah,3Eh
	mov bx, [FileHandle]
	int 21h
	ret
endp CloseBmpFile


; Read 54 bytes the Header (To skip the bmp's file intro)
proc ReadBmpHeader	near					
	push cx
	push dx
	
	mov ah,3fh
	mov bx, [FileHandle]
	mov cx,54
	mov dx,offset Header
	int 21h
	
	pop dx
	pop cx
	ret
endp ReadBmpHeader

proc ReadBmpPalette near ; Read BMP file color palette, 256 colors * 4 bytes (400h)
						 ; 4 bytes for each color BGR + null)	
						 ;Reads it into an array named-Palette
	push cx
	push dx
	
	mov ah,3fh
	mov cx,400h
	mov dx,offset Palette
	int 21h
	
	pop dx
	pop cx
	
	ret
endp ReadBmpPalette

;An action that copy the bmp pallete to the video port.(3C8,3C9)
; Will move out to screen memory the colors
; video ports are 3C8h for number of first color
; and 3C9h for all rest

;My explanation:
;1. Loads the black color (color 0) to the default color - video port 3c8
;2. Loop on 256 colors - For each color RGB (0-63) load for video port 3c9:
;Red -  Loading the amount of Red (0-63)
;Green - Loading the amount of green (0-63)
;Blue - Loading the amount of blue (0-63)
proc CopyBmpPalette		near															
	push cx
	push dx
	
	mov si,offset Palette
	mov cx,256
	mov dx,3C8h
	mov al,0  ; black first							
	out dx,al ;3C8h
	inc dx	  ;3C9h
CopyNextColor:
	mov al,[si+2] 		; Red				
	shr al,2 			; divide by 4 Max (cos max is 63 and we have here max 255 ) (loosing color resolution).				
	out dx,al 	
	
	mov al,[si+1] 		; Green.				
	shr al,2            
	out dx,al
	
	mov al,[si] 		; Blue.				
	shr al,2            
	out dx,al
	
	add si,4 			; Point to next color.  (4 bytes for each color BGR + null)				
								
	loop CopyNextColor
	
	pop dx
	pop cx	
	ret
endp CopyBmpPalette

; BMP graphics are saved upside-down.
; Read the graphic line by line (BmpRowSize lines in VGA format),
; displaying the lines from bottom to top.

;My explanation:
;Parameters: Image Width - BmpColSize and Image Length - BmpRowSize,location on screen - BmpLeft = X, BmpTop = Y
;In DS - there is an array (ScrLine) called  one line at a time

;The bmp image is saved inverted by rows (starts at the last row )
;Each row size (image width) is rounded to 4 bytes DWORD
;Loop on the number of lines:
 ;1.Read each row by the width of the image (skipping the "upholstery") to the memory in DS
 ;2.Copy the line to the correct position in (320 * Y + X) A000
 ;3.The copy is done using rep movsb
proc ShowBMP 
	push cx
	
	mov ax, 0A000h
	mov es, ax
	
	mov cx,[BmpRowSize]
	 
	mov ax,[BmpColSize] ; row size must dived by 4 so if it less we must calculate the extra padding bytes
	xor dx,dx
	mov si,4
	div si
	cmp dx,0
	mov bp,0
	jz @@row_ok
	mov bp,4
	sub bp,dx

@@row_ok:	
	mov dx,[BmpLeft]
	
@@NextLine:
	push cx
	push dx
	
	mov di,cx  ; Current Row at the small bmp (each time -1)
	add di,[BmpTop] ; add the Y on entire screen
	
	; next 5 lines  di will be  = cx*320 + dx , point to the correct screen line
	mov cx,di
	shl cx,6
	shl di,8
	add di,cx
	add di,dx
	 
	; small Read one line
	mov ah,3fh
	mov cx,[BmpColSize]  
	add cx,bp  ; extra  bytes to each row must be divided by 4
	mov dx,offset ScrLine
	int 21h
	
	; Copy one line into video memory
	cld ; Clear direction flag, for movsb
	mov cx,[BmpColSize]  
	mov si,offset ScrLine
	rep movsb ; Copy line to the screen
	
	pop dx
	pop cx
	 
	loop @@NextLine
	
	pop cx
	ret
endp ShowBMP 

;An action that draws black background
proc DrawBackground
	push dx
	
	mov dx, offset FileBlackBackground
	mov [BmpLeft],0
	mov [BmpTop],0
	mov [BmpColSize], BMP_WIDTH
	mov [BmpRowSize] ,BMP_HEIGHT
		
	mov dx, offset FileBlackBackground
	call OpenShowBmp
	
	pop dx
	ret 
endp DrawBackground

;An action that draws the opening screen
proc DrawOpeningScreen
	push dx
	
	mov dx, offset FileOpeningScreen
	mov [BmpLeft],0
	mov [BmpTop],0
	mov [BmpColSize], BMP_WIDTH
	mov [BmpRowSize] ,BMP_HEIGHT
		
	mov dx, offset FileOpeningScreen
	call OpenShowBmp
	
	pop dx
	ret
endp DrawOpeningScreen

;An action that draws the instructions.
proc DrawInstructions
	push dx
	
	mov dx, offset FileInstructions
	mov [BmpLeft],0
	mov [BmpTop],0
	mov [BmpColSize], BMP_WIDTH
	mov [BmpRowSize] ,BMP_HEIGHT
		
	mov dx, offset FileInstructions
	call OpenShowBmp
	
	pop dx
	ret
endp DrawInstructions

;An action that set the cursor position. In dl the x position and in  dh the y position. 
;X position:0-25,Y position: 0-80 
proc SetCursor 
	push bx
	push ax
    push dx
	
	mov dl,[XCursorPosition]
	mov dh,[YCursorPosition]
	mov  ah, 2                  
    mov  bh, 0  ;Page number
	int  10h                    
	
	pop dx
	pop ax
	pop bx
    ret
endp SetCursor

;An action that print the player's points into the screen
proc PrintPoints
	push ax
	push dx
	
	;Set cursor
	mov [XCursorPosition],0
	mov [YCursorPosition],0
	call SetCursor
	
	;Print the message
	mov dx,offset PointsMessage
	mov ah,9
	int 21h
	
	;We need to change the number (CorrectPlayer) from decimal value to chars
	cmp [CorrectPlayer],10 ;To see if it's a 2 digit number.
	jb WriteOneDigit
	
	;The number is a 2 digit number
	mov al,[CorrectPlayer]
	mov ah,0
	mov bl,10
	div bl ;Now in al there is the tens digit and in ah there is the units digit.
	
	mov [UnitsDigit],ah
	add [UnitsDigit],'0'
	mov [TensDigit],al
	add [TensDigit],'0'
	
	;Set the cursor
	mov [XCursorPosition],7
	mov [YCursorPosition],0
	call SetCursor
	
	;Print tens digit:
	mov dl,[TensDigit]  
	mov ah, 2
	int 21h
	
	;Print units digit:
	mov dl,[UnitsDigit]  
	mov ah, 2
	int 21h
	
	jmp @@exit
		
	WriteOneDigit:	
	mov al,[CorrectPlayer]
	mov [UnitsDigit],al
	add [UnitsDigit],'0'
	
	;Set the cursor
	mov [XCursorPosition],7
	mov [YCursorPosition],0
	call SetCursor
	
	;Print units digit:
	mov dl,[UnitsDigit]  
	mov ah, 2
	int 21h
	
	@@exit:
	pop dx
	pop ax
	ret
endp PrintPoints

;An action that prints to the screen the record.
proc PrintRecord
	push ax
	push bx
	push cx
	push dx
	
	;Set cursor
	mov [XCursorPosition],0
	mov [YCursorPosition],1
	call SetCursor
	
	;Print message
	mov dx,offset RecordMessage
	mov ah,9
	int 21h
	
	;Open the file:
	mov ah,3Dh
	mov al,2
	mov dx,offset FileRecord
	int 21h
	
	;copy the file handle into a variable called FileHandle
	mov [FileHandle],ax
	
	;Change the pointer in the file to the end
	mov [PointerFilePosition],2
	call ChangePointerFile
	
	;Save the length of the file 
	mov [LengthRecordFile],ax
	
	;Return the pointer to the start:
	mov [PointerFilePosition],0
	call ChangePointerFile
	
	;Read all the file:
	mov ah,3Fh
	mov bx,[FileHandle]
	mov cx,[LengthRecordFile]
	mov dx,offset TheRecord
	int 21h
	
	;Print the record:
	mov dx,offset TheRecord
	mov ah,9
	int 21h
	
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp PrintRecord

;An action that change the pointer in the file. In the variable PointerFilePosition there is the position we need to do.
proc ChangePointerFile
	push ax
	push bx
	push cx
	push dx
	
	mov ah,42h
	mov al,[PointerFilePosition]
	mov bx,[FileHandle]
	mov cx,0
	mov dx,0
	int 21h
	
	pop dx
	pop cx
	pop bx
	pop ax
	ret
endp ChangePointerFile

;An action that checks the record and compare it to the player's correct sequence. If the player breaks the record- the action write the new one in the file:FileRecord. 
proc CheckRecord
	push ax
	push dx
	push bx
	push cx
	
	;Open the file:
	mov ah,3Dh
	mov al,2
	mov dx,offset FileRecord
	int 21h
	
	;copy the file handle into a variable called FileHandle
	mov [FileHandle],ax
	
	;Change the pointer in the file to the end (we can't use the action because we need the value in ax)
	mov ah,42h
	mov al,2
	mov bx,[FileHandle]
	mov cx,0
	mov dx,0
	int 21h
	
	;Save the length of the file
	mov [LengthRecordFile],ax
	
	;Return the pointer to the start:
	mov [PointerFilePosition],0
	call ChangePointerFile
	
	cmp [LengthRecordFile],1
	je ReadOneDigit
	
	;The number is a 2 digit number: Read the tens digit and the units digit separately
	;Read from the file the tens digit:
	mov ah,3Fh
	mov bx,[FileHandle]
	mov cx,1
	mov dx,offset Char
	int 21h
	
	sub [Char],'0' ;To change the char into decimal value
	
	mov ax,10
	mul [Char]
	mov [BestRecord],al
	
	;Read from the file the units digit:
	mov ah,3Fh
	mov bx,[FileHandle]
	mov cx,1
	mov dx,offset Char
	int 21h
	
	sub [Char],'0' ;To change the char into decimal value
	
	mov al,[Char]
	add [BestRecord],al
	
	jmp Continue
	
	ReadOneDigit:
	;Read from the file:
	mov ah,3Fh
	mov bx,[FileHandle]
	mov cx,1
	mov dx,offset Char
	int 21h
	
	sub [Char],'0' ;To change the char into decimal value
	mov al,[Char]
	mov [BestRecord],al
	
	Continue:	
	mov al,[BestRecord]
	cmp [CorrectPlayer],al
	jbe @@exit ;The player didn't break the record.
	
	;Now we need to write to the file the new record
	;First, we need to change the number (CorrectPlayer) from decimal to chars
	cmp [CorrectPlayer],10
	jb @@WriteOneDigit
	
	;The number is a 2 digit number
	mov al,[CorrectPlayer]
	mov ah,0
	mov bl,10
	div bl ;Now in al there is the tens digit and in ah there is the units digit.
	
	mov [UnitsDigit],ah
	add al,'0'
	mov [Char],al
	
	;Return the pointer to the start:
	mov [PointerFilePosition],0
	call ChangePointerFile
	
	;Now we write to the file the tens digit
	mov ah,40h
	mov bx,[FileHandle]
	mov cx,1
	mov dx,offset Char
	int 21h
	
	mov ah,[UnitsDigit]
	add ah,'0'
	mov [Char],ah
	
	;Now we write to the file the units digit
	mov ah,40h
	mov bx,[FileHandle]
	mov cx,1
	mov dx,offset Char
	int 21h
	
	jmp @@exit
		
	@@WriteOneDigit:	
	mov al,[CorrectPlayer]
	add al,'0'
	mov [BestRecord],al
	
	;Return the pointer to the start:
	mov [PointerFilePosition],0
	call ChangePointerFile
	
	;Now we write to the file
	mov ah,40h
	mov bx,[FileHandle]
	mov cx,1
	mov dx,offset BestRecord
	int 21h

	@@exit:
	;Close the file:
	mov ah,3Eh
	mov bx,[FileHandle]
	int 21h
	
	pop cx
	pop bx
	pop dx
	pop ax
	ret
endp CheckRecord

;An action that prints the loser screen
proc LoserScreen
	push dx
	
	mov dx, offset FileLoser
	mov [BmpLeft],0
	mov [BmpTop],0
	mov [BmpColSize], BMP_WIDTH
	mov [BmpRowSize] ,BMP_HEIGHT 
		
	mov dx, offset FileLoser
	call OpenShowBmp
	
	pop dx
	ret
endp LoserScreen

;An action that prints the loser screen
proc WinnerScreen
	push dx
	
	mov dx, offset FileWinner
	mov [BmpLeft],0
	mov [BmpTop],0
	mov [BmpColSize], BMP_WIDTH
	mov [BmpRowSize] ,BMP_HEIGHT 
		
	mov dx, offset FileWinner
	call OpenShowBmp
	
	pop dx
	ret
endp WinnerScreen

;An action that prints an exit message
proc PrintExitMessage
	push ax
	push dx
	
	;Set cursor
	mov [XCursorPosition],0
	mov [YCursorPosition],24
	call SetCursor
	
	;Print the message
	mov dx,offset ExitMessage
	mov ah,9
	int 21h
	
	pop dx
	pop ax
	ret
endp PrintExitMessage

;Just for dibbuging
proc ShowAxDecimal
       push ax
	   push bx
	   push cx
	   push dx
	   
	   ; check if negative
	   test ax,08000h
	   jz PositiveAx
			
	   ;  put '-' on the screen
	   push ax
	   mov dl,'-'
	   mov ah,2
	   int 21h
	   pop ax

	   neg ax ; make it positive
PositiveAx:
       mov cx,0   ; will count how many time we did push 
       mov bx,10  ; the divider
   
put_mode_to_stack:
       xor dx,dx
       div bx
       add dl,30h
	   ; dl is the current LSB digit 
	   ; we cant push only dl so we push all dx
       push dx    
       inc cx
       cmp ax,9   ; check if it is the last time to div
       jg put_mode_to_stack

	   cmp ax,0
	   jz pop_next  ; jump if ax was totally 0
       add al,30h  
	   mov dl, al    
  	   mov ah, 2h
	   int 21h        ; show first digit MSB
	       
pop_next: 
       pop ax    ; remove all rest LIFO (reverse) (MSB to LSB)
	   mov dl, al
       mov ah, 2h
	   int 21h        ; show all rest digits
       loop pop_next
		
	   mov dl, ','
       mov ah, 2h
	   int 21h
   
	   pop dx
	   pop cx
	   pop bx
	   pop ax
	   
	   ret
endp ShowAxDecimal

start:
	mov ax, @data
	mov ds, ax
; --------------------------
; Your code here
; --------------------------
	call SetGraphic
	 
	Open: 
	call DrawOpeningScreen
		
	;Now we will wait until the player will prees any bottom and will jump accordingly.(play/exit/instructions)	
	Mouse:
	call _200MiliSecDelay ;A small dilay 
	
	;To show the mouse:
	mov ax,1
	int 33h
		
	;To receive the position of the mouse:
	mov ax,3
	int 33h
	
	cmp bx,1
	jne Mouse ;If nothing pressed go again to StartMouse
	
	;To hide the mouse
	mov ax,2
	int 33h
	
	;To receive the pixel's color 
	shr cx,1
	mov bh,0	
	mov ah,0Dh
	int 10h
	
	cmp al,48 ;Press on the play bottom
	je StartPlay
	
	cmp al,79 ;Press on the exit bottom
	je exit1 ;Can't jump directly to exit (out of range)
	
	cmp al,252 ;Press on the instructions
	je Instructions 
	
	jmp Mouse 
	
	Instructions:
	call DrawInstructions
	
	;silent input as ReadKey
	mov ah,7 
	int 21h
	
	jmp Open
	
	exit1:
	call DrawBackground
	jmp exit
	
	StartPlay:	
	call DrawBackground
	call DrawBoard	 
		
StartGame:
	mov [ThisComputerSeqPos],0
	call PrintPoints
	call PrintRecord
	
	call RandomMove
	;Save the random number in the array in the position:TotalComputerSeqPos
	mov al,[Move]
	mov bl,[TotalComputerSeqPos]
	mov bh,0
	mov [SequenceArray+bx],al
	
	call PlayComputerSeq
	
	;To show the mouse:
	StartMouse:
	call _200MiliSecDelay ;A small dilay 
	mov ax,1
	int 33h
		
	;To receive the position of the mouse:
	mov ax,3
	int 33h
	
	cmp bx,1
	jne StartMouse ;If nothing pressed go again to StartMouse
	
	;To hide the mouse
	mov ax,2
	int 33h
	
	;To receive the pixel's color 
	shr cx,1
	mov bh,0	
	mov ah,0Dh
	int 10h
	
	;Compare the pixel value (in al) to 0 (black color)
	cmp al,0
	je StartMouse
	
	mov [ColorPlayer],al
	
	call CompareColorRect
	
	;Check if the player was wrong- if he did- the game is over
	cmp [IsOver],0
	je ExitLoser
	
	mov cl,[ThisComputerSeqPos]
	dec cl ;In the action CompareColorRect, I added 1 to ThisComputerSeqPos
			
	;Compare the positions- if they are equall:we have raeched the end of the array that needs checking.
	                       ;else-we need to continue with the checking.
	cmp cl,[TotalComputerSeqPos]
	jne StartMouse ;To continue checking the next color
	
	inc [CorrectPlayer]
	mov cl,[CorrectPlayer]
	
	;Compare the most largest sequence to the player's sequence
	cmp cl,[BiggestCorrect]
	je ExitWinner ;The player wins
	
	inc [TotalComputerSeqPos]
	jmp StartGame
	
	ExitLoser:
	call LoserScreen
 	;silent input as ReadKey
	mov ah,7 
	int 21h
	call DrawBackground
	call DrawBoard
	call PlayComputerSeq ;Play the correct sequence to the player
	call PrintExitMessage
	jmp ReadKey
	
	ExitWinner:
	call WinnerScreen
	
	ReadKey: 
	call CheckRecord ;To check the record
	;silent input as ReadKey
	mov ah,7 
	int 21h
	
	call DrawBackground
	;Set cursor to the original position
	mov [XCursorPosition],0
	mov [YCursorPosition],0
	call SetCursor
exit:
	mov ax, 4c00h
	int 21h

EndOfCsLbl:
END start