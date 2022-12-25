;PROGRAM CODE FOR TIC TAC TOE GAME
;AUTHORS: HAMZA, MUSHAHID & LAKSH
;LANGUAGE: ASSEMBLY-LANGUAGE-32-BIT FOR X86 PROCESSORS
;YEAR OF DEVELOPMENT : 2019	 

INCLUDE IRVINE32.INC

.DATA

HEADER 	BYTE 	" ***** * ****     *****  **  ****   ***** ***** ****",0ah,0dh
	BYTE	"   *   * *          *   *  * *        *   *   * *    ",0ah,0dh
	BYTE	"   *   * *          *   *  * *        *   *   * ****",0ah,0dh
	BYTE	"   *   * *          *   **** *        *   *   * *    ",0ah,0dh	
	BYTE	"   *   * ****       *   *  * ****     *   ***** ****",0ah,0dh
	BYTE	" ============     ===============   ================",0


coordi	BYTE 10,5,10,15,10,25,16,5,16,15,16,25,22,5,22,15,22,25
u_coord	BYTE 18 DUP (0)
Nameplate BYTE 0ah,0dh,"~~~~~TIC TAC TOE GAME - By HMLSoft~~~~",0
Grid BYTE "     The Tic Tac Toe Grid:     ",0

row = 19	; CONSTANT
col = 31	; CONSTANT

rows BYTE 0
cols BYTE 0

;PMH'S DECLARATIONS
last_row	BYTE	?
last_col	BYTE	?
sign		BYTE	?
end_game	BYTE	?
msgWinner	BYTE	"Match won!"
p1winmsg	BYTE	"   PLAYER 1 WINS (X)   ",0
p2winmsg	BYTE	"   PLAYER 2 WINS (O)   ",0
Compwinmsg	BYTE	"   COMPUTER WINS (O)   ",0
DrawMsg		BYTE	"      MATCH DRAWN      ",0 
turns		BYTE 0
response	BYTE ?
FileVal		DWORD 0			;It Would contain the value read from file as int
HighScore	DWORD 0


testGrid BYTE 9 DUP(2)
test2Grid BYTE 9 DUP(2)

mainGrid BYTE 3 DUP(2)
		 rowsize = $ - mainGrid
		 BYTE	 3 DUP(2)
		 BYTE	 3 DUP(2)
			
.CODE

;--------------------------------------------------------------------------------------

main PROC				;	MAIN PROCEDURE

call Welcome_Screen			;	Entering The Game
exit					;	Exiting The Game

main ENDP				;	END OF MAIN PROCEDURE

;--------------------------------------------------------------------------------------

.DATA
Main_Menu BYTE "MAIN MENU:",0
Option1 BYTE "PRESS 1 FOR 2-PLAYERS",0
Option2 BYTE "PRESS 2 FOR PLAYER VS. COMPUTER",0
Option3 BYTE "PRESS 3 FOR HIGHEST SCORE",0
Option4 BYTE "PRESS 4 TO EXIT GAME",0
PutChoice	BYTE "ENTER YOUR CHOICE: ",0
PutChoice2	BYTE "ERROR! RE-ENTER CHOICE: ",0
choose BYTE ?
mode	BYTE	?
color	DWORD	?

.CODE

					;WELCOME SCREEN DISPLAY

;--------------------------------------------------------------------------------------

Welcome_Screen PROC
mov eax,143
call SetTextColor

L0:		; To Keep Stay In Proc 
call Clrscr
mov edx,offset Header
call WriteString
call CRLF
call GetTextColor
mov color,eax

mov eax,46
call SetTextColor

mov edx,offset nameplate
call writestring
call CRLF
call CRLF

mov eax,79
call SetTextColor

mov edx, OFFSET Main_Menu
call WriteString
call CRLF
call CRLF

mov eax,color
call SetTExtColor

mov edx, OFFSET Option1		;change
call WriteString
call CRLF

mov edx, OFFSET Option2		;change
call WriteString
call CRLF

mov edx, OFFSET Option3
call WriteString
call CRLF

mov edx, OFFSET Option4
call WriteString
call CRLF
call CRLF

mov eax,31
call SetTextColor

mov edx, OFFSET PutChoice
call WriteString
jmp L2
L1:
mov edx, OFFSET PutChoice2
call WriteString
L2:
call ReadDec
cmp eax,0
JBE L1
cmp eax,4
JA L1

cmp eax,1
JE Page1
cmp eax,2
JE Page2
cmp eax,3
JE Page3
cmp eax,4
JE Return

Return:
mov eax,FileVal
cmp eax,HighScore
jae RETURN1
	call Write_file
RETURN1:
ret

Page1:
mov eax,color
call SetTExtColor
mov mode,1;		2 Players
call _2Players
call Clear_Everything
jmp L0

Page2:
mov eax,color
call SetTExtColor
mov mode,2	;	2 players
call _VsComputer
call Clear_Everything
jmp L0

Page3:
mov eax,color
call SetTExtColor
call _HighScore
jmp L0

Welcome_Screen ENDP

;--------------------------------------------------------------------------------------
				
.DATA
vsComp			BYTE	"~	PLAYER VS COMPUTER	~",0
_2playersOut	BYTE	"~	PLAYER-1 VS PLAYER-2	~",0
_high			BYTE	"~	HIGHSCORES	~",0
goback			BYTE	"Press 0 TO GO BACK",0
play			BYTE	"PRESS 1 TO PLAY ON",0
continue		BYTE	"PRESS ANY KEY TO CONTINUE...",0
p1turn			BYTE	"PLAYER-1'S TURN (X)",0
p2turn			BYTE	"PLAYER-2'S TURN (O)",0
base2			BYTE	2

.CODE

;	PAGES DISPLAY PROCS
;--------------------------------------------------------------------------------------

_2Players PROC		;	Procedure to Control _2Players Screen

mov eax,95
call SetTextColor

call Clrscr
mov edx,OFFSET NamePlate
call WriteString
call CRLF
call CRLF
mov edx,OFFSET _2playersout		;change
call WriteString
call CRLF
call CRLF
call DisplayGrid
call CRLF
call CRLF
mov edx,OFFSET goBack
call WriteString
call CRLF

mov edx,OFFSET play
call WriteString
call CRLF

mov edx, OFFSET PutChoice
call WriteString
jmp L2

	L1:
mov edx, OFFSET PutChoice2
call WriteString
	
	L2:
call ReadDec
cmp eax,0
JB L1
cmp eax,1
JA L1

cmp eax,0
JE BACK 
cmp eax,1
JE DASH

BACK:
ret

DASH:
mov end_game,0
mov ecx,9

Get_Input:
mov edx,0
mov eax,ecx
div base2

cmp ah,0
JE Put_Tick
call Player1
cmp end_game,1
JE Return
jmp L3

Put_Tick:
call Player2
cmp end_game,1					;Incase of Winner
JE Return
L3:
	Loop Get_Input

call MoveCursorToEnd
call CRLF
mov edx, OFFSET DrawMsg	;INCASE OF DRAW
call WriteString
call CRLF
call WaitMsg

Return:
	ret

_2Players ENDP						; END OF _2Players PROCEDURE

;--------------------------------------------------------------------------------------

Player2 PROC

mov dh,31
mov dl,0
call GoToXY
mov eax,240
call SetTextColor
mov edx,offset p2turn
call WriteString
call UserPlay
mov eax,'O'
call WriteChar
mov sign, 1
call RegisterGrid
ret

Player2 ENDP

;--------------------------------------------------------------------------------------

Player1 PROC
mov dh,31
mov dl,0
call GoToXY
mov eax,79
call SetTextColor
mov edx,offset p1turn
call WriteString
call UserPlay
mov eax,'X'
call WriteChar
mov sign, 0
call RegisterGrid
ret

Player1 ENDP

;--------------------------------------------------------------------------------------

UserPlay PROC

mov dh,coordi[0]
mov dl,coordi[1]
call GoToXY
call GetKey
ret

UserPlay ENDP

;--------------------------------------------------------------------------------------

.DATA
difficulty	BYTE	"Press 1 For Beginner Mode",0ah,0dh
			BYTE	"Press 2 For Expert Mode",0ah,0dh,0
gaming_mode	BYTE	0
highscoremsg	BYTE	"HIGHEST SCORE : ",0
teststr		BYTE	"Testing...",0ah,0dh,0

.CODE

;--------------------------------------------------------------------------------------

_VsComputer PROC	;	Procedure to Control _VsComputer Screen

mov eax,111
call SetTextColor

call Clrscr
mov edx,OFFSET NamePlate
call WriteString
call CRLF
call CRLF
mov edx,OFFSET VsComp
call WriteString
call CRLF
call CRLF
call DisplayGrid
call CRLF
call CRLF
mov edx,OFFSET goBack
call WriteString
call CRLF

mov edx,OFFSET difficulty
call WriteString
call CRLF

mov edx, OFFSET PutChoice
call WriteString
jmp L2

L1:
	mov edx, OFFSET PutChoice2
	call WriteString	
L2:
	call ReadDec
	cmp eax,0
	JB L1
	cmp eax,2
	JA L1

cmp eax,0
je RETURN
cmp eax,1
je Beginner1
cmp eax,2
je Expert

Beginner1:
mov gaming_mode, 1
jmp GamePlay

Expert:
mov gaming_mode,2

GamePlay:
call Beginner

RETURN:
ret

_VsComputer ENDP		; END OF _VsComputer PROCEDURE

;-------------------------------------------------------------------------------------

Beginner PROC

mov end_game,0
mov ecx,9

Get_Input:
	mov edx,0
	mov eax,ecx
	div base2

	cmp ah,0
	JE CompTurn
UserTurn:
	call PlayUser
	cmp end_game,1
	JE Return
	jmp L3

CompTurn:
	call PlayComp
	cmp end_game,1				;Incase of Winner
	JE Return

L3:
	Loop Get_Input

call MoveCursorToEnd
call CRLF
mov edx, OFFSET DrawMsg	;INCASE OF DRAW
call WriteString
call CRLF
call WaitMsg

RETURN:
ret

Beginner ENDP

;-------------------------------------------------------------------------------------

PlayUser PROC

mov eax,79
call SetTextColor
mov dh,coordi[0]
mov dl,coordi[1]
call GoToXY
call GetKey

mov eax,'X'
call WriteChar
mov sign, 0
call RegisterGrid
ret

PlayUser ENDP

;-------------------------------------------------------------------------------------

PlayComp PROC

mov eax,240
call SetTextColor
call GenerateRandom
TRUE1:
call CompUsedCoordi
mov eax,'O'
call WriteChar
mov sign, 3
call RegisterGrid

ret
PlayComp ENDP

;--------------------------------------------------------------------------------------

GenerateRandom PROC

mov bl, 2
cmp gaming_mode, 2
JNE random
call generateresponse
ret
random:

call Randomize
mov eax,18
call RandomRange
mov esi,eax

mov ah,0
div base2

cmp ah,0
JE RETURN
dec esi

RETURN:
ret

GenerateRandom ENDP

;--------------------------------------------------------------------------------------

CompUsedCoordi PROC		;	PROCEDURE Will NOt Allow Multiple Tics On Same Location

L1:
mov ah,coordi[esi]
mov al,coordi[esi+1]

mov last_row,ah		; temp ROW
mov last_col,al		; temp COL

cmp ah, u_coord[esi]
JE TRUE1

False1:
mov u_coord[esi],ah
mov u_coord[esi+1],al
mov dh,ah
mov dl,al
call GoToXY
ret

TRUE1:
cmp al, u_coord[esi+1]
JE TRUE2
jmp False1

TRUE2:
call GenerateRandom
jmp L1

CompUsedCoordi ENDP			; END OF PROCEDURE

;--------------------------------------------------------------------------------------

_HighScore PROC			;	Procedure to Read HighScore From File And Display It

mov eax,47
call SetTextColor

call Clrscr
mov edx,OFFSET NamePlate
call WriteString
call CRLF
call CRLF
mov edx,OFFSET _high
call WriteString
call CRLF
call CRLF

call Read_from_file
mov eax , NoBytes
call ConvertToint		;After this Eax would contain the value read from file 
mov edx,offset highscoremsg
call WriteString
call WriteDec
mov FileVal,eax
call CRLF
call CRLF
mov edx,OFFSET goBack
call WriteString
call CRLF

mov edx, OFFSET PutChoice
call WriteString
jmp L2
L1:
	mov edx, OFFSET PutChoice2
	call WriteString
	
L2:
	call ReadDec
	cmp eax,0
	JB L1
	cmp eax,0
	JA L1
	ret
_HighScore ENDP					; End Of HIGHSCORE PROCEDURE

;--------------------------------------------------------------------------------------

GetKey PROC

mov esi,0
Get_Key:
mov eax,50
call Delay		; DELAY FOR KEY PRESS
call ReadKey	; READING KEY WITHOUT PRINTING IT
JZ Get_Key		
				; CONDITIONS TO CHECK KEY
mov al,0		; DELETING ASCII CODES AND TAKING VIRTUAL CODES
cmp ax, 1C00h	; Enter Key
je RETURN		; if Enter/Space key is pressed quit from function
cmp ax, 3900h	; Space Key
je RETURN		
cmp ax,4800h	; Up Arrow Key
je Up
cmp ax,5000h	; Down Arrow Key
je Down
cmp ax,4B00h	; Left Arrow Key
je Left
cmp ax,4D00h	; Right Arrow Key
je Right
cmp ax,1100h	; W key
je Up
cmp ax,1F00h	; S key 
je Down
cmp ax,1E00h	; A key
je Left
cmp ax,2000h	; D key
je Right
jmp Get_Key

Right:
	cmp esi,16
	JE Reset
	add esi,2
	mov dh,coordi[esi]
	mov dl,coordi[esi+1]
	call GoToXY
	jmp Get_Key
	
Left:
	cmp esi,0
	JE Setup
	sub esi,2
	mov dh,coordi[esi]
	mov dl,coordi[esi+1]
	call GoToXY
	jmp Get_Key

UP:
	cmp esi,6
	JB Row_Setup
	sub esi,6
	mov dh,coordi[esi]
	mov dl,coordi[esi+1]
	call GoToXY
	jmp Get_Key

DOWN:
	cmp esi,11
	JA Row_Reset
	add esi,6
	mov dh,coordi[esi]
	mov dl,coordi[esi+1]
	call GoToXY
	jmp Get_Key

Row_Reset:
	sub esi,12
	mov dh,coordi[esi]
	mov dl,coordi[esi+1]
	call GoToXY
	jmp Get_Key

Row_Setup:
	add esi,12
	mov dh,coordi[esi]
	mov dl,coordi[esi+1]
	call GoToXY
	jmp Get_Key

Reset:
	mov esi,0
	mov dh,coordi[esi]
	mov dl,coordi[esi+1]
	call GoToXY
	jmp Get_Key

Setup:
	mov esi,16
	mov dh,coordi[esi]
	mov dl,coordi[esi+1]
	call GoToXY
	jmp Get_Key

Return:
	call UsedCoordi
	ret
GetKey ENDP

;----------------------------------------------------------------------------------------

UsedCoordi PROC		; PROCEDURE Will NOt Allow Multiple Tics On Same Location

mov ah,coordi[esi]
mov al,coordi[esi+1]

mov last_row,ah		; temp ROW
mov last_col,al		; temp COL


cmp ah, u_coord[esi]
JE TRUE1

False1:
mov u_coord[esi],ah
mov u_coord[esi+1],al
ret

TRUE1:
cmp al, u_coord[esi+1]
JE TRUE2
jmp False1

True2:
call GetKey
ret

UsedCoordi ENDP			; END OF PROCEDURE

;--------------------------------------------------------------------------------------

MoveCursorToEnd PROC	; Procedure to Move Cursor To End Just To Avoid Display Issues
mov dl,40	; moving cursor to end
mov dh,30	; moving cursor to end
call GoToXY
ret
MoveCursorToEnd ENDP

;--------------------------------------------------------------------------------------

DisplayGrid PROC		;	PROCEDURE TO DISPLAY 3X3 GRID

call CRLF
mov edx,offset Grid
call WriteString
call CRLF
mov rows,0
mov ecx, row
L1:
	push ecx
	mov cols,0
	mov ecx,col
			L2:
				cmp cols,0
				je TRUE1

				cmp cols,30
				je TRUE1

				cmp cols,10
				je TRUE1

				cmp cols,20
				je TRUE1

				cmp rows,0
				je TRUE1

				cmp rows,18
				je TRUE1

				cmp rows,6
				je TRUE1

				cmp rows,12
				je TRUE1

				mov al," "
				call WriteChar
				
			L3:
				inc cols
			Loop L2
	
	inc rows
	call CRLF
	pop ecx
Loop L1

ret

TRUE1:
mov eax, '*' 
call WriteChar
jmp L3

DisplayGrid ENDP			; END OF PROCEDURE

;--------------------------------------------------------------------------------------

CrossOut PROC				; PROCEDURE TO CROSS-OUT STREAK IF MADE

; FOR ROW # 1
mov dl,2
mov dh,9
mov ecx,27
L1:
	mov eax,'-'
	call GoToXY
	call WriteChar
	inc dl
Loop L1

; FOR ROW # 2
mov dl,2
mov dh,15
mov ecx,27
L2:
	mov eax,'-'
	call GoToXY
	call WriteChar
	inc dl
Loop L2

; FOR ROW # 3
mov dl,2
mov dh,21
mov ecx,27
L3:
	mov eax,'-'
	call GoToXY
	call WriteChar
	inc dl
Loop L3

; FOR COL # 1
mov dl,5
mov dh,8
mov ecx,15
L4:
	mov eax,'|'
	call GoToXY
	call WriteChar
	inc dh
Loop L4

; FOR COL # 2
mov dl,15
mov dh,8
mov ecx,15
L5:
	mov eax,'|'
	call GoToXY
	call WriteChar
	inc dh
Loop L5

; FOR COL # 3
mov dl,25
mov dh,8
mov ecx,15
L6:
	mov eax,'|'
	call GoToXY
	call WriteChar
	inc dh
Loop L6


; FOR DIAGNOL # 1
mov dl,0
mov dh,7
mov ecx,16
L7:
	mov eax,'/'
	call GoToXY
	call WriteChar
	inc dh
	inc dl
	inc dl
Loop L7

; FOR DIAGNOL # 2
mov dl,30
mov dh,7
mov ecx,16
L8:
	mov eax,'/'
	call GoToXY
	call WriteChar
	inc dh
	dec dl
	dec dl
Loop L8

call MoveCursorToEnd
ret
CrossOut ENDP					; END OF PROCEDURE

;--------------------------------------------------------------------------------------

RegisterGrid PROC uses eax ecx ebx edx edi esi
	

	mov eax, 0
	mov ebx, 0
	mov al, last_row
	sub eax, 10
	mov bl, 6
	div bl
	mov last_row, al
	mov al, last_col
	sub al, 5
	mov bl, 10
	div bl
	mov last_col, al

	mov al, last_row
	mov bl, rowsize
	mul bl
	mov bl, last_col
	mov esi, eax
	mov al, sign
	mov mainGrid[esi + ebx], al
	inc turns
	call CheckWinner

	ret
RegisterGrid ENDP

;--------------------------------------------------------------------------------------

CheckWinner PROC uses eax ecx ebx edx edi esi

	mov edi, 0 ;row
	mov ecx, 3
	L1:
		mov eax, edi
		mov bl, 3
		mul bl
		mov bl, mainGrid[eax]
		cmp bl, 2
		JE skip
		cmp bl, mainGrid[eax + 1]
		JNE skip
		cmp bl, mainGrid[eax+2]
		JNE skip
		call displayWinningMessage
		ret

		skip:
		inc edi
	loop L1

	mov esi, 0
	mov ecx, 3
	L2:
		mov bl, mainGrid[esi]
		cmp bl, 2
		JE skip2
		cmp bl, mainGrid[esi +3]
		JNE skip2
		cmp bl, mainGrid[esi+6]
		JNE skip2
		call displayWinningMessage
		ret


		skip2:
		inc esi
	loop L2

	mov bl, mainGrid[0]
	cmp bl, 2
	JE skip3
	cmp bl, mainGrid[4]
	JNE skip3
	cmp bl, mainGrid[8]
	JNE skip3
	
	call displayWinningMessage
	ret

	skip3:

	mov bl, mainGrid[2]
	cmp bl, 2
	JE skip4
	cmp bl, mainGrid[4]
	JNE skip4
	cmp bl, mainGrid[6]
	JNE skip4
	call displayWinningMessage
	ret

	skip4:
	ret
	
CheckWinner ENDP

;--------------------------------------------------------------------------------------

restoretestgrid PROC uses esi edi ecx

	mov esi, OFFSET mainGrid
	mov edi, OFFSET testGrid
	mov ecx, 9
	rep movsb
	ret
restoretestgrid ENDP

;--------------------------------------------------------------------------------------

displayWinningMessage PROC
	
	call MoveCursorToEnd
	call CRLF
	mov end_game,1
	cmp sign, 0
	JNE p2winsOrComp
		
		cmp gaming_mode,1
		JE TRUE1
		cmp gaming_mode,2
		JE TRUE2
	jmp TRUE3

	TRUE1:
	add HighScore,5
	jmp TRUE3

	TRUE2:
	add HighScore,20
	
	TRUE3:	
	mov edx, OFFSET p1winmsg
	call WriteString
	call CRLF
	call WaitMsg
	ret

	p2winsOrComp:
	cmp sign,1
	JNE CompWins
	mov edx, OFFSET p2winmsg
	call WriteString
	call CRLF
	call WaitMsg
	ret

	CompWins:
	mov edx, OFFSET Compwinmsg
	call WriteString
	call CRLF
	call WaitMsg
	ret


displayWinningMessage ENDP

;--------------------------------------------------------------------------------------

CheckWinnerTest PROC  uses eax ecx ebx esi edi

	mov edi, 0 ;row
	mov ecx, 3
	L1:
		mov eax, edi
		mov bl, 3
		mul bl
		mov bl, testGrid[eax]
		cmp bl, 2
		JE skip
		cmp bl, testGrid[eax + 1]
		JNE skip
		cmp bl, testGrid[eax+2]
		JNE skip
		cmp bl, 3
		JE pc1
		mov edx, 1
		ret
		pc1:
		mov edx, 0
		ret

		skip:
		inc edi
	loop L1

	mov esi, 0
	mov ecx, 3
	L2:
		mov bl, testGrid[esi]
		cmp bl, 2
		JE skip2
		cmp bl, testGrid[esi +3]
		JNE skip2
		cmp bl, testGrid[esi+6]
		JNE skip2
		cmp bl, 3
		JE pc2
		mov edx, 1
		ret
		pc2:
		mov edx, 0
		ret


		skip2:
		inc esi
	loop L2

	mov bl, testGrid[0]
	cmp bl, 2
	JE skip3
	cmp bl, testGrid[4]
	JNE skip3
	cmp bl, testGrid[8]
	JNE skip3
	
		cmp bl, 3
		JE pc3
		mov edx, 1
		ret
		pc3:
		mov edx, 0
	ret

	skip3:

	mov bl, testGrid[2]
	cmp bl, 2
	JE skip4
	cmp bl, testGrid[4]
	JNE skip4
	cmp bl, testGrid[6]
	JNE skip4
	cmp bl, 3
		JE pc4
		mov edx, 1
		ret
		pc4:
		mov edx, 0
	ret



	skip4:
	ret
	
CheckWinnerTest ENDP


;--------------------------------------------------------------------------------------

checkpossiblewin PROC uses eax ecx ebx edi esi
	
	mov ecx, 9
	mov esi, OFFSET testGrid
	mov edi, OFFSET test2Grid
	rep movsb 
	
	mov ecx, 9
	mov esi, 0
	mov bl,0
	L1:
		cmp testGrid[esi], 2
		JNE skip
		mov testGrid[esi], 1
		call CheckWinnerTest
		cmp edx, 1
		JNE skip
		inc bl
		skip:
		mov testGrid[esi], 2
		inc esi
	loop L1

	cmp bl, 2
	JNE allow
	mov edx, 0
	ret
	allow:
	mov edx, 1
	ret
checkpossiblewin ENDP

;--------------------------------------------------------------------------------------

generateresponse PROC uses eax ecx ebx edx edi

	cmp turns, 1
	JNE next

	mov ecx, 9
	mov esi, 0
	L0:
		cmp mainGrid[esi], 2
		JNE skip0
		add esi, esi
		ret
		skip0:
		inc esi
	loop L0

	next:

	mov ecx, 9
	mov esi, 0
	mov edx, 2
	L1:
		call restoretestgrid
		cmp testGrid[esi], 2
		JNE skip
			mov testGrid[esi], 3
			call CheckWinnerTest
			cmp edx, 0
			JNE skip
			;esi has the response
			add esi, esi
			ret
		skip:
		inc esi
	loop L1

	mov ecx, 9
	mov esi, 0
	L2:
		call restoretestgrid
		cmp testGrid[esi], 2
		JNE skip2
			mov testGrid[esi], 0
			call CheckWinnerTest
			cmp edx, 1
			JNE skip2
			;esi has the response

			add esi, esi
			ret
		skip2:
		inc esi
	loop L2

	mov ecx, 9
	mov esi, 0
	L3:
		call restoretestgrid
		cmp testGrid[esi], 2
		JNE skip3
		mov testGrid[esi], 3
		
			call checkpossiblewin
			cmp edx, 1
			JNE skip3
			;esi has the response
			add esi, esi
			ret

		skip3:
		mov testGrid[esi], 2
		inc esi
	loop L3

	mov esi, 6


generateresponse ENDP



;--------------------------------------------------------------------------------------

Clear_Everything PROC

; this proc will clear all the array and coordinates

mov ecx, lengthof u_coord
mov esi,0
L1:
mov u_coord[esi],0		; clearing u_coordi
inc esi
loop L1

mov ecx, 9
mov esi,0
L2:
mov mainGrid[esi],2		; clearing u_coordi
inc esi
loop L2
ret

Clear_Everything ENDP

;--------------------------------------------------------------------------------------

;FILING PROCS

.DATA
filehandle DWORD ?
filename BYTE "MyFile.txt", 0
buffer BYTE 10 DUP(?)
str1 byte 20 dup(?)		;It would store the highscore as string
NoBytes dword 0			;number of bytes Read from file
highLen dword 0			;no of digits in highscore
str2 byte "CONGRATS! NEW HIGH SCORE ACHIEVED!",0
str3 byte "HighScore:",0

.CODE


;--------------------------------------------------------------------------------------

Read_from_file proc
	
	mov edx,OFFSET filename
	call OpenInputFile
	mov filehandle, EAX
	mov eax, filehandle 
	mov edx, OFFSET buffer ;Buffer will contain the highscore read from file
	mov ecx, 1000 
	call ReadFromFile

mov edx,offset buffer
mov NoBytes , eax		;NoBytes has number of bytes Read from file
mov eax, filehandle 
call CloseFile
ret

Read_from_file endp

;--------------------------------------------------------------------------------------

ConvertToint proc
mov esi,0
mov ecx,eax
mov eax,0
mov ebx,10
l1:
	mul ebx
	mov edx,0
	mov dl,buffer[esi]
	sub edx,48
	add eax,edx
	inc esi
loop l1

ret
ConvertToint endp

;--------------------------------------------------------------------------------------

ConvertToString proc

mov eax,highscore
mov ebx,10
mov edx,0
l1:
	inc highLen
	div ebx
	cmp eax,0
	jbe outLoop
	jmp l1

outLoop:
	
mov esi,highlen
dec esi
mov ecx,highlen
mov eax,highscore
mov edx,0
l2:
	div ebx
	add dl,48
	mov str1[esi] , dl
	mov edx,0
	dec esi
loop l2

ret
ConvertToString endp

;--------------------------------------------------------------------------------------

Write_file proc

mov edx, offset filename
call CreateOutputFile
mov filehandle,eax

call ConvertToString

mov eax, filehandle 
mov edx, OFFSET str1
mov ecx,0
mov ecx, highlen
call WriteToFile
jnc l3
	jmp l4
	
l3:
	mov eax, filehandle 
	call CloseFile
	
	mov edx,offset str1
	mov ebx, offset str2
	call MsgBox
l4:
ret

Write_file endp

;--------------------------------------------------------------------------------------

END main