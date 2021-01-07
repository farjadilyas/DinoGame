;************************************
; Muhammad Farjad Ilyas - 18I-0436  *
; Adan Abbas 		- 18I-0401  *
; Omar Mughal		- 18I-0511  *
;************************************
drawPxBox2 MACRO x1,y1,x2,y2					; DRAWS A RECTANGLE GIVEN 4 POINTS
	LOCAL a1,a2,xlow,xhigh,ylow,yhigh
	.data
		xlow DW ?
		xhigh DW ?
		ylow DW ?
		yhigh DW ?
	.code
		mov xlow,x1
		mov xhigh,x2
		mov ylow,y1
		mov yhigh,y2
		
	mov cx,xlow
	a1:
	mov dx,ylow
		a2:
			mov bh,0
			mov ax,di
			mov ah,0Ch
			int 10h
			inc dx
			cmp dx,yhigh
		jb a2
		inc cx
		cmp cx,xhigh
		jb a1
endm
drawGround MACRO x1,y1							; DRAWS THE MOVING ROCKS OF THE GROUND								
	LOCAL wingdown,wingup,birdcont
	lea si,Ground
	add si,8
	mov cx,groundSize
	drawShape x1,y1
endm

randomizer macro small,large					; RETURNS RANDOM NUMBER > SMALL && < LARGE IN AX
      .code
 
        mov ah,2ch
    		int 21H

        xor ax,ax           
     
   
        mov al,dl    ;mov microseconds in al  
        mul cx       ;Multiplying by minutes and hours   
        
        
        mov cx,large
        sub cx,small
        xor dx,dx    ;initializing dx to zero
        div cx       
        
        
        add dx,small 
        mov ax,dx   ;returns rand value in ax        
endm
;'================================='

drawBird MACRO x1,y1,flap						; DRAWS BIRD FROM BOTTOM LEFT CORNER X1,Y1
	LOCAL wingdown,wingup,birdcont				; FLAP = 0 -> WING DOWN
	lea si,Bird									; FLAP = 1 -> WING UP
	add si,8
	push ax
	mov ax,flap
	cmp ax,0
	je wingdown
	jmp wingup
	
wingdown:
	mov cx,4
	drawShape x1,y1
	add si,32
	jmp birdcont
	
wingup:
	add si,32
	mov cx,4
	drawShape x1,y1
	jmp birdcont
	
birdcont:
	pop ax
	mov cx,Birdsize
	drawShape x1,y1
endm

drawCactus MACRO x,y,indx
	LOCAL drawSmCactus,drawSmCactus1,drawSmCactus2,drawCactus1,drawCactus2,drawCactus3,drawCactus4,LP_START,END_OF_LOOP,SET_FALSE,END_DC
	jmp LP_START

drawSmCactus1:									;***************************************
	lea si,SmCactus1							;									   *
	mov cx,smsize								;	MAIN CACTUS DRAWING MACRO		   *
	drawShape x,y								;	X,Y BOTTOM LEFT CORNER			   *
	jmp END_OF_LOOP								;	INDX SELECTS CACTUS TO BE DRAWN    *
												;	INDX < 1 -> DONT DRAW  A CACTUS	   *
drawSmCactus2:									;									   *									
	lea si,SmCactus2							;***************************************
	mov cx,smsize
	drawShape x,y
	jmp END_OF_LOOP

drawSmCactus:
	lea si,SmCactus
	mov cx,smsize	
	drawShape x,y
	jmp END_OF_LOOP


drawCactus1:
	lea si,Cactus1
	mov cx,cactusize1
	drawShape x,y
	jmp END_OF_LOOP

drawCactus2:
	lea si,Cactus2
	mov cx,cactusize2
	drawShape x,y
	jmp END_OF_LOOP

drawCactus3:
	lea si,Cactus3
	mov cx,cactusize3
	drawShape x,y
	jmp END_OF_LOOP

drawCactus4:
	lea si,Cactus4
	mov cx,cactusize4
	drawShape x,y
	jmp END_OF_LOOP

LP_START:
		mov dx,[indx]
		;push indx
		cmp dx,1
		jb SET_FALSE
		cmp dx,1
		je drawCactus1
		
		cmp dx,2
		je drawCactus2

		cmp dx,3
		je drawCactus3

		cmp dx,4
		je drawCactus4

		cmp dx,5
		je drawSmCactus

		cmp dx,6
		je drawSmCactus1

		cmp dx,7
		je drawSmCactus2

		; pop indx
		; add indx,2
	SET_FALSE:							; RETURNS 0 IN DX IF CACTUS ISNT DRAWN
		mov dx,0
		jmp END_DC
	END_OF_LOOP:						; RETURNS 1 IN DX IF CACTUS IS DRAWN
		mov dx,1
		jmp END_DC
	END_DC:
ENDM

drawMoon1 MACRO x1,y1					; DRAWS MOON
	lea si,Moon1
	mov cx,moonsize1
	drawShape x1,y1
endm

drawMoon2 MACRO x1,y1
	lea si,Moon2
	mov cx,moonsize2
	drawShape x1,y1
endm

drawCloud1 MACRO x1,y1					; DRAWS CLOUD
	
	lea si,Cloud1
	add si,8
	mov di,01		;Color shade 1
	mov cx,8
	drawShape x1,y1

	mov di,05		;Color shade 2
	mov cx,6
	drawShape x1,y1	

	mov di,00 		;Color Shade 3
	mov cx,3
	drawShape x1,y1

endm

drawHeader MACRO x,y					; DRAWS THE LARGE DINO TEXT ON MAIN MENU
	lea si,HEADER
	mov cx,header_size
	drawShape x,y
endm

drawStar MACRO x,y						; DRAWS THE STARS IN NIGHT MODE
	lea si,STAR
	mov cx,star_size
	drawShape x,y
endm


drawCloud2 MACRO x1,y1	

	lea si,Cloud2
	add si,8
	mov di,01		;Color shade 1
	mov cx,9
	drawShape x1,y1

	mov di,05 		;Color Shade 2
	mov cx,7
	drawShape x1,y1 

	mov di,00 		;Color Shade 3
	mov cx,3
	drawShape x1,y1
endm

;=============================
drawSittingDino MACRO x1,y1,leg1,leg2					; DRAWS SITTING DINO
	LOCAL Ldown,Lup,Rdown,Rup,dinoeye,rightcomp

	push ax

	mov di,20
	lea si,DinoSitting
	mov cx,15
	drawShape x1,y1


	;========BENEATH
		push ax
	mov ax,leg1
	cmp ax,2
	je Ldown
	jmp Lup
rightcomp:
	mov ax,leg2
	cmp ax,2
	je Rdown
	jmp Rup
	
Ldown:
	lea si,DinoLDOWN
	mov cx,2
	drawShape x1,y1
	jmp rightcomp
Lup:
	lea si,DinoLUP
	mov cx,2
	drawShape x1,y1
	jmp rightcomp
Rdown:
	lea si,DinoRDOWN1
	mov cx,2
	drawShape x1,y1
	jmp dinoeye
Rup:
	lea si,DinoRUP1
	mov cx,2
	drawShape x1,y1
	jmp dinoeye
	
dinoeye:
	pop ax
	mov al,3
	drawPxBox x1+19,y1+3,x1+21,y1+5
endm
;============================

drawDino MACRO x1,y1,leg1,leg2							; DRAWS DINO
	LOCAL Ldown,Lup,Rdown,Rup,dinoeye,rightcomp			; IF LEG1 IS 2, LEFT LEG IS DOWN, IF 0, IT IS UP, SAME FOR LEG2 AND RIGHT LEG

	lea si,Dino
	mov cx,dinosize
	drawShape x1,y1
	push ax
	mov ax,leg1
	cmp ax,2
	je Ldown
	jmp Lup
rightcomp:
	mov ax,leg2
	cmp ax,2
	je Rdown
	jmp Rup
	
Ldown:
	lea si,DinoLDOWN
	mov cx,2
	drawShape x1,y1
	jmp rightcomp
Lup:
	lea si,DinoLUP
	mov cx,2
	drawShape x1,y1
	jmp rightcomp
Rdown:
	lea si,DinoRDOWN
	mov cx,2
	drawShape x1,y1
	jmp dinoeye
Rup:
	lea si,DinoRUP
	mov cx,2
	drawShape x1,y1
	jmp dinoeye
	
dinoeye:
	pop ax
	;mov al,3
	;drawPxBox x1+13,y1-7,x1+15,y1-5
endm


drawShape MACRO x,y								; MAIN SHAPE DRAWING FUNCTION
LOCAL setthis,this_x,this_y						; X,Y BOTTOM LEFT CORNER
.data											; SI CONTAINS OFFSET OF COORDINATE ARRAY
	this_x DW ?									
	this_y DW ?
.code
	mov ax,x
	mov this_x,ax
	mov ax,y
	mov this_y,ax								; CX ALREADY CONTAINS NO. OF ROWS IN SHAPE MATRIX
setthis:
		push cx
		mov ax,this_x
		add ax,	WORD PTR [si]					; ADDS ARRAY COORDINATES TO BOTTOM LEFT CORNER TO..
		add si,2								; DRAW RECTANGLES ONE BY ONE
		mov bx,this_y
		add bx,WORD PTR [si]
		add si,2
		mov cx,this_x
		add cx,WORD PTR [si]
		add si,2
		mov dx,this_y
		add dx,WORD PTR [si]					; STORES 4 CORNERS OF EACH RECTANGLE IN AX BX CX DX
		add si,2								; CALLS MAIN RECTANGLE DRAWING FUNCTION
		drawPxBox2 ax,bx,cx,dx
		pop cx
	loop setthis
endm

moveToAXBXCXDX MACRO z1,z2,z3,z4
	mov ax,z1
	mov bx,z2
	mov cx,z3
	mov dx,z4
endm

pushAllReg MACRO
	push ax
	push bx
	push cx
	push dx
endm

popAllReg MACRO
	pop dx
	pop cx
	pop bx
	pop ax
endm


;=====================================HERE End============================================;


mousein MACRO
mov bx,0
	mov dx,ycoord
	mov ax,dx
	mov bl,8
	div bl
	mov dx,0
	mov dl,al
	mov ycoord,dx		; Divides x and y values by 8 to get character positions/ coordinates
	mov ax,xcoord
	div bl
	mov cx,0
	mov cl,al
	; mov ax,xcoord
	; CALL display
	; mov ax,ycoord
	; CALL display
endm
pointer1 MACRO			; For printing pointer for option 1 in MAIN MENU
	mov dh,10
	mov dl,0
	mov ch,6
	mov cl,0
	CALL drawbox 
	mov dh,10
	mov dl,10
	mov ch,6
	mov cl,10
	CALL drawbox 
	mov dh,6
	mov dl,10
	mov ch,6
	mov cl,0
	CALL drawbox 
	mov dh,10
	mov dl,10
	mov ch,10
	mov cl,0
	CALL drawbox 
endm
pointer2 MACRO			; For printing pointer for option 2 in MAIN MENU
	mov dh,10
	mov dl,11
	mov ch,6
	mov cl,11
	CALL drawbox
	mov dh,10
	mov dl,26
	mov ch,6
	mov cl,26
	CALL drawbox
	mov dh,10
	mov dl,26
	mov ch,10
	mov cl,11
	CALL drawbox
	mov dh,6
	mov dl,26
	mov ch,6
	mov cl,11
	CALL drawbox
endm
pointer3 MACRO			; For printing pointer for option 3 in MAIN MENU
	mov dh,10
	mov dl,27
	mov ch,6
	mov cl,27
	CALL drawbox
	mov dh,10
	mov dl,39
	mov ch,6
	mov cl,39
	CALL drawbox
	mov dh,10
	mov dl,39
	mov ch,10
	mov cl,27
	CALL drawbox
	mov dh,6
	mov dl,39
	mov ch,6
	mov cl,27
	CALL drawbox
endm

TIMEOFDAY MACRO MODE,DAY_LIM,NIGHT_LIM					; SETS DAY/NIGHT MODE, CHECKS TIME LIMIT FOR EACH, SWITCHES IF NECESSARY

LOCAL SET_DARK_MODE,SET_LIGHT_MODE,RESET_MODE,END_TOD
	inc MODE
	
	cmp MODE,DAY_LIM
	jbe SET_DARK_MODE
	cmp MODE,DAY_LIM+NIGHT_LIM
	jbe SET_LIGHT_MODE
	jmp RESET_MODE
SET_LIGHT_MODE:						; SETS COLORS FOR DAY
	mov bird_color,19
	mov dino_color,19
	mov cactus_color,19
	mov ground_color,0
	mov back_accent,4fh
	jmp END_TOD
	
SET_DARK_MODE:						; SETS COLORS FOR NIGHT
	mov bird_color,115
	mov dino_color,5
	mov cactus_color,6
	mov ground_color,0
	mov back_accent,0
	jmp END_TOD
RESET_MODE:
	mov MODE,0
	jmp SET_DARK_MODE

END_TOD:
ENDM
 
.model small
.stack 100h
.data
	instr_file db "myfile.txt",0
	instructions db 500 DUP('$')
	handle DW ?
	msg1 db " PLAY "
	msg2 db " INSTRUCTIONS "
	msg3 db " EXIT "
	HI db " HI 05472 00000"
	hearts db 3,3,3,3,3
	xcoord DW ?
	ycoord DW ? 
	page_no DW ?
	var DW 0
	
	;~~~~~~~~~~~~~~~~~~~~~~~~~~~~ START OF ALL COORDINATE MATRICES ~~~~~~~~~~~~~~~~~~~~~~~
	smsize DW 11
	SmCactus DW 0,-14,1,-9,
				1,-15,2,-8,
				2,-14,3,-7
				DW 3,-9,5,-7,
				5,-19,6,0,
				6,-20,8,0
				DW 8,-19,9,0,
				9,-12,11,-10,
				11,-17,12,-10
				DW 12,-18,13,-11,
				13,-17,14,-12
	
	SmCactus1 DW 0,-17,1,-12,
				1,-18,3,-11,
				3,-17,4,-10,
				4,-12,5,-10
				DW 5,-19,6,0,
				6,-20,8,0,
				8,-19,9,0,
				9,-9,11,-7
				DW 11,-14,12,-7,
				12,-15,13,-8,
				13,-14,14,-9
				
	SmCactus2 DW 0,-17,1,-12,
				1,-18,3,-11,
				3,-17,4,-10,
				4,-12,5,-10
				DW 5,-19,6,0,
				6,-20,8,0,
				8,-19,9,0,
				9,-9,11,-7
				DW 11,-14,12,-7,
				12,-15,13,-8,
				13,-14,14,-9
				
	;+++++++++++++++++++++++++++++++++++++++++++++++

	Cloudpos1 	DW	200

	Cloud1 		DW	-18,0,26,10 ,
				0,0,8,1,
				 -1,1,9,2,
				 -2,2,10,3,
				 14,2,18,3
				 

				DW -2,3,20,4,
					-12,3,-10,4,
				   -14,4,-8,5,
				   -6,4,23,5,
			 	    2,2,6,3,
  				    0,3,12,4

  		 		DW 14,3,16,4,
  				   -2,4,18,5,
  				   -12,4,-10,5,
  				   -14,5,22,6,
  				   -16,5,-14,6

  				DW  22,5,24,6,
  				    -18,6,26,7+2

  	Cloudpos2  	DW 	50


  	Cloud2 		DW 	-14,-5,32,10,	
				0,0,6,1,
  				-2,1,8,2,
  				-4,2,10,3,  				
  				-4,3,10,4,
  				-8,3,-6,4

  			DW 	12,3,14,4,
  				-10,4,16,5,
  				-10,5,-6,6,
  				-10,6,-8,7

  			DW 	2,2,4,3,
  				-2,3,6,4,
  				-2,4,8,5,
  				12,4,14,5,
  				22,4,24,5

  			DW 	-8,5,28,6,
  				-9,6,30,7

  			DW  	-12,6,-10,7,
  				 26,6,30,7,
  				-14,7,32,8+2


  	FullSunPos      DW 	150

  	FullSun		DW  	0,0,1,15,
  				1,-1,2,16,
  				2,-2,3,17,
  				3,-3,4,18,
  				4,-4,5,19

  			DW 	5,-5,6,20,
  				6,-6,7,21,
  				7,-7,8,22,
  				8,-8,9,23,
  				9,-9,10,24

  			DW 	10,-9,20,24

  			DW 	20,-8,21,23,
  				21,-7,22,22,
  				22,-6,23,21,
  				23,-5,24,20,
  				24,-4,25,19

  			DW 	25,-3,26,18,
  				26,-2,27,17,
				27,-1,28,16,
				28,0,29,15
 
	DinoSitting	DW 	0,0,1,7,
				1,3,2,8,
				2,4,3,9,
				3,5,4,9,
				4,5,5,11


	
			DW	13+5,9+1,19+5,10+1,
				15+5,7+1,16+5,10+1,
				13+5,1+1,15+5,10+1,
				15+5,1+1,20+5,8+1,
				20+5,2+1,21+5,8+1

			DW 	4,5,15,11,
				6,4,13,11, 
				15,6,18,10, 
				14,5,18,7,
				17,3,18,7


;++++++++++++++++++++++++++++++++++++++++++++++++
				
	cactusize1 DW 13
	cactusize2 DW 13
	cactusize3 DW 12
	cactusize4 DW 12
	
	Cactus1	DW	8,-29,9,0,
				9,-30,13,0,
				13,-29,14,0,
				14,-13,18,-9
				DW 4,-13,8,-9,
				1,-20,2,-12,
				2,-21,3,-11,
				3,-21,4,-10,
				4,-20,5,-10
				DW 20,-20,21,-12,
				19,-21,20,-11,
				18,-21,19,-10,
				17,-20,18,-10
				
	Cactus2 DW  8,-29,+9,0,
				9,-30,+13,0,
				13,-29,+14,0,
				14,-13,+18,-9
				DW 4,-19,+8,-16,
				1,-26,+2,-19,
				2,-27,+3,-18,
				3,-27,+4,-17
				DW 4,-26,+5,-16,
				20,-20,+21,-12,
				19,-21,+20,-11
				DW 18,-21,+19,-10,
				17,-20,+18,-10

	Cactus3 DW	8,-29,9,0,
				9,-30,13,0,
				13,-29,14,0,
				14,-12,16,-9
				DW 4,-13,8,-9,
				1,-20,2,-12,
				2,-21,3,-11,
				3,-21,4,-10,
				4,-20,5,-10
				DW 18,-20,19,-12,
				17,-21,18,-11,
				16,-20,17,-10

	Cactus4 DW	8,-29,9,0,
				9,-30,11,0,
				11,-29,12,0,
				12,-13,14,-9,
				6,-15,8,-13
				DW 3,-24,4,-15,
				4,-25,5,-14,
				5,-25,6,-13,
				6,-24,7,-13
				DW 16,-27,17,-12,
				15,-28,16,-11,
				14,-27,15,-10
	
	dinosize DW 19
	dinoclear DW 0,-9,22,17
	Dino 	DW	0,0,1,7,
				1,3,2,8,
				2,4,3,9,
				3,5,4,9,
				4,5,5,11
	
			DW	6,4,7,13,
				7,3,8,12,
				8,2,9,11,
				9,1,10,12
				 
			DW	11,-8,12,10,
				12,-8,13,9,
				13,-1,14,1

			DW	13,-0,19,1,
				15,-2,16,1,
				13,-8,15,8,
				15,-8,20,-1,
				20,-7,21,-1

			DW	13,3,18,5,
				17,4,18,5
				
DinoLDOWN	DW	6,15,8,16,
				5,5,7,16
				
DinoLUP		DW	6,13,8,14,
				5,5,7,14
				
DinoRDOWN	DW	10,-7,12,16,
				11,15,13,16
				
DinoRUP		DW	10,-7,12,14,
				11,13,13,14
				
DinoRDOWN1		DW	10,4,12,16,
				11,15,13,16
				
DinoRUP1		DW	10,4,12,14,
				11,13,13,14
				
Birdsize	DW 	14
				
Bird		DW	0,-20,26,0,		; for clearing
				10,-11,11,0,	; 4 lines for wing up
				11,-11,13,-1,
				13,-11,15,-3,
				15,-11,17,-4
				
			DW	10,-11-9,11,-8,  ; 4 lines for wingdown
				11,-11-8,13,-7,
				13,-11-7,15,-6,
				15,-11-6,17,-5,
				8,-14,9,-9			; common body starts
				
			DW	9,-11,10,-8,				
				17,-11,19,-5,
				19,-10,20,-5,
				20,-9,22,-5,
				22,-7,25,-6
				
			DW	22,-8,24,-7,
				22,-9,26,-8,				
				0,-12,8,-10,
				1,-13,8,-11,
				2,-14,8,-12
				
			DW	3,-15,8,-13,
				4,-16,8,-13,
				5,-17,8,-14

groundSize  DW	41
			
Ground		DW	0,-5,299,-2,
				0,-5,3,-4,
				4,-3,7,-2,
				9,-5,10,-4,
				23,-3,24,-2
				
			DW	26,-3,29,-2,
				31,-5,32,-4,
				41,-4,43,-3,
				46,-5,47,-4,
				49,-3,52,-2
				
			DW	56,-3,57,-2,
				60,-5,63,-4,
				69,-3,70,-2,
				80,-5,81,-4,
				85,-5,86,-4
				
			DW	96,-3,99,-2,
				104,-5,105,-4,
				109,-5,110,-4,
				113,-3,114,-2,
				118,-5,119,-4
				
			DW	130,-5,133,-4,
				138,-3,140,-2,
				143,-3,144,-2,
				150,-5,151,-4,
				155,-4,156,-3
				
			DW	159,-3,160,-2,
				170,-4,171,-3,
				178,-4,181,-3,
				188,-3,189,-2,
				197,-3,200,-2
				
			DW	210,-5,211,-4,
				215,-3,216,-2,
				224,-5,227,-4,
				233,-5,234,-4,
				240,-5,243,-4
				
			DW	250,-3,251,-2,
				260,-5,263,-4,
				268,-4,269,-3,
				275,-3,278,-2,
				288,-5,291,-4
				
			DW	294,-5,295,-4,
				296,-4,299,-3
	
moonsize2	DW	21
moonsize1	DW	13

Moon2 		DW	12,-30,18,0,
				10,-29,12,-1,
				8,-28,10,-2,
				7,-27,8,-3,
				6,-26,7,-4
				
			DW	5,-25,6,-5,
				4,-24,5,-6,
				3,-23,4,-7,
				2,-22,3,-8,
				1,-21,2,-9
				
			DW	0,-18,1,-12,
				18,-29,20,-1,
				20,-28,22,-2,
				22,-27,23,-3,
				23,-26,24,-4
				
			DW	24,-25,25,-5,
				25,-24,26,-6,
				26,-23,27,-7
				
			DW	27,-22,28,-8,
				28,-21,29,-9,
				29,-18,30,-12
	
	Moon1	DW	0,-30,4,-29,
				2,-29,6,-28,
				4,-28,8,-27,
				6,-27,10,-25,
				8,-25,12,-23
				
			DW	10,-23,14,-21,
				11,-21,15,-13,
				10,-13,14,-11,
				8,-11,12,-9,
				6,-9,10,-7
				
			DW	4,-7,8,-6,
				2,-6,6,-5,
				0,-5,4,-4
				
	HEADER 	DW 	0,-35,4,0,
				0,-3,26,0,
				0,-35,26,-32,
				20,-6,36,-3,
				24,-9,36,-6
			DW	28,-18,36,-9,
				20,-32,26,-29,
				24,-29,36,-26,
				28,-26,36,-18,
				2,-3,30,3,
				12,-25,18,-9,
				18,-20,22,-14
				
			DW	33,-3,52,2,
				33,-35,36,-29,
				33,-35,52,-32,
				45,-32,57,-3,
				53,-35,68,-32
				
			DW	65,-32,74,-29,
				68,-29,74,-26,
				71,-26,74,-23,
				71,-35,84,-32,
				74,-3,88,2,
				53,-3,71,2,
				65,-20,68,-3
				
			DW	68,-16,71,-3,
				71,-12,74,-3,
				81,-32,88,-3,
				88,-35,91,-29,
				88,-6,91,0,
				111,-6,114,0
				
			DW	91,-3,117,2,
				114,-9,120,-6,
				114,-29,120,-3,
				111,-32,117,-29,
				91,-35,114,-32, ;34
				95,-26,106,-9
						
	header_size DW 36
	
	STAR	DW	0,0,6,0,
				0,1,4,1,
				0,-1,4,-1,
				0,-5,0,-1,
				1,-3,1,-1,
				-1,-3,-1,-1 
				
			DW	0,1,0,5,	
				-1,0,-1,3,
				1,0,1,3,				
				-5,0,-1,0,	
				-3,1,0,1,
				-3,-1,0,-1
	star_size DW 12
				
	
	;~~~~~~~~~~~~~~~~~~~~~~ DINO VARIABLES ~~~~~~~~~~~~~~~~~~~~~~~~ 
	traj 	DW  -16,-13,-5,-2,-1,1,2,5,13,16
	trajL	DW	0
	dino_L DW 00000000b
	dino_R DW 00000010b
	dino_H DW 147
	
	;~~~~~~~~~~~~~~~~~~~~~~~
	
	BirdW DW 00000000b 
	birdPosX DW 250
	birdPosY DW 100
	
	cactus_select DW 1,4,6
	cactus_pos DW 260,280,300
	
	g_val DW 299
	
	;~~~~~~~~~~~~~~~~~ GAME CONTROL VARIABLES ~~~~~~~~~~~~~~~~~~~~~
	SCORE DW 0
	LIVES DW 5
	
	;~~~~~~~~~~~~~~~~~~~~~ COLOR VARIABLES ~~~~~~~~~~~~~~~~~~~~~~~~
	DARK_MODE DW 0
	back_accent DB ?
	dino_color DW ?
	ground_color DW ?
	bird_color DW ?
	cactus_color DW ?
.code
main proc
	mov ax,@data
	mov ds,ax
	mov ax,0
	
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 					                                  ~
;                  Display Set Up                     ~
;					                                  ~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	mov ax,013h
	int 10h
	
	mov ah,05h			; set display page
	mov al,0
	int 10h
	
	mov ax,1003h		; turn off blinking / enable bright colors
	mov bx,0
	int 10h
	
menu_start:

	mov DARK_MODE,1
	TIMEOFDAY DARK_MODE,30,30
	mov bh,back_accent						; set background color OF GAME BOX
	mov dh,25
	mov dl,40
	mov ch,0
	mov cl,0 
	CALL drawbox
	mov bh,0h								; set background color TOP BLACK SRIP
	mov dh,2
	mov dl,40
	mov ch,0
	mov cl,0 
	CALL drawbox
	mov bh,90h								; set background color TGROUND
	mov dh,25
	mov dl,40
	mov ch,20
	mov cl,0 
	CALL drawbox
	
	
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 					                                    ~
;	                   Main Menu                        ~
;					                                    ~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	mov dl, 3
	mov dh, 8
	lea si,msg1
	mov cx,5
	mov bl,31				; prints option 1
	CALL printstr	
	
	mov dl, 13
	mov dh, 8                  
	lea si,msg2
	mov cx,	12
	mov bl,31				; prints option 2
	CALL printstr
	
	mov dl, 32
	mov dh, 8                 
	lea si,msg3
	mov cx,5
	mov bl,31				;prints option 3
	CALL printstr

	mov di,31
	drawHeader 95,40 
	mov di,31						;*********************************
	drawStar 50,20					;								 *
	drawStar 20,30					;	DRAW SPRITES FOR MAIN Menu	 *
	drawStar 260,20					;								 *
	drawStar 290,30					;*********************************
	mov cx,0
	mov dx,160
	mov bx,320
	mov ax,ground_color
	CALL drawHorizontalLine
	mov di,ground_color
	drawGround g_val,167
	mov di,dino_color
	drawDino 20,dino_H,2,2
	mov di,cactus_color
	drawCactus 250,165,1
	drawCactus 225,165,4
    
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 					                                              ~
;                        Menu Mouse Operations                    ~
;					                                              ~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	mov var,0
	mov ax,0		; Check if mouse is supported - either cont with game or quit
	int 33h
	mov bx,ax
	mov ax,1		; Turn on cursor display
	int 33h
	cmp bx,0FFFFh
	jne endprog		; Check if mouse is available & end if necessary
	jmp lpant
	
coordcomp: 			; Checks mouse position & sets appropriate pointer for option
    mousein
	 mov xcoord,cx
	 cmp ycoord,14
	 jae join2
	 cmp ycoord,2
	 jbe join2
     cmp xcoord,18
     jbe op1
     cmp xcoord,55
     jbe op2
     jmp op3

op1:									;-------------------------------
	mov bh,31							;	DRAWS POINTER FOR OPTION 1
	pointer1							;===============================
	mov bh,0
	pointer2
	pointer3 
	mov page_no,1
	jmp join2 
	
op2:									;------------------------------
	mov bh,31							;	DRAW POINTER FOR OPTION 2
	pointer2 							;==============================
	mov bh,0
	pointer1 
	pointer3 
	mov page_no,2
	jmp join2
	
op3:									;-------------------------------
	mov bh,31							;	DRAW POINTER FOR OPTION 3
	pointer3							;===============================
	mov bh,0
	pointer1
	pointer2
	mov page_no,3
	jmp join2
	
join1:						; Clears previously highlighted pointers for all options if mouse isn't in range
	mov bh,0
	pointer1
	pointer2
	pointer3
	jmp join2
lpant:	 					; Update option pointers, check for press
	
	lp1: 
		mov ax,03h
		int 33h 
		push bx
		mov xcoord,cx		;Save cursor position and status
		mov ycoord,dx  
		jmp coordcomp 
	join2:
	    pop bx
		cmp bx,0
		jz lp1
		jmp check_press
	
check_press:  
	mousein
	cmp page_no,2
	je page2
	cmp page_no,3
	je endprog
	jmp page1
	
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 			                                     		   ~
;	                    GAME PAGE                          ~
;					                                       ~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
page1:

mov ax,013h		; set graphics mode
int 10h

jmp gmlp

dinojmp:								; LOOPS THROUGH DINO JUMP TRAJECTORY
	cmp al,27
	je endprog
	lea si,traj
	mov bx,trajL
	add bx,trajL
	mov ax,[si+bx]
	add dino_H,ax
	inc trajL
	jmp back1p2

resettraj:								; END OF DINO JUMP
	MOV AH,00H
	INT 16H								; CLEARS KEYBOARD BUFFER
	mov trajL,0
	mov dino_R,0
	jmp back1p1
	
;*********************************************************************
;																	 *
;								GAME LOOP							 *
;																	 *
;*********************************************************************

gmlp:
	
	TIMEOFDAY DARK_MODE,30,30				; Increments counter, sets day/night
back0p0:
	mov bh,back_accent						; set background color OF GAME BOX
	mov dh,25
	mov dl,40
	mov ch,0
	mov cl,0 
	CALL drawbox
	mov bh,0h								; set background color TOP BLACK SRIP
	mov dh,2
	mov dl,40
	mov ch,0
	mov cl,0 
	CALL drawbox
	mov bh,90h								; set background color TGROUND
	mov dh,25
	mov dl,40
	mov ch,20
	mov cl,0 
	CALL drawbox
	
	
	lea si,HI
	mov ax,SCORE							; WRITE SCORE TO STRING
	CALL scoreup
	
	mov dl, 22
	mov dh, 1                  
	lea si,HI
	mov cx,lengthof HI						; PRINTS SCORE
	;mov bx, 0FF03h	
	mov bl,31
	CALL printstr
	
	.IF (DARK_MODE < 30)
		mov di,31
		drawStar 100,55							; STAR DRAWING
		drawStar 50,30
		drawStar 220,45
		drawStar 270,38
		
		mov di,31
		drawMoon1 190,70						; MOON DRAWING
	.ENDIF
	
	mov dl, 2
	mov dh, 1                  
	lea si,hearts
	mov cx,LIVES							; HEART DRAWING
	mov bl,31
	CALL printstr
	
	;======================					; CLOUD DRAWING
	drawCloud1 Cloudpos1,50
	sub Cloudpos1,1

	drawCloud2 Cloudpos2,50
	sub Cloudpos2,1

	;drawSittingDino 20,100,dino_L,dino_R
	;====================
	mov di,ground_color						;************************
	drawGround g_val,167					;						*
											;						*
	sub g_val,10							;						*
	mov cx,0								;	 GROUND DRAWING		*
	mov dx,160								;						*
	mov bx,320								;						*
	mov ax,ground_color						;						*
	CALL drawHorizontalLine					;************************
	
	mov di,dino_color
	drawDino 20,dino_H,dino_L,dino_R		;***********************
	mov ah,01h								;					   *
	int 16h									;	KEYBOARD INPUT	   *
	jnz dinojmp								;					   *
	jmp back1p2								;***********************
	
back1p2:
	cmp trajL,lengthof traj					;----------------------
	je resettraj							; DINO JUMP CONTROLS
	jmp back1p1								;======================
	
back1p1:

	.if (birdPosX >= 1 && birdPosX<=300)	;---------------------------
		mov di,bird_color					;	DRAWS BIRD IF IN RANGE
		drawBird birdPosX,birdPosY,BirdW	;===========================
		sub birdPosX,10
	.endif


	.if ( birdPosX>=300)					;-------------------------------------------------------
		sub birdPosX,10						; Decrements birposX until in range, effective a timer
	.endif									;
											;=========================================================

	mov di,cactus_color
	lea bx,cactus_select
	lea si, cactus_pos
	.IF (WORD PTR [si] < 10)				;---------------------------------
		randomizer 1,7						;	RESETS FIRST CACTUS POSITION
		mov [bx],ax							;=================================
		mov WORD PTR [si],280
	.ENDIF
	
	mov ax,[si]
	sub WORD PTR [si],20					; Decreases cactus #1's pos
	drawCactus ax,165,bx
	
	.IF (dx == 1)							;----------------------------------------------------------
		mov di,cactus_color					;	IF cactus #1 has spawned, check if #2 will spawn
		lea bx,cactus_select				;==========================================================
		add bx,2
		lea si,cactus_pos
		add si,2
		
		.IF (WORD PTR [si] < 10)			;-------------------------------
			randomizer -3,7					;	RESERTS #2 CACTUS POSITION
			mov [bx],ax						;===============================
			mov WORD PTR [si],280
		.ENDIF
		mov ax,[si]
		sub WORD PTR [si],20
		drawCactus ax,165,bx
		
		.IF (dx == 1)
			mov di,cactus_color				; IF #2 CACTUS SPAWNED, SEE IF #3 SPAWNS
			lea bx,cactus_select
			add bx,4
			lea si,cactus_pos
			add si,4
			
			.IF (WORD PTR [si] < 10)		; RESETS #3 CACTUS POSITION
				randomizer -7,7
				mov [bx],ax
				mov WORD PTR [si],280
			.ENDIF
			mov ax,[si]
			sub WORD PTR [si],20
			drawCactus ax,165,bx
		.ENDIF
	.ENDIF
	
	cmp dino_H,147
	je switch_dinolegs
	jmp reset_dinolegs
	
switch_dinolegs:
	xor dino_L,00000010b					;-----------------------
	xor dino_R,00000010b					;	DINO LEGS SWITCHING
	jmp togglewings							;=======================
	
reset_dinolegs:								;-----------------------
	mov dino_L,2							;	DINO LEGS DURING JUMP
	mov dino_R,2							;=======================
	
togglewings:								;-----------------------
	xor BirdW,00000001b						;	BIRD WING FLAPPING
											;=======================
	cmp g_val,10
	jbe reset_gval
	
	
reset_birdpos:
	jmp endloop
	
reset_gval:									; RESETS GROUND X AXIS for moving rocks
	mov g_val,290
	jmp endloop
	
endloop:
	mov bx,40
	CALL FRAME_CONTROL						; WAIT BETWEEN FRAMES
	inc SCORE								; INCREMENT SCORE
	
	
	cmp birdPosX,10
	jle setBirdPos
	
jmp gmlp

setBirdPos:									; Resets bird position
	randomizer 500,1000
	mov birdPosX,ax
	xor ax,ax
	randomizer 110,150
	mov birdPosY,ax

jmp gmlp


mov ah,01h
int 21h


jmp endprog

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; 			                                   ~
;	               INSTRUCTIONS PAGE                   ~
;					                   ~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

page2:
	mov ah,00h
	mov al,03h
	int 10h
	
	mov ax,1003h		; turn off blinking / enable bright colors
	mov bx,0
	int 10h
	
	mov bh,00001111b
	mov dh,25
	mov dl,80
	mov ch,0
	mov cl,0
	CALL drawbox				; Reset Instruction Page background
	
	mov bh,11001111b
	mov dh,20
	mov dl,75
	mov ch,4
	mov cl,5
	CALL drawbox				; Reset Instruction Page background
	
	mov bh,00001111b
	mov dh,25
	mov dl,5
	mov ch,0
	mov cl,0
	CALL drawbox				; Reset Instruction Page background
	
	mov al,2
	lea dx,instr_file			; Open "Instructions.txt"
	CALL openfile
	mov handle,ax
	
	mov cx,255
	mov bx,handle
	lea dx,instr_file
	mov si,dx
	CALL printfile				; Print file contents on screen
	
	mov bx,handle
	CALL closefile				; Close file
	mov ah,01h
	int 21h
	mov ax,13h
	int 10h
	jmp menu_start

endprog:
	mov ah,04ch
	int 21h
main endp
;=============================Main End================================;



drawHorizontalLine proc uses ax bx cx dx 	  ; Takes cx as X-coordiante , dx as Y-coordinate , bx as length , al as color



    	add bx,cx
    	push bx
	mov bh, 0      ; DisplayPage
	pop bx
    a1:
	mov ah, 0Ch  ; AH=0Ch is BIOS.WritePixel
	int 10h
	inc cx
	cmp cx,bx
	jne a1

ret
drawHorizontalLine endp

drawbox proc			; Box drawing proc -- not using pixels
	mov ah,06h
	mov al,0
	int 10h
	ret
drawbox endp


openfile proc			;open file handles, stores it in ax
	mov ah,3dh
	int 21h
	ret
openfile endp

printfile proc			; prints file contents
	mov ah,3fh
	int 21h
	
	mov dl, 25
	mov dh, 7                  
	;mov cx,108
	mov bl, 11001111b
	CALL printstr

	RET
printfile endp

printstr proc			; parameters: si -- Prints any string on screen
	push es
	mov al,1
	mov bh, 0
	push ds
	pop es
	mov bp, si
	mov ah, 13h
	int 10h
	pop es
	ret
printstr endp

closefile proc			; parameter: bx - filehandle
	mov ah,3Eh
	int 21h
	ret
closefile endp

FRAME_CONTROL proc		; Waits between frames
	push cx
	mov cx,65000
	bekaarlp:
		push cx
		mov cx,bx
			bk2:
			nop
			loop bk2
		pop cx
	loop bekaarlp
	pop cx
	ret
FRAME_CONTROL endp

scoreup proc						; PROCEDURE TO WRITE SCORE TO STRING FOR DISPLAYING
mov di,0
add si,14
	
   MOV BX, 10      ;Initializes divisor
   MOV DX, 0000H   ;Clears DX
   MOV CX, 0000H   ;Clears CX
    
;_____________________________	
;Splitting process starts here
;=============================
L1:  
   MOV DX, 0000H    ;Clears DX during jump
   div BX           ;Divides AX by BX
   ;PUSH DX          ;Pushes DX(remainder) to stack
   mov BYTE PTR [si],dl
   add BYTE PTR [si],30h
   dec si
   INC CX           ;Increments counter to track the number of digits
   CMP AX, 0        ;Checks if there is still something in AX to divide
   JNE L1           ;Jumps if AX is not zero
   RET             ;returns control
scoreup ENDP
end main