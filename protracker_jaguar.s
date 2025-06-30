; https://github.com/djh0ffman/ProTrackerReplay

; replay protracket natif Jaguar
;
; OK - structure + affichage texte
; OK - nouveau moteur Paula DSP : test bouton => play sample
			; on écrit dans Paula_custom, qui est ensuite copié dans buffers_paula_asynchrones ( 8 buffers asynchrones pour gérer le replay en 60hz en vbl, et la musique à jouer en 50 hz) // copie_Paula_to_data_stack
; - replay protracker en vbl
; - gérer le dmacon
; - gestion bpm : ligne 1240
; - convertir le replay en timer 1 DSP


; 3 etape:
	; - play normal
	; - dmacon, remplissage interne / forcer sample + maj volumes
	; - remplis location+length pour bouclages

    ; sub.l      a0,a0                ; vector base, go find it yourself
    ; moveq      #0,d1                ; 0 = pal / 1 = ntsc
    ; bsr        CIA_Install          ; install cia interrupts
    ; lea        mod,a0               ; module
    ; bsr        mt_init              ; init module
    ; st         mt_Enable            ; enable playback


flag_display_infos=1
mixage=1
lecture_valeurs_paula=1

channel_1		.equ		1
channel_2		.equ		1
channel_3		.equ		1
channel_4		.equ		1

volume_music=256				; 0-256
volume_SFX=80					; 0-256

;-------------------------
;CC (Carry Clear) = %00100
;CS (Carry Set)   = %01000
;EQ (Equal)       = %00010
;MI (Minus)       = %11000
;NE (Not Equal)   = %00001
;PL (Plus)        = %10100
;HI (Higher)      = %00101
;T (True)         = %00000
;-------------------------

COULEUR_MARQUEUR_vert_vif=$FF000000
COULEUR_MARQUEUR_rouge_vif=$00FF0000
COULEUR_MARQUEUR_bleu_vif=$000000FF
COULEUR_MARQUEUR_cyan=$FF0000FF
COULEUR_MARQUEUR_violet=$00FF00FF
COULEUR_MARQUEUR_jaune=$FFFF0000
COULEUR_MARQUEUR_blanc=$FFFF00FF

	include	"jaguar.inc"
CLEAR_BSS			.equ			1									; 1=efface toute la BSS jusqu'a la fin de la ram utilisée


; ----------------------------
; parametres affichage
;ob_liste_originale			equ		(ENDRAM-$4000)							; address of list (shadow)
ob_list_courante			equ		((ENDRAM-$4000)+$2000)				; address of read list
nb_octets_par_ligne			equ		320
nb_lignes					equ		256

curseur_Y_min		.equ		8




DEFAULT_BPM       = 125                                                          ; well, I found this useful.

n_note          EQU 0                                                            ; W
n_cmd           EQU 2                                                            ; W
n_cmdlo         EQU 3                                                            ; B
n_start         EQU 4                                                            ; L
n_length        EQU 8                                                            ; W
n_loopstart     EQU 10                                                           ; L
n_replen        EQU 14                                                           ; W
n_period        EQU 16                                                           ; W
n_finetune      EQU 18                                                           ; B
n_volume        EQU 19                                                           ; B
n_dmabit        EQU 20                                                           ; W
n_toneportdirec EQU 22                                                           ; B
n_toneportspeed EQU 23                                                           ; B
n_wantedperiod  EQU 24                                                           ; W
n_vibratocmd    EQU 26                                                           ; B
n_vibratopos    EQU 27                                                           ; B
n_tremolocmd    EQU 28                                                           ; B
n_tremolopos    EQU 29                                                           ; B
n_wavecontrol   EQU 30                                                           ; B
n_glissfunk     EQU 31                                                           ; B
n_sampleoffset  EQU 32                                                           ; B
n_pattpos       EQU 33                                                           ; B
n_loopcount     EQU 34                                                           ; B
n_funkoffset    EQU 35                                                           ; B
n_wavestart     EQU 36                                                           ; L
n_reallength    EQU 40                                                           ; W
n_realvolume    EQU 44                                                           ; W

;                   include    "protracker_macro.asm"



.opt "~Oall"

.text

			.68000

relaunch_all:
	move.l		#$70007,G_END
	move.l		#$70007,D_END
	move.l		#INITSTACK-128, sp	

; routines de debug
	lea			bus_error_68000,a0
	move.l		a0,$0008.w
	move.l		#address_error_68000,$C.w

; init curseur / relaunch test
	move.w		#25,couleur_char
	move.w		#0,curseur_x
	move.w		#curseur_Y_min,curseur_y
; init dsp volumes for relaunch test
	move.l		#volume_SFX,DSP_volume_SFX
	move.l		#volume_music,DSP_volume_music


	
	move.w		#%0000011011000111, VMODE			; 320x256
	
	move.w		#$100,JOYSTICK


	move.w		#801,VI			; stop VI

; clear BSS
	lea			DEBUT_BSS,a0
	lea			FIN_RAM,a1
	moveq		#0,d0
	
boucle_clean_BSS:
	move.b		d0,(a0)+
	cmp.l		a0,a1
	bne.s		boucle_clean_BSS
; clear stack
	lea			INITSTACK-100,a0
	lea			INITSTACK,a1
	moveq		#0,d0
	
boucle_clean_BSS2:
	move.b		d0,(a0)+
	cmp.l		a0,a1
	bne.s		boucle_clean_BSS2

    bsr     InitVideo               	; Setup our video registers.

	jsr     copy_olist              	; use Blitter to update active list from shadow

	move.l	#ob_list_courante,d0					; set the object list pointer
	swap	d0
	move.l	d0,OLP

	lea		CLUT,a2
	move.l	#255-2,d7
	moveq	#0,d0
	
copie_couleurs:
	move.w	d0,(a2)+
	addq.l	#5,d0
	dbf		d7,copie_couleurs

	lea		CLUT+2,a2
	move.w	#$F00F,(a2)+


; CLS
	;moveq	#0,d0
	;bsr		print_caractere

; init DSP

	lea		chaine_HIPPEL,a0
	bsr		print_string
; ligne suivant
	moveq	#10,d0
	bsr		print_caractere
	
    bsr        CIA_Install          ; install cia interrupts
    lea        module_amiga,a0               ; module
    bsr        mt_init              ; init module

	
	
	jsr				PAULA_init
 
	
	
	
	
	move.w		#0,compteur_frame_music

	move.w		#85,couleur_char

; replay frequency
	lea			chaine_replay_frequency,a0
	bsr			print_string

	move.l		DSP_frequence_de_replay_reelle_I2S,d0
	bsr			print_nombre_5_chiffres

	lea			chaine_Hz_init_LSP,a0
	bsr			print_string
	
	lea			chaine_frequency_correction,a0
	bsr			print_string
	
	move.l		#PAULA_corretion_frequence,d0
	bsr			print_nombre_3_chiffres
; ligne suivant
	moveq	#10,d0
	bsr		print_caractere

; ligne suivant
	moveq	#10,d0
	bsr		print_caractere

	lea			chaine_replay_volumes,a0
	bsr			print_string
	move.l		DSP_volume_music,d0
	bsr			print_nombre_3_chiffres
	move.l	#' ',d0
	bsr		print_caractere
	move.l		DSP_volume_SFX,d0
	bsr			print_nombre_3_chiffres
; ligne suivant
	moveq	#10,d0
	bsr		print_caractere

	
	move.w		#145,couleur_char


	move.l  #VBL,LEVEL0     	; Install 68K LEVEL0 handler
	move.w  a_vde,d0                	; Must be ODD
	;sub.w   #16,d0
	ori.w   #1,d0
	move.w  d0,VI

	move.w  #%01,INT1                 	; Enable video interrupts 11101


	;and.w   #%1111100011111111,sr				; 1111100011111111 => bits 8/9/10 = 0
	and.w   #$f8ff,sr


    st         mt_Enable            ; enable playback

;----------------
	nop
	nop
main:
; wait timer1
				move.l		Paula_compteur_frames_Timer1,d0
main2:
				cmp.l			Paula_compteur_frames_Timer1,d0
				beq.s			main2

				bsr				mt_music
				bsr				mt_music_copy_volume_and_enable_DMA
				bsr				mt_music_copy_loop_pointers
				bsr				copy_custom_paula_to_buffers_paula_asynchrones



				.if				flag_display_infos=1
				;bsr			display_infos
				.endif
				bra.s		main
;----------------



; -----------------------------------------------------
; protracker Jaguar 68000

;// d1: PAL(0) or NTSC(1)//
; // a0 = vector base //
CIA_Install:
                   ;PUSHALL
                   move.w     #DEFAULT_BPM,d0
                   move.w     d0,CIA_CurrentBPM
                   move.w     d0,CIA_MusicBPM
                   ;move.w     #(1<<13),$dff09a                                   ; disable CIA interrupt
                   ;lea        .LSP_MainIrq(pc),a0
                   ;move.l     CIA_Vector(pc),a5
                   ;move.l     a0,(a5)

                   ;lea        $bfd000,a0
                   ;move.b     #$7f,$d00(a0)				; CIAB interrupt control register
                   ;move.b     #$10,$e00(a0)				; CIAB Control register A
                   ;move.b     #$10,$f00(a0)				; CIAB Control register B
                   ;lsl.w      #2,d1
                   ; move.l     .palClocks(pc,d1.w),d1                             ; PAL or NTSC clock
                   ; lea        CIA_Clock(pc),a5
                   ; move.l     d1,(a5)
                   ; divu.w     d0,d1
                   ; move.b     d1,$400(a0)
                   ; lsr.w      #8,d1
                   ; move.b     d1,$500(a0)
                   ; move.b     #$83,$d00(a0)
                   ; move.b     #$11,$e00(a0)
			
                   ; move.b     #496&255,$600(a0)                                  ; set timer b to 496 ( to set DMACON )
                   ; move.b     #496>>8,$700(a0)

                   ; move.w     #(1<<13),$dff09c                                   ; clear any req CIA
                   ; move.w     #$a000,$dff09a                                     ; CIA interrupt enabled
                   ;POPALL
                   rts
		
;.palClocks:        dc.l       1773447,1789773

CIA_CurrentBPM:    dc.w       0
CIA_MusicBPM:      dc.w       0


mt_init:            
                   SF         mt_Enable 
                 
                   move.w     #DEFAULT_BPM,CIA_MusicBPM
                   lea        mt_chan1temp,a4
                   move.w     #mt_chanend-mt_chan1temp-1,d7
.clear:
                   clr.b      (a4)+
                   dbra       d7,.clear
                    
                   moveq      #4-1,d7
                   lea        mt_chan1temp,a4
                   move.w     #$1,d0
.dmaset:                   
                   move.w     d0,n_dmabit(a4)
                   lsl.w      #1,d0
                   lea        mt_chan2temp-mt_chan1temp(a4),a4
                   dbra       d7,.dmaset

                   MOVE.L     A0,mt_SongDataPtr
                   MOVE.L     A0,A1
                   LEA        952(A1),A1
                   MOVEQ      #127,D0
                   MOVEQ      #0,D1
mtloop:             MOVE.L     D1,D2
                   SUBQ.W     #1,D0
mtloop2:            MOVE.B     (A1)+,D1
                   CMP.B      D2,D1
                   BGT.S      mtloop
                   DBRA       D0,mtloop2
                   ADDQ.B     #1,D2
			
                   LEA        mt_SampleStarts,A1
                   ASL.L      #8,D2
                   ASL.L      #2,D2
                   ADD.L      #1084,D2
                   ADD.L      A0,D2
                   MOVE.L     D2,A2
                   MOVEQ      #30,D0
mtloop3:            CLR.L      (A2)
                   MOVE.L     A2,(A1)+
                   MOVEQ      #0,D1
                   MOVE.W     42(A0),D1
                   ASL.L      #1,D1
                   ADD.L      D1,A2
                   ADD.L      #30,A0
                   DBRA       D0,mtloop3

                   ;OR.B       #2,$BFE001
                   MOVE.B     #6,mt_speed
                   CLR.B      mt_counter
                   CLR.B      mt_SongPos
                   CLR.W      mt_PatternPos
mt_end:             
					;CLR.W      $DFF0A8		; volume a zéro
                   ;CLR.W      $DFF0B8
                   ;CLR.W      $DFF0C8
                   ;CLR.W      $DFF0D8
                   ;MOVE.W     #$F,$DFF096	; dmacon
                   RTS



mt_music:
                   TST.B      mt_Enable
                   bne        .play
                   rts
.play:
                   MOVEM.L    D0-D4/A0-A6,-(SP)
                   ADDQ.B     #1,mt_counter
                   MOVE.B     mt_counter,D0
                   CMP.B      mt_speed,D0
                   BLO.S      mt_NoNewNote
                   CLR.B      mt_counter
                   TST.B      mt_PattDelTime2
                   BEQ.S      mt_GetNewNote
                   BSR.S      mt_NoNewAllChannels
                   BRA        mt_dskip

mt_NoNewNote:
                   BSR.S      mt_NoNewAllChannels
                   BRA        mt_NoNewPosYet

mt_NoNewAllChannels:
                   ;LEA        $DFF0A0,A5
				   lea			Paula_custom,a5
                   LEA        mt_chan1temp,A6
                   BSR        mt_CheckEfx
                   ;LEA        $DFF0B0,A5
				   lea			Paula_custom+16,a5
                   LEA        mt_chan2temp,A6
                   BSR        mt_CheckEfx
                   ;LEA        $DFF0C0,A5
				   lea			Paula_custom+32,a5
                   LEA        mt_chan3temp,A6
                   BSR        mt_CheckEfx
                   ;LEA        $DFF0D0,A5
				   lea			Paula_custom+48,a5
                   LEA        mt_chan4temp,A6
                   BRA        mt_CheckEfx

mt_GetNewNote:
                   MOVE.L     mt_SongDataPtr,A0
                   LEA        12(A0),A3
                   LEA        952(A0),A2                                         ;pattpo
                   LEA        1084(A0),A0                                        ;patterndata
                   MOVEQ      #0,D0
                   MOVEQ      #0,D1
                   MOVE.B     mt_SongPos(PC),D0
                   MOVE.B     (A2,D0.W),D1
                   ASL.L      #8,D1
                   ASL.L      #2,D1
                   ADD.W      mt_PatternPos(PC),D1
                   CLR.W      mt_DMACONtemp

                   ;LEA        $DFF0A0,A5
				   lea			Paula_custom,a5
                   LEA        mt_chan1temp(PC),A6
                   BSR.S      mt_PlayVoice
                   ;LEA        $DFF0B0,A5
				   lea			Paula_custom+16,a5
                   LEA        mt_chan2temp(PC),A6
                   BSR.S      mt_PlayVoice
                   ;LEA        $DFF0C0,A5
				   lea			Paula_custom+32,a5
                   LEA        mt_chan3temp(PC),A6
                   BSR.S      mt_PlayVoice
                   ;LEA        $DFF0D0,A5
				   lea			Paula_custom+48,a5
                   LEA        mt_chan4temp(PC),A6
                   BSR.S      mt_PlayVoice
                   BRA        mt_SetDMA

mt_PlayVoice:
                   TST.L      (A6)
                   BNE.S      mt_plvskip
                   BSR        mt_PerNop
mt_plvskip:
                   MOVE.L     (A0,D1.L),(A6)
                   ADDQ.L     #4,D1
                   MOVEQ      #0,D2
                   MOVE.B     n_cmd(A6),D2
                   AND.B      #$F0,D2
                   LSR.B      #4,D2
                   MOVE.B     (A6),D0
                   AND.B      #$F0,D0
                   OR.B       D0,D2
                   TST.B      D2
                   BEQ        mt_SetRegs
                   move.w     n_dmabit(A6),d3
                   or.w       d3,mt_LoadPointer
                   MOVEQ      #0,D3
                   LEA        mt_SampleStarts(PC),A1
                   MOVE       D2,D4
                   SUBQ.L     #1,D2
                   ASL.L      #2,D2
                   MULU       #30,D4
                   MOVE.L     (A1,D2.L),n_start(A6)
                   MOVE.W     (A3,D4.L),n_length(A6)
                   MOVE.W     (A3,D4.L),n_reallength(A6)
                   MOVE.B     2(A3,D4.L),n_finetune(A6)
                   MOVE.B     3(A3,D4.L),n_volume(A6)
                   MOVE.W     4(A3,D4.L),D3                                      ; Get repeat
                   TST.W      D3
                   BEQ.S      mt_NoLoop
                   MOVE.L     n_start(A6),D2                                     ; Get start
                   ASL.W      #1,D3
                   ADD.L      D3,D2                                              ; Add repeat
                   MOVE.L     D2,n_loopstart(A6)
                   MOVE.L     D2,n_wavestart(A6)
                   MOVE.W     4(A3,D4.L),D0                                      ; Get repeat
                   ADD.W      6(A3,D4.L),D0                                      ; Add replen
                   MOVE.W     D0,n_length(A6)
                   MOVE.W     6(A3,D4.L),n_replen(A6)                            ; Save replen
                   MOVEQ      #0,D0
                   MOVE.B     n_volume(A6),D0
                   ;MOVE.W     D0,8(A5)                                           ; Set volume
                   move.w     d0,n_realvolume(a6)
                   BRA.S      mt_SetRegs

mt_NoLoop:
                   MOVE.L     n_start(A6),D2
                   ADD.L      D3,D2
                   MOVE.L     D2,n_loopstart(A6)
                   MOVE.L     D2,n_wavestart(A6)
                   MOVE.W     6(A3,D4.L),n_replen(A6)                            ; Save replen
                   MOVEQ      #0,D0
                   MOVE.B     n_volume(A6),D0
                   ;MOVE.W     D0,8(A5)                                           ; Set volume
                   move.w     d0,n_realvolume(a6)
mt_SetRegs:
                   MOVE.W     (A6),D0
                   AND.W      #$0FFF,D0
                   BEQ        mt_CheckMoreEfx                                    ; If no note
                   MOVE.W     2(A6),D0
                   AND.W      #$0FF0,D0
                   CMP.W      #$0E50,D0
                   BEQ.S      mt_DoSetFineTune
                   MOVE.B     2(A6),D0
                   AND.B      #$0F,D0
                   CMP.B      #3,D0                                              ; TonePortamento
                   BEQ.S      mt_ChkTonePorta
                   CMP.B      #5,D0
                   BEQ.S      mt_ChkTonePorta
                   CMP.B      #9,D0                                              ; Sample Offset
                   BNE.S      mt_SetPeriod
                   BSR        mt_CheckMoreEfx
                   BRA.S      mt_SetPeriod

mt_DoSetFineTune:
                   BSR        mt_SetFineTune
                   BRA.S      mt_SetPeriod

mt_ChkTonePorta:
                   BSR        mt_SetTonePorta
                   BRA        mt_CheckMoreEfx

mt_SetPeriod:
                   MOVEM.L    D0-D1/A0-A1,-(SP)
                   MOVE.W     (A6),D1
                   AND.W      #$0FFF,D1
                   tst.b      n_finetune(A6)
                   bne        .ftlookup
                   MOVE.W     D1,n_period(A6)
                   bra        mt_perlookdone
.ftlookup:
                   LEA        mt_PeriodTable,A1
                   MOVEQ      #0,D0
                   MOVEQ      #36,D7
mt_ftuloop:
                   CMP.W      (A1,D0.W),D1
                   BHS.S      mt_ftufound
                   ADDQ.L     #2,D0
                   DBRA       D7,mt_ftuloop
mt_ftufound:
                   MOVEQ      #0,D1
                   MOVE.B     n_finetune(A6),D1
                   MULU       #36*2,D1
                   ADD.L      D1,A1
                   MOVE.W     (A1,D0.W),n_period(A6)
mt_perlookdone:
                   MOVEM.L    (SP)+,D0-D1/A0-A1

                   MOVE.W     2(A6),D0
                   AND.W      #$0FF0,D0
                   CMP.W      #$0ED0,D0                                          ; Notedelay
                   BEQ        mt_CheckMoreEfx

                   ;MOVE.W     n_dmabit(A6),$DFF096
                   move.w     n_dmabit(a6),d0
                   or.w       d0,mt_DMACONtemp

                   BTST       #2,n_wavecontrol(A6)
                   BNE.S      mt_vibnoc
                   CLR.B      n_vibratopos(A6)
mt_vibnoc:
                   BTST       #6,n_wavecontrol(A6)
                   BNE.S      mt_trenoc
                   CLR.B      n_tremolopos(A6)
mt_trenoc:
                   MOVE.L     n_start(A6),(A5)                                   ; Set start
                   MOVE.W     n_length(A6),4(A5)                                 ; Set length
                   MOVE.W     n_period(A6),D0
                   MOVE.W     D0,6(A5)                                           ; Set period
                   MOVE.W     n_dmabit(A6),D0
                   OR.W       D0,mt_DMACONtemp
                   BRA        mt_CheckMoreEfx
 
mt_SetDMA:
;   off loaded to stage 2 and 3
;                   MOVE.W     #DMAWait,D0
;mt_WaitDMA
;                   DBRA       D0,mt_WaitDMA
;                   MOVE.W     mt_DMACONtemp(PC),D0
;                   OR.W       #$8000,D0
;                   MOVE.W     D0,$DFF096
;                   MOVE.W     #DMAWait,D0
;mt_WaitDMA2
;                   DBRA       D0,mt_WaitDMA2
;
;                   LEA        $DFF000,A5
;                   LEA        mt_chan4temp(PC),A6
;                   MOVE.L     n_loopstart(A6),$D0(A5)
;                   MOVE.W     n_replen(A6),$D4(A5)
;                   LEA        mt_chan3temp(PC),A6
;                   MOVE.L     n_loopstart(A6),$C0(A5)
;                   MOVE.W     n_replen(A6),$C4(A5)
;                   LEA        mt_chan2temp(PC),A6
;                   MOVE.L     n_loopstart(A6),$B0(A5)
;                   MOVE.W     n_replen(A6),$B4(A5)
;                   LEA        mt_chan1temp(PC),A6
;                   MOVE.L     n_loopstart(A6),$A0(A5)
;                   MOVE.W     n_replen(A6),$A4(A5)

mt_dskip:
                   ADD.W      #16,mt_PatternPos
                   MOVE.B     mt_PattDelTime,D0
                   BEQ.S      mt_dskc
                   MOVE.B     D0,mt_PattDelTime2
                   CLR.B      mt_PattDelTime
mt_dskc:            TST.B      mt_PattDelTime2
                   BEQ.S      mt_dska
                   SUBQ.B     #1,mt_PattDelTime2
                   BEQ.S      mt_dska
                   SUB.W      #16,mt_PatternPos
mt_dska:            TST.B      mt_PBreakFlag
                   BEQ.S      mt_nnpysk
                   SF         mt_PBreakFlag
                   MOVEQ      #0,D0
                   MOVE.B     mt_PBreakPos(PC),D0
                   CLR.B      mt_PBreakPos
                   LSL.W      #4,D0
                   MOVE.W     D0,mt_PatternPos
mt_nnpysk:
                   CMP.W      #1024,mt_PatternPos
                   BLO.S      mt_NoNewPosYet
mt_NextPosition:	
                   MOVEQ      #0,D0
                   MOVE.B     mt_PBreakPos(PC),D0
                   LSL.W      #4,D0
                   MOVE.W     D0,mt_PatternPos
                   CLR.B      mt_PBreakPos
                   CLR.B      mt_PosJumpFlag
                   ADDQ.B     #1,mt_SongPos
                   AND.B      #$7F,mt_SongPos
                   MOVE.B     mt_SongPos(PC),D1
                   MOVE.L     mt_SongDataPtr(PC),A0
                   CMP.B      950(A0),D1
                   BLO.S      mt_NoNewPosYet
                   CLR.B      mt_SongPos
mt_NoNewPosYet:	
                   TST.B      mt_PosJumpFlag
                   BNE.S      mt_NextPosition
                   MOVEM.L    (SP)+,D0-D4/A0-A6
                   RTS

mt_CheckEfx:
                   BSR        mt_UpdateFunk
                   MOVE.W     n_cmd(A6),D0
                   AND.W      #$0FFF,D0
                   BEQ.S      mt_PerNop
                   MOVE.B     n_cmd(A6),D0
                   AND.B      #$0F,D0
                   BEQ.S      mt_Arpeggio
                   CMP.B      #1,D0
                   BEQ        mt_PortaUp
                   CMP.B      #2,D0
                   BEQ        mt_PortaDown
                   CMP.B      #3,D0
                   BEQ        mt_TonePortamento
                   CMP.B      #4,D0
                   BEQ        mt_Vibrato
                   CMP.B      #5,D0
                   BEQ        mt_TonePlusVolSlide
                   CMP.B      #6,D0
                   BEQ        mt_VibratoPlusVolSlide
                   CMP.B      #$E,D0
                   BEQ        mt_E_Commands
SetBack:            MOVE.W     n_period(A6),6(A5)
                   CMP.B      #7,D0
                   BEQ        mt_Tremolo
                   CMP.B      #$A,D0
                   BEQ        mt_VolumeSlide
mt_Return2:
                   RTS

mt_PerNop:
                   MOVE.W     n_period(A6),6(A5)
                   RTS

mt_Arpeggio:
                   MOVEQ      #0,D0
                   MOVE.B     mt_counter(PC),D0
                   DIVS       #3,D0
                   SWAP       D0
                   CMP.W      #0,D0
                   BEQ.S      mt_Arpeggio2
                   CMP.W      #2,D0
                   BEQ.S      mt_Arpeggio1
                   MOVEQ      #0,D0
                   MOVE.B     n_cmdlo(A6),D0
                   LSR.B      #4,D0
                   BRA.S      mt_Arpeggio3

mt_Arpeggio1:
                   MOVEQ      #0,D0
                   MOVE.B     n_cmdlo(A6),D0
                   AND.B      #15,D0
                   BRA.S      mt_Arpeggio3

mt_Arpeggio2:
                   MOVE.W     n_period(A6),D2
                   BRA.S      mt_Arpeggio4

mt_Arpeggio3:
                   ASL.W      #1,D0
                   MOVEQ      #0,D1
                   MOVE.B     n_finetune(A6),D1
                   MULU       #36*2,D1
                   LEA        mt_PeriodTable,A0
                   ADD.L      D1,A0
                   MOVEQ      #0,D1
                   MOVE.W     n_period(A6),D1
                   MOVEQ      #36,D7
mt_arploop:
                   MOVE.W     (A0,D0.W),D2
                   CMP.W      (A0),D1
                   BHS.S      mt_Arpeggio4
                   ADDQ.L     #2,A0
                   DBRA       D7,mt_arploop
                   RTS

mt_Arpeggio4:
                   MOVE.W     D2,6(A5)
                   RTS

mt_FinePortaUp:
                   TST.B      mt_counter
                   BNE.S      mt_Return2
                   MOVE.B     #$0F,mt_LowMask
mt_PortaUp:
                   MOVEQ      #0,D0
                   MOVE.B     n_cmdlo(A6),D0
                   AND.B      mt_LowMask(PC),D0
                   MOVE.B     #$FF,mt_LowMask
                   SUB.W      D0,n_period(A6)
                   MOVE.W     n_period(A6),D0
                   AND.W      #$0FFF,D0
                   CMP.W      #113,D0
                   BPL.S      mt_PortaUskip
                   AND.W      #$F000,n_period(A6)
                   OR.W       #113,n_period(A6)
mt_PortaUskip:
                   MOVE.W     n_period(A6),D0
                   AND.W      #$0FFF,D0
                   MOVE.W     D0,6(A5)
                   RTS	
 
mt_FinePortaDown:
                   TST.B      mt_counter
                   BNE        mt_Return2
                   MOVE.B     #$0F,mt_LowMask
mt_PortaDown:
                   CLR.W      D0
                   MOVE.B     n_cmdlo(A6),D0
                   AND.B      mt_LowMask(PC),D0
                   MOVE.B     #$FF,mt_LowMask
                   ADD.W      D0,n_period(A6)
                   MOVE.W     n_period(A6),D0
                   AND.W      #$0FFF,D0
                   CMP.W      #856,D0
                   BMI.S      mt_PortaDskip
                   AND.W      #$F000,n_period(A6)
                   OR.W       #856,n_period(A6)
mt_PortaDskip:
                   MOVE.W     n_period(A6),D0
                   AND.W      #$0FFF,D0
                   MOVE.W     D0,6(A5)
                   RTS

mt_SetTonePorta:
                   MOVE.L     A0,-(SP)
                   MOVE.W     (A6),D2
                   AND.W      #$0FFF,D2
                   MOVEQ      #0,D0
                   MOVE.B     n_finetune(A6),D0
                   MULU       #36*2,D0                                           ;37?
                   LEA        mt_PeriodTable,A0
                   ADD.L      D0,A0
                   MOVEQ      #0,D0
mt_StpLoop:
                   CMP.W      (A0,D0.W),D2
                   BHS.S      mt_StpFound
                   ADDQ.W     #2,D0
                   CMP.W      #36*2,D0                                           ;37?
                   BLO.S      mt_StpLoop
                   MOVEQ      #35*2,D0
mt_StpFound:
                   MOVE.B     n_finetune(A6),D2
                   AND.B      #8,D2
                   BEQ.S      mt_StpGoss
                   TST.W      D0
                   BEQ.S      mt_StpGoss
                   SUBQ.W     #2,D0
mt_StpGoss:
                   MOVE.W     (A0,D0.W),D2
                   MOVE.L     (SP)+,A0
                   MOVE.W     D2,n_wantedperiod(A6)
                   MOVE.W     n_period(A6),D0
                   CLR.B      n_toneportdirec(A6)
                   CMP.W      D0,D2
                   BEQ.S      mt_ClearTonePorta
                   BGE        mt_Return2
                   MOVE.B     #1,n_toneportdirec(A6)
                   RTS

mt_ClearTonePorta:
                   CLR.W      n_wantedperiod(A6)
                   RTS

mt_TonePortamento:
                   MOVE.B     n_cmdlo(A6),D0
                   BEQ.S      mt_TonePortNoChange
                   MOVE.B     D0,n_toneportspeed(A6)
                   CLR.B      n_cmdlo(A6)
mt_TonePortNoChange:
                   TST.W      n_wantedperiod(A6)
                   BEQ        mt_Return2
                   MOVEQ      #0,D0
                   MOVE.B     n_toneportspeed(A6),D0
                   TST.B      n_toneportdirec(A6)
                   BNE.S      mt_TonePortaUp
mt_TonePortaDown:
                   ADD.W      D0,n_period(A6)
                   MOVE.W     n_wantedperiod(A6),D0
                   CMP.W      n_period(A6),D0
                   BGT.S      mt_TonePortaSetPer
                   MOVE.W     n_wantedperiod(A6),n_period(A6)
                   CLR.W      n_wantedperiod(A6)
                   BRA.S      mt_TonePortaSetPer

mt_TonePortaUp:
                   SUB.W      D0,n_period(A6)
                   MOVE.W     n_wantedperiod(A6),D0
                   CMP.W      n_period(A6),D0
                   BLT.S      mt_TonePortaSetPer
                   MOVE.W     n_wantedperiod(A6),n_period(A6)
                   CLR.W      n_wantedperiod(A6)

mt_TonePortaSetPer:
                   MOVE.W     n_period(A6),D2
                   MOVE.B     n_glissfunk(A6),D0
                   AND.B      #$0F,D0
                   BEQ.S      mt_GlissSkip
                   MOVEQ      #0,D0
                   MOVE.B     n_finetune(A6),D0
                   MULU       #36*2,D0
                   LEA        mt_PeriodTable,A0
                   ADD.L      D0,A0
                   MOVEQ      #0,D0
mt_GlissLoop:
                   CMP.W      (A0,D0.W),D2
                   BHS.S      mt_GlissFound
                   ADDQ.W     #2,D0
                   CMP.W      #36*2,D0
                   BLO.S      mt_GlissLoop
                   MOVEQ      #35*2,D0
mt_GlissFound:
                   MOVE.W     (A0,D0.W),D2
mt_GlissSkip:
                   MOVE.W     D2,6(A5)                                           ; Set period
                   RTS

mt_Vibrato:
                   MOVE.B     n_cmdlo(A6),D0
                   BEQ.S      mt_Vibrato2
                   MOVE.B     n_vibratocmd(A6),D2
                   AND.B      #$0F,D0
                   BEQ.S      mt_vibskip
                   AND.B      #$F0,D2
                   OR.B       D0,D2
mt_vibskip:
                   MOVE.B     n_cmdlo(A6),D0
                   AND.B      #$F0,D0
                   BEQ.S      mt_vibskip2
                   AND.B      #$0F,D2
                   OR.B       D0,D2
mt_vibskip2:
                   MOVE.B     D2,n_vibratocmd(A6)
mt_Vibrato2:
                   MOVE.B     n_vibratopos(A6),D0
                   LEA        mt_VibratoTable,A4
                   LSR.W      #2,D0
                   AND.W      #$001F,D0
                   MOVEQ      #0,D2
                   MOVE.B     n_wavecontrol(A6),D2
                   AND.B      #$03,D2
                   BEQ.S      mt_vib_sine
                   LSL.B      #3,D0
                   CMP.B      #1,D2
                   BEQ.S      mt_vib_rampdown
                   MOVE.B     #255,D2
                   BRA.S      mt_vib_set
mt_vib_rampdown:
                   TST.B      n_vibratopos(A6)
                   BPL.S      mt_vib_rampdown2
                   MOVE.B     #255,D2
                   SUB.B      D0,D2
                   BRA.S      mt_vib_set
mt_vib_rampdown2:
                   MOVE.B     D0,D2
                   BRA.S      mt_vib_set
mt_vib_sine:
                   MOVE.B     0(A4,D0.W),D2
mt_vib_set:
                   MOVE.B     n_vibratocmd(A6),D0
                   AND.W      #15,D0
                   MULU       D0,D2
                   LSR.W      #7,D2
                   MOVE.W     n_period(A6),D0
                   TST.B      n_vibratopos(A6)
                   BMI.S      mt_VibratoNeg
                   ADD.W      D2,D0
                   BRA.S      mt_Vibrato3
mt_VibratoNeg:
                   SUB.W      D2,D0
mt_Vibrato3:
                   MOVE.W     D0,6(A5)
                   MOVE.B     n_vibratocmd(A6),D0
                   LSR.W      #2,D0
                   AND.W      #$003C,D0
                   ADD.B      D0,n_vibratopos(A6)
                   RTS

mt_TonePlusVolSlide:
                   BSR        mt_TonePortNoChange
                   BRA        mt_VolumeSlide

mt_VibratoPlusVolSlide:
                   BSR.S      mt_Vibrato2
                   BRA        mt_VolumeSlide

mt_Tremolo:
                   MOVE.B     n_cmdlo(A6),D0
                   BEQ.S      mt_Tremolo2
                   MOVE.B     n_tremolocmd(A6),D2
                   AND.B      #$0F,D0
                   BEQ.S      mt_treskip
                   AND.B      #$F0,D2
                   OR.B       D0,D2
mt_treskip:
                   MOVE.B     n_cmdlo(A6),D0
                   AND.B      #$F0,D0
                   BEQ.S      mt_treskip2
                   AND.B      #$0F,D2
                   OR.B       D0,D2
mt_treskip2:
                   MOVE.B     D2,n_tremolocmd(A6)
mt_Tremolo2:
                   MOVE.B     n_tremolopos(A6),D0
                   LEA        mt_VibratoTable,A4
                   LSR.W      #2,D0
                   AND.W      #$001F,D0
                   MOVEQ      #0,D2
                   MOVE.B     n_wavecontrol(A6),D2
                   LSR.B      #4,D2
                   AND.B      #$03,D2
                   BEQ.S      mt_tre_sine
                   LSL.B      #3,D0
                   CMP.B      #1,D2
                   BEQ.S      mt_tre_rampdown
                   MOVE.B     #255,D2
                   BRA.S      mt_tre_set
mt_tre_rampdown:
                   TST.B      n_vibratopos(A6)
                   BPL.S      mt_tre_rampdown2
                   MOVE.B     #255,D2
                   SUB.B      D0,D2
                   BRA.S      mt_tre_set
mt_tre_rampdown2:
                   MOVE.B     D0,D2
                   BRA.S      mt_tre_set
mt_tre_sine:
                   MOVE.B     0(A4,D0.W),D2
mt_tre_set:
                   MOVE.B     n_tremolocmd(A6),D0
                   AND.W      #15,D0
                   MULU       D0,D2
                   LSR.W      #6,D2
                   MOVEQ      #0,D0
                   MOVE.B     n_volume(A6),D0
                   TST.B      n_tremolopos(A6)
                   BMI.S      mt_TremoloNeg
                   ADD.W      D2,D0
                   BRA.S      mt_Tremolo3
mt_TremoloNeg:
                   SUB.W      D2,D0
mt_Tremolo3:
                   BPL.S      mt_TremoloSkip
                   CLR.W      D0
mt_TremoloSkip:
                   CMP.W      #$40,D0
                   BLS.S      mt_TremoloOk
                   MOVE.W     #$40,D0
mt_TremoloOk:
                   ;MOVE.W     D0,8(A5)                                           ; Set volume
                   move.w     d0,n_realvolume(a6)
                   MOVE.B     n_tremolocmd(A6),D0
                   LSR.W      #2,D0
                   AND.W      #$003C,D0
                   ADD.B      D0,n_tremolopos(A6)
                   RTS

mt_SampleOffset:
                   MOVEQ      #0,D0
                   MOVE.B     n_cmdlo(A6),D0
                   BEQ.S      mt_sononew
                   MOVE.B     D0,n_sampleoffset(A6)
mt_sononew:
                   MOVE.B     n_sampleoffset(A6),D0
                   LSL.W      #7,D0
                   CMP.W      n_length(A6),D0
                   BGE.S      mt_sofskip
                   SUB.W      D0,n_length(A6)
                   LSL.W      #1,D0
                   ADD.L      D0,n_start(A6)
                   RTS
mt_sofskip:
                   MOVE.W     #$0001,n_length(A6)
                   RTS

mt_VolumeSlide:
                   MOVEQ      #0,D0
                   MOVE.B     n_cmdlo(A6),D0
                   LSR.B      #4,D0
                   TST.B      D0
                   BEQ.S      mt_VolSlideDown
mt_VolSlideUp:
                   ADD.B      D0,n_volume(A6)
                   CMP.B      #$40,n_volume(A6)
                   BMI.S      mt_vsuskip
                   MOVE.B     #$40,n_volume(A6)
mt_vsuskip:
                   MOVE.B     n_volume(A6),D0
                   ;MOVE.W     D0,8(A5)                                           ; Set volume
                   move.w     d0,n_realvolume(a6)
                   RTS

mt_VolSlideDown:
                   MOVEQ      #0,D0
                   MOVE.B     n_cmdlo(A6),D0
                   AND.B      #$0F,D0
mt_VolSlideDown2:
                   SUB.B      D0,n_volume(A6)
                   BPL.S      mt_vsdskip
                   CLR.B      n_volume(A6)
mt_vsdskip:
                   MOVE.B     n_volume(A6),D0
                   ;MOVE.W     D0,8(A5)                                           ; Set volume
                   move.w     d0,n_realvolume(a6)
                   RTS

mt_PositionJump:
                   MOVE.B     n_cmdlo(A6),D0
                   SUBQ.B     #1,D0
                   MOVE.B     D0,mt_SongPos
mt_pj2:             CLR.B      mt_PBreakPos
                   ST         mt_PosJumpFlag
                   RTS

mt_VolumeChange:
                   MOVEQ      #0,D0
                   MOVE.B     n_cmdlo(A6),D0
                   CMP.B      #$40,D0
                   BLS.S      mt_VolumeOk
                   MOVEQ      #$40,D0
mt_VolumeOk:
                   MOVE.B     D0,n_volume(A6)
                   ;MOVE.W     D0,8(A5)                                           ; Set volume
                   move.w     d0,n_realvolume(a6)
                   RTS

mt_PatternBreak:
                   MOVEQ      #0,D0
                   MOVE.B     n_cmdlo(A6),D0
                   MOVE.L     D0,D2
                   LSR.B      #4,D0
                   MULU       #10,D0
                   AND.B      #$0F,D2
                   ADD.B      D2,D0
                   CMP.B      #63,D0
                   BHI.S      mt_pj2
                   MOVE.B     D0,mt_PBreakPos
                   ST         mt_PosJumpFlag
                   RTS

mt_SetSpeed:
                   MOVEQ      #0,D0
                   MOVE.B     3(A6),D0
                   BEQ        .stop                                              ;mt_end
                   CLR.B      mt_counter
                   CMP.B      #32,D0
                   bhs        .ciaset
                   MOVE.B     D0,mt_speed
                   RTS
.ciaset:
                   move.w     d0,CIA_MusicBPM
                   rts

.stop:
                   sf         mt_Enable
                   rts

mt_CheckMoreEfx:
                   BSR        mt_UpdateFunk
                   MOVE.B     2(A6),D0
                   AND.B      #$0F,D0
                   CMP.B      #$9,D0
                   BEQ        mt_SampleOffset
                   CMP.B      #$B,D0
                   BEQ        mt_PositionJump
                   CMP.B      #$D,D0
                   BEQ.S      mt_PatternBreak
                   CMP.B      #$E,D0
                   BEQ.S      mt_E_Commands
                   CMP.B      #$F,D0
                   BEQ.S      mt_SetSpeed
                   CMP.B      #$C,D0
                   BEQ        mt_VolumeChange
                   BRA        mt_PerNop

mt_E_Commands:
                   MOVE.B     n_cmdlo(A6),D0
                   AND.B      #$F0,D0
                   LSR.B      #4,D0
                   BEQ.S      mt_FilterOnOff
                   CMP.B      #1,D0
                   BEQ        mt_FinePortaUp
                   CMP.B      #2,D0
                   BEQ        mt_FinePortaDown
                   CMP.B      #3,D0
                   BEQ.S      mt_SetGlissControl
                   CMP.B      #4,D0
                   BEQ        mt_SetVibratoControl
                   CMP.B      #5,D0
                   BEQ        mt_SetFineTune
                   CMP.B      #6,D0
                   BEQ        mt_JumpLoop
                   CMP.B      #7,D0
                   BEQ        mt_SetTremoloControl
                   CMP.B      #9,D0
                   BEQ        mt_RetrigNote
                   CMP.B      #$A,D0
                   BEQ        mt_VolumeFineUp
                   CMP.B      #$B,D0
                   BEQ        mt_VolumeFineDown
                   CMP.B      #$C,D0
                   BEQ        mt_NoteCut
                   CMP.B      #$D,D0
                   BEQ        mt_NoteDelay
                   CMP.B      #$E,D0
                   BEQ        mt_PatternDelay
                   CMP.B      #$F,D0
                   BEQ        mt_FunkIt
                   RTS

mt_FilterOnOff:
                   MOVE.B     n_cmdlo(A6),D0
                   AND.B      #1,D0
                   ASL.B      #1,D0
                   AND.B      #$FD,$BFE001
                   OR.B       D0,$BFE001
                   RTS	

mt_SetGlissControl:
                   MOVE.B     n_cmdlo(A6),D0
                   AND.B      #$0F,D0
                   AND.B      #$F0,n_glissfunk(A6)
                   OR.B       D0,n_glissfunk(A6)
                   RTS

mt_SetVibratoControl:
                   MOVE.B     n_cmdlo(A6),D0
                   AND.B      #$0F,D0
                   AND.B      #$F0,n_wavecontrol(A6)
                   OR.B       D0,n_wavecontrol(A6)
                   RTS

mt_SetFineTune:
                   MOVE.B     n_cmdlo(A6),D0
                   AND.B      #$0F,D0
                   MOVE.B     D0,n_finetune(A6)
                   RTS

mt_JumpLoop:
                   TST.B      mt_counter
                   BNE        mt_Return2
                   MOVE.B     n_cmdlo(A6),D0
                   AND.B      #$0F,D0
                   BEQ.S      mt_SetLoop
                   TST.B      n_loopcount(A6)
                   BEQ.S      mt_jumpcnt
                   SUBQ.B     #1,n_loopcount(A6)
                   BEQ        mt_Return2
mt_jmploop:         MOVE.B     n_pattpos(A6),mt_PBreakPos
                   ST         mt_PBreakFlag
                   RTS

mt_jumpcnt:
                   MOVE.B     D0,n_loopcount(A6)
                   BRA.S      mt_jmploop

mt_SetLoop:
                   MOVE.W     mt_PatternPos(PC),D0
                   LSR.W      #4,D0
                   MOVE.B     D0,n_pattpos(A6)
                   RTS

mt_SetTremoloControl:
                   MOVE.B     n_cmdlo(A6),D0
                   AND.B      #$0F,D0
                   LSL.B      #4,D0
                   AND.B      #$0F,n_wavecontrol(A6)
                   OR.B       D0,n_wavecontrol(A6)
                   RTS

mt_RetrigNote:
                   MOVE.L     D1,-(SP)
                   MOVEQ      #0,D0
                   MOVE.B     n_cmdlo(A6),D0
                   AND.B      #$0F,D0
                   BEQ.S      mt_rtnend
                   MOVEQ      #0,D1
                   MOVE.B     mt_counter(PC),D1
                   BNE.S      mt_rtnskp
                   MOVE.W     (A6),D1
                   AND.W      #$0FFF,D1
                   BNE.S      mt_rtnend
                   MOVEQ      #0,D1
                   MOVE.B     mt_counter(PC),D1
mt_rtnskp:
                   DIVU       D0,D1
                   SWAP       D1
                   TST.W      D1
                   BNE.S      mt_rtnend
mt_DoRetrig:
                   ;MOVE.W     n_dmabit(A6),$DFF096                               ; Channel DMA off
                   move.w     n_dmabit(a6),d0
                   ;move.w     d0,$dff096
                   or.w       d0,mt_DMACONtemp
                   MOVE.L     n_start(A6),(A5)                                   ; Set sampledata pointer
                   MOVE.W     n_length(A6),4(A5)                                 ; Set length
;   off loaded to stage 2 and 3
;                   MOVE.W     #DMAWait,D0
;mt_rtnloop1
;                   DBRA       D0,mt_rtnloop1
;                   MOVE.W     n_dmabit(A6),D0
;                   BSET       #15,D0
;                   MOVE.W     D0,$DFF096
;                   MOVE.W     #DMAWait,D0
;mt_rtnloop2
;                   DBRA       D0,mt_rtnloop2
;                   MOVE.L     n_loopstart(A6),(A5)
;                   MOVE.L     n_replen(A6),4(A5)
mt_rtnend:
                   MOVE.L     (SP)+,D1
                   RTS

mt_VolumeFineUp:
                   TST.B      mt_counter
                   BNE        mt_Return2
                   MOVEQ      #0,D0
                   MOVE.B     n_cmdlo(A6),D0
                   AND.B      #$F,D0
                   BRA        mt_VolSlideUp

mt_VolumeFineDown:
                   TST.B      mt_counter
                   BNE        mt_Return2
                   MOVEQ      #0,D0
                   MOVE.B     n_cmdlo(A6),D0
                   AND.B      #$0F,D0
                   BRA        mt_VolSlideDown2

mt_NoteCut:
                   MOVEQ      #0,D0
                   MOVE.B     n_cmdlo(A6),D0
                   AND.B      #$0F,D0
                   CMP.B      mt_counter(PC),D0
                   BNE        mt_Return2
                   CLR.B      n_volume(A6)
                   ;MOVE.W     #0,8(A5)
                   clr.w      n_realvolume(a6)
                   RTS

mt_NoteDelay:
                   MOVEQ      #0,D0
                   MOVE.B     n_cmdlo(A6),D0
                   AND.B      #$0F,D0
                   CMP.B      mt_counter,D0
                   BNE        mt_Return2
                   MOVE.W     (A6),D0
                   BEQ        mt_Return2
                   MOVE.L     D1,-(SP)
                   BRA        mt_DoRetrig

mt_PatternDelay:
                   TST.B      mt_counter
                   BNE        mt_Return2
                   MOVEQ      #0,D0
                   MOVE.B     n_cmdlo(A6),D0
                   AND.B      #$0F,D0
                   TST.B      mt_PattDelTime2
                   BNE        mt_Return2
                   ADDQ.B     #1,D0
                   MOVE.B     D0,mt_PattDelTime
                   RTS

mt_FunkIt:
                   TST.B      mt_counter
                   BNE        mt_Return2
                   MOVE.B     n_cmdlo(A6),D0
                   AND.B      #$0F,D0
                   LSL.B      #4,D0
                   AND.B      #$0F,n_glissfunk(A6)
                   OR.B       D0,n_glissfunk(A6)
                   TST.B      D0
                   BEQ        mt_Return2
mt_UpdateFunk:
                   MOVEM.L    A0/D1,-(SP)
                   MOVEQ      #0,D0
                   MOVE.B     n_glissfunk(A6),D0
                   LSR.B      #4,D0
                   BEQ.S      mt_funkend
                   LEA        mt_FunkTable,A0
                   MOVE.B     (A0,D0.W),D0
                   ADD.B      D0,n_funkoffset(A6)
                   BTST       #7,n_funkoffset(A6)
                   BEQ.S      mt_funkend
                   CLR.B      n_funkoffset(A6)

                   MOVE.L     n_loopstart(A6),D0
                   MOVEQ      #0,D1
                   MOVE.W     n_replen(A6),D1
                   ADD.L      D1,D0
                   ADD.L      D1,D0
                   MOVE.L     n_wavestart(A6),A0
                   ADDQ.L     #1,A0
                   CMP.L      D0,A0
                   BLO.S      mt_funkok
                   MOVE.L     n_loopstart(A6),A0
mt_funkok:
                   MOVE.L     A0,n_wavestart(A6)
                   MOVEQ      #-1,D0
                   SUB.B      (A0),D0
                   MOVE.B     D0,(A0)
mt_funkend:
                   MOVEM.L    (SP)+,A0/D1
                   RTS

mt_chan1temp:       dc.l       0,0,0,0,0,$00010000,0,  0,0,0,0,0
mt_chan2temp:       dc.l       0,0,0,0,0,$00020000,0,  0,0,0,0,0
mt_chan3temp:       dc.l       0,0,0,0,0,$00040000,0,  0,0,0,0,0
mt_chan4temp:       dc.l       0,0,0,0,0,$00080000,0,  0,0,0,0,0
mt_chanend:
mt_SampleStarts:    dc.l       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
                   dc.l       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

mt_SongDataPtr:     dc.l       0

mt_speed:           dc.b       6
mt_counter:         dc.b       0
mt_SongPos:         dc.b       0
mt_PBreakPos:       dc.b       0
mt_PosJumpFlag:     dc.b       0
mt_PBreakFlag:      dc.b       0
mt_LowMask:         dc.b       0
mt_PattDelTime:     dc.b       0
mt_PattDelTime2:    dc.b       0,0
mt_PatternPos:      dc.w       0
mt_DMACONtemp:      dc.w       0
mt_LoadPointer:     dc.w       0
mt_Enable:          dc.b       0
		.phrase
; -----------------------------------------------------








; ----------- display infos --------
display_infos:
	.if			channel_1=1
; pointeur sample en cours / interne
	move.l	PAULA_sample_location0,d0
	lsr.l		#8,d0
	lsr.l		#nb_bits_virgule_offset-8,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere

; virgule sample en cours / interne
	move.l	PAULA_sample_location_virgule0,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere

; end 0 / interne
	move.l	PAULA_sample_end0,d0
	lsr.l		#8,d0
	lsr.l		#nb_bits_virgule_offset-8,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere

; PAULA_PAULA_increment0
	move.l	PAULA_increment0,d0
	bsr		print_nombre_hexa_8_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere

	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere


; PAULA_volume0
	move.l	PAULA_volume0,d0
	bsr		print_nombre_hexa_2_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere


; pointeur sample repeat / externe
	move.l	PAULA_repeat_location0,d0
	lsr.l		#8,d0
	lsr.l		#nb_bits_virgule_offset-8,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere

; virgule sample repeat fin / externe
	move.l	PAULA_repeat_end0,d0
	lsr.l		#8,d0
	lsr.l		#nb_bits_virgule_offset-8,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere


	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere
	.endif

; voie B
	.if			channel_2=1
; pointeur sample en cours / interne
	move.l	PAULA_sample_location1,d0
	lsr.l		#8,d0
	lsr.l		#nb_bits_virgule_offset-8,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere

; virgule sample en cours / interne
	move.l	PAULA_sample_location_virgule1,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere

; end 0 / interne
	move.l	PAULA_sample_end1,d0
	lsr.l		#8,d0
	lsr.l		#nb_bits_virgule_offset-8,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere

; PAULA_PAULA_increment0
	move.l	PAULA_increment1,d0
	bsr		print_nombre_hexa_8_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere


	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere

; PAULA_volume0
	move.l	PAULA_volume1,d0
	bsr		print_nombre_hexa_2_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere


; pointeur sample repeat / externe
	move.l	PAULA_repeat_location1,d0
	lsr.l		#8,d0
	lsr.l		#nb_bits_virgule_offset-8,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere

; virgule sample repeat fin / externe
	move.l	PAULA_repeat_end1,d0
	lsr.l		#8,d0
	lsr.l		#nb_bits_virgule_offset-8,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere


	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere
	.endif
	
; voie C
	.if		channel_3=1
; pointeur sample en cours / interne
	move.l	PAULA_sample_location2,d0
	lsr.l		#8,d0
	lsr.l		#nb_bits_virgule_offset-8,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere

; virgule sample en cours / interne
	move.l	PAULA_sample_location_virgule2,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere

; end 0 / interne
	move.l	PAULA_sample_end2,d0
	lsr.l		#8,d0
	lsr.l		#nb_bits_virgule_offset-8,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere

; PAULA_PAULA_increment0
	move.l	PAULA_increment2,d0
	bsr		print_nombre_hexa_8_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere


	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere

; PAULA_volume0
	move.l	PAULA_volume2,d0
	bsr		print_nombre_hexa_2_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere


; pointeur sample repeat / externe
	move.l	PAULA_repeat_location2,d0
	lsr.l		#8,d0
	lsr.l		#nb_bits_virgule_offset-8,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere

; virgule sample repeat fin / externe
	move.l	PAULA_repeat_end2,d0
	lsr.l		#8,d0
	lsr.l		#nb_bits_virgule_offset-8,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere


	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere
	.endif

; voie D
	.if		channel_4=1
; pointeur sample en cours / interne
	move.l	PAULA_sample_location3,d0
	lsr.l		#8,d0
	lsr.l		#nb_bits_virgule_offset-8,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere

; virgule sample en cours / interne
	move.l	PAULA_sample_location_virgule3,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere

; end 0 / interne
	move.l	PAULA_sample_end3,d0
	lsr.l		#8,d0
	lsr.l		#nb_bits_virgule_offset-8,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere

; PAULA_PAULA_increment0
	move.l	PAULA_increment3,d0
	bsr		print_nombre_hexa_8_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere


	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere

; PAULA_volume0
	move.l	PAULA_volume3,d0
	bsr		print_nombre_hexa_2_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere


; pointeur sample repeat / externe
	move.l	PAULA_repeat_location3,d0
	lsr.l		#8,d0
	lsr.l		#nb_bits_virgule_offset-8,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere

; virgule sample repeat fin / externe
	move.l	PAULA_repeat_end3,d0
	lsr.l		#8,d0
	lsr.l		#nb_bits_virgule_offset-8,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere


	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere
	.endif

;SFX
	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere

; pointeur sample en cours / interne
	move.l	PAULA_sample_location_sfx1,d0
	lsr.l		#8,d0
	lsr.l		#nb_bits_virgule_offset-8,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere

; end 0 / interne
	move.l	PAULA_sample_end_sfx1,d0
	lsr.l		#8,d0
	lsr.l		#nb_bits_virgule_offset-8,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere

; PAULA_PAULA_increment0
	move.l	PAULA_period_sfx1,d0
	bsr		print_nombre_hexa_8_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere


	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere

; PAULA_volume0
	move.l	PAULA_volume_sfx1,d0
	bsr		print_nombre_hexa_2_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere


; pointeur sample repeat / externe
	move.l	PAULA_repeat_location_sfx1,d0
	lsr.l		#8,d0
	lsr.l		#nb_bits_virgule_offset-8,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere

; virgule sample repeat fin / externe
	move.l	PAULA_repeat_end_sfx1,d0
	lsr.l		#8,d0
	lsr.l		#nb_bits_virgule_offset-8,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere


	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere

; pointeur sample en cours / interne
	move.l	PAULA_sample_location_sfx2,d0
	lsr.l		#8,d0
	lsr.l		#nb_bits_virgule_offset-8,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere

; end 0 / interne
	move.l	PAULA_sample_end_sfx2,d0
	lsr.l		#8,d0
	lsr.l		#nb_bits_virgule_offset-8,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere

; PAULA_PAULA_increment0
	move.l	PAULA_period_sfx2,d0
	bsr		print_nombre_hexa_8_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere


	; ligne suivant
	moveq	#10,d0
	bsr		print_caractere

; PAULA_volume0
	move.l	PAULA_volume_sfx2,d0
	bsr		print_nombre_hexa_2_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere


; pointeur sample repeat / externe
	move.l	PAULA_repeat_location_sfx2,d0
	lsr.l		#8,d0
	lsr.l		#nb_bits_virgule_offset-8,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere

; virgule sample repeat fin / externe
	move.l	PAULA_repeat_end_sfx2,d0
	lsr.l		#8,d0
	lsr.l		#nb_bits_virgule_offset-8,d0
	bsr		print_nombre_hexa_6_chiffres
; 
	move.l	#' ',d0
	bsr		print_caractere



	; retour a la ligne	 au dessus
	.if			channel_1=1
	.rept			2
	moveq	#8,d0
	bsr		print_caractere
	.endr
	.endif
	.if			channel_2=1
	.rept			2
	moveq	#8,d0
	bsr		print_caractere
	.endr
	.endif
	.if			channel_3=1
	.rept			2
	moveq	#8,d0
	bsr		print_caractere
	.endr
	.endif
	.if			channel_4=1
	.rept			2
	moveq	#8,d0
	bsr		print_caractere
	.endr
	.endif
; SFX
	.rept			4
	moveq	#8,d0
	bsr		print_caractere
	.endr


	rts

;-----------------------------------------------------------------------------------
;--------------------------
; VBL

VBL:
; vbl interrupt, but also DSP to 68000 interrupt
; 
                movem.l 	d0-d7/a0-a6,-(a7)
                bsr     copy_olist              	; use Blitter to update active list from shadow
                addq.l	#1,vbl_counter
				
				
; test keys / pad
; xxxxxxCx xxBx2580 147*oxAP 369#RLDU
				move.l		DSP_pad1,d0
				move.l		d0,d1
				and.l			#U235SE_BUT_A,d1
				beq.s		.pas_button_A
				moveq		#0,d0			; sample=0
				moveq		#0,d1			; left
				bsr		plays_speech_sfx
.pas_button_A:
				move.l		d0,d1
				and.l			#U235SE_BUT_B,d1
				beq.s		.pas_button_B
				moveq		#1,d0			; sample=1
				moveq		#1,d1			; right
				bsr		plays_speech_sfx
.pas_button_B:
				move.l		d0,d1
				and.l			#U235SE_BUT_C,d1
				beq.s		.pas_button_C
				moveq		#2,d0			; sample=2
				moveq		#0,d1			; left
				bsr		plays_speech_sfx
.pas_button_C:
				
				
				
VBL_exit:
                movem.l (a7)+,d0-d7/a0-a6
VBL_empty:
                move.w		 	 #$101,INT1              	; Signal we're done						= $F000E0		$101 = clear VI
				move.w  	#$0,INT2						; The bus priorities restored = $F000E2
               rte


copy_custom_paula_to_buffers_paula_asynchrones:
		lea					Paula_custom,a1
		lea					buffers_paula_asynchrones,a2
		movem.l			(a1)+,d0/d2-d7/a0/a3-a6			; 12*4
		movem.l			d0/d2-d7/a0/a3-a6,(a2)
		movem.l			(a1)+,d0/d2-d4							; 4*4
		movem.l			d0/d2-d4,(12*4)(a2)
		
		;clear
		lea					Paula_custom,a1
		moveq			#0,d0
		.rept				16
		move.l			d0,(a1)+
		.endr
		
		rts


mt_music_copy_volume_and_enable_DMA:
;gestion dma
					move.w			mt_DMACONtemp,d0

					btst				#0,d0
					beq.s			.dmavoie1
; copie location + length dans interne					
					move.l		channela,channela+12
					move.w		channela+4,channela+10
                   LEA        mt_chan1temp(PC),A6
                   MOVE.L     n_loopstart(A6),channela				; location 
                   MOVE.W     n_replen(A6),channela+4			; length
					

.dmavoie1:
					btst				#1,d0
					beq.s			.dmavoie2
; copie location + length dans interne					
					move.l		channelb,channelb+12
					move.w		channelb+4,channelb+10
                   LEA        mt_chan2temp(PC),A6
                   MOVE.L     n_loopstart(A6),channelb				; location 
                   MOVE.W     n_replen(A6),channelb+4			; length

.dmavoie2:
					btst				#2,d0
					beq.s			.dmavoie3
; copie location + length dans interne					
					move.l		channelc,channelc+12
					move.w		channelc+4,channelc+10
                   LEA        mt_chan3temp(PC),A6
                   MOVE.L     n_loopstart(A6),channelc				; location 
                   MOVE.W     n_replen(A6),channelc+4			; length

.dmavoie3:
					btst				#3,d0
					beq.s			.dmavoie4
; copie location + length dans interne					
					move.l		channeld,channeld+12
					move.w		channeld+4,channeld+10
                   LEA        mt_chan4temp(PC),A6
                   MOVE.L     n_loopstart(A6),channeld				; location 
                   MOVE.W     n_replen(A6),channeld+4			; length

.dmavoie4:

                   move.w     mt_chan1temp+n_realvolume(pc),Paula_custom_V1
                   move.w     mt_chan2temp+n_realvolume(pc),Paula_custom_V2
                   move.w     mt_chan3temp+n_realvolume(pc),Paula_custom_V3
                   move.w     mt_chan4temp+n_realvolume(pc),Paula_custom_V4
	rts

mt_music_copy_loop_pointers:
				
                   clr.w      mt_DMACONtemp
                   clr.w      mt_LoadPointer
					rts


plays_speech_sfx:
; d1=sfx channel ( 0=left / 1=right)
; volume = 63 / period = $240
			lea			sample,a0
			moveq		#0,d1

			moveq		#0,d4
			lea			PAULA_SFX_left,a1
			cmp.w		#1,d1
			bne.s		.left
			lea			PAULA_SFX_right,a1
.left:
			move.l		(a1),d6
			cmp.w		#0,d6
			bne.s		.exit					; deja un sfx sur la meme voie
			move.l		#63,d6
			move.l		#$240,d7

			move.l		#sample,d3
			lsl.l			#8,d3
			lsl.l			#nb_bits_virgule_offset-8,d3		; location << nb_bits_virgule_offset
			
			move.l		#fin_sample,d5
			lsl.l			#8,d5
			lsl.l			#nb_bits_virgule_offset-8,d5		; end << nb_bits_virgule_offset

			movem.l	d3-d7,(a1)
.exit:			
			rts


		rts


; ---------------------------------------
; print pads status 
; Pads : mask = xxxxxxCx xxBx2580 147*oxAP 369#RLDU
print_pads_status:
	
	move.l		DSP_pad1,d1
	lea			string_pad_status,a0
	move.l		#31,d6

.boucle:	
	moveq		#0,d0
	btst.l		d6,d1
	beq.s		.print_space
	move.b		(a0)+,d0
	bsr			print_caractere
	bra.s		.ok
.print_space:
	move.b		#'.',d0
	bsr			print_caractere
	lea			1(a0),a0
.ok:
	dbf			d6,.boucle

; ligne suivante
	moveq		#10,d0
	bsr			print_caractere
	
print_pads_status_pad2:
; pad2
	move.l		DSP_pad2,d1
	lea			string_pad_status,a0
	move.l		#31,d6

.boucle2:	
	moveq		#0,d0
	btst.l		d6,d1
	beq.s		.print_space2
	move.b		(a0)+,d0
	bsr			print_caractere
	bra.s		.ok2
.print_space2:
	move.b		#'.',d0
	bsr			print_caractere
	lea			1(a0),a0
.ok2:
	dbf			d6,.boucle2

; ligne suivante
	moveq		#10,d0
	bsr			print_caractere
	

	rts

string_pad_status:		dc.b		"......CE..BD2580147*oFAp369#RLDU"
		even

; ---------------------------------------
; imprime une chaine terminée par un zéro
; a0=pointeur sur chaine
print_string:
	movem.l d0-d7/a0-a6,-(a7)	

print_string_boucle:
	moveq	#0,d0
	move.b	(a0)+,d0
	cmp.w	#0,d0
	bne.s	print_string_pas_fin_de_chaine
	movem.l (a7)+,d0-d7/a0-a6
	rts
print_string_pas_fin_de_chaine:
	bsr		print_caractere
	bra.s	print_string_boucle

; ---------------------------------------
; imprime un nombre HEXA de 2 chiffres
print_nombre_hexa_2_chiffres:
	movem.l d0-d7/a0-a6,-(a7)
	lea		convert_hexa,a0
	move.l		d0,d1
	divu		#16,d0
	and.l		#$F,d0			; limite a 0-15
	move.l		d0,d2
	mulu		#16,d2
	sub.l		d2,d1
	move.b		(a0,d0.w),d0
	bsr			print_caractere
	move.l		d1,d0
	and.l		#$F,d0			; limite a 0-15
	move.b		(a0,d0.w),d0
	bsr			print_caractere
	movem.l (a7)+,d0-d7/a0-a6
	rts
	
convert_hexa:
	dc.b		48,49,50,51,52,53,54,55,56,57
	dc.b		65,66,67,68,69,70
	even
	
; ---------------------------------------
; imprime un nombre de 2 chiffres
print_nombre_2_chiffres:
	movem.l d0-d7/a0-a6,-(a7)
	move.l		d0,d1
	divu		#10,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#10,d2
	sub.l		d2,d1
	cmp.l		#0,d0
	beq.s		.zap
	add.l		#48,d0
	bsr			print_caractere
.zap:
	move.l		d1,d0
	add.l		#48,d0
	bsr			print_caractere
	movem.l (a7)+,d0-d7/a0-a6
	rts

; ---------------------------------------
; imprime un nombre de 3 chiffres
print_nombre_3_chiffres:
	movem.l d0-d7/a0-a6,-(a7)
	move.l		d0,d1

	divu		#100,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#100,d2
	sub.l		d2,d1
	cmp.l		#0,d0
	beq.s		.zap
	add.l		#48,d0
	bsr			print_caractere
.zap:
	move.l		d1,d0	
	divu		#10,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#10,d2
	sub.l		d2,d1
	add.l		#48,d0
	bsr			print_caractere
	
	move.l		d1,d0
	add.l		#48,d0
	bsr			print_caractere
	movem.l (a7)+,d0-d7/a0-a6
	rts


; ---------------------------------------
; imprime un nombre de 2 chiffres , 00
print_nombre_2_chiffres_force:
	movem.l d0-d7/a0-a6,-(a7)
	move.l		d0,d1
	divu		#10,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#10,d2
	sub.l		d2,d1
	add.l		#48,d0
	bsr			print_caractere
	move.l		d1,d0
	add.l		#48,d0
	bsr			print_caractere
	movem.l (a7)+,d0-d7/a0-a6
	rts

; ---------------------------------------
; imprime un nombre de 4 chiffres HEXA
print_nombre_hexa_4_chiffres:
	movem.l d0-d7/a0-a6,-(a7)
	move.l		d0,d1
	lea		convert_hexa,a0

	divu		#4096,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#4096,d2
	sub.l		d2,d1
	move.b		(a0,d0.w),d0
	bsr			print_caractere

	move.l		d1,d0
	divu		#256,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#256,d2
	sub.l		d2,d1
	move.b		(a0,d0.w),d0
	bsr			print_caractere


	move.l		d1,d0
	divu		#16,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#16,d2
	sub.l		d2,d1
	move.b		(a0,d0.w),d0
	bsr			print_caractere
	move.l		d1,d0
	move.b		(a0,d0.w),d0
	bsr			print_caractere
	movem.l (a7)+,d0-d7/a0-a6
	rts

; ---------------------------------------
; imprime un nombre de 6 chiffres HEXA ( pour les adresses memoire)
print_nombre_hexa_6_chiffres:
	movem.l d0-d7/a0-a6,-(a7)
	
	move.l		d0,d1
	lea		convert_hexa,a0

	move.l		d1,d0
	swap		d0
	and.l		#$F0,d0
	divu		#16,d0
	and.l		#$F,d0
	move.b		(a0,d0.w),d0
	and.l		#$FF,d0
	bsr			print_caractere

	move.l		d1,d0
	swap		d0
	and.l		#$F,d0
	move.b		(a0,d0.w),d0
	and.l		#$FF,d0
	bsr			print_caractere

	and.l		#$FFFF,d1
	move.l		d1,d0
	divu		#4096,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#4096,d2
	sub.l		d2,d1
	move.b		(a0,d0.w),d0
	bsr			print_caractere

	move.l		d1,d0
	divu		#256,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#256,d2
	sub.l		d2,d1
	move.b		(a0,d0.w),d0
	bsr			print_caractere


	move.l		d1,d0
	divu		#16,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#16,d2
	sub.l		d2,d1
	move.b		(a0,d0.w),d0
	bsr			print_caractere
	move.l		d1,d0
	move.b		(a0,d0.w),d0
	bsr			print_caractere
	movem.l (a7)+,d0-d7/a0-a6
	rts

; ---------------------------------------
; imprime un nombre de 8 chiffres HEXA ( pour les adresses memoire et les données en 16:16)
print_nombre_hexa_8_chiffres:
	movem.l d0-d7/a0-a6,-(a7)
	
	move.l		d0,d1
	lea		convert_hexa,a0

	move.l		d1,d0
	swap		d0
	and.l		#$F000,d0
	divu		#4096,d0
	and.l		#$F,d0
	move.b		(a0,d0.w),d0
	and.l		#$FF,d0
	bsr			print_caractere



	move.l		d1,d0
	swap		d0
	and.l		#$F00,d0
	divu		#256,d0
	and.l		#$F,d0
	move.b		(a0,d0.w),d0
	and.l		#$FF,d0
	bsr			print_caractere


	move.l		d1,d0
	swap		d0
	and.l		#$F0,d0
	divu		#16,d0
	and.l		#$F,d0
	move.b		(a0,d0.w),d0
	and.l		#$FF,d0
	bsr			print_caractere

	move.l		d1,d0
	swap		d0
	and.l		#$F,d0
	move.b		(a0,d0.w),d0
	and.l		#$FF,d0
	bsr			print_caractere

	and.l		#$FFFF,d1
	move.l		d1,d0
	divu		#4096,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#4096,d2
	sub.l		d2,d1
	move.b		(a0,d0.w),d0
	bsr			print_caractere

	move.l		d1,d0
	divu		#256,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#256,d2
	sub.l		d2,d1
	move.b		(a0,d0.w),d0
	bsr			print_caractere


	move.l		d1,d0
	divu		#16,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#16,d2
	sub.l		d2,d1
	move.b		(a0,d0.w),d0
	bsr			print_caractere
	move.l		d1,d0
	move.b		(a0,d0.w),d0
	bsr			print_caractere
	movem.l (a7)+,d0-d7/a0-a6
	rts


; ---------------------------------------
; imprime un nombre de 4 chiffres
print_nombre_4_chiffres:
	movem.l d0-d7/a0-a6,-(a7)
	move.l		d0,d1

	divu		#1000,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#1000,d2
	sub.l		d2,d1
	add.l		#48,d0
	bsr			print_caractere

	move.l		d1,d0
	divu		#100,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#100,d2
	sub.l		d2,d1
	add.l		#48,d0
	bsr			print_caractere


	move.l		d1,d0
	divu		#10,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#10,d2
	sub.l		d2,d1
	add.l		#48,d0
	bsr			print_caractere
	move.l		d1,d0
	add.l		#48,d0
	bsr			print_caractere
	movem.l (a7)+,d0-d7/a0-a6
	rts

; ---------------------------------------
; imprime un nombre de 5 chiffres
print_nombre_5_chiffres:
	movem.l d0-d7/a0-a6,-(a7)
	move.l		d0,d1

	divu		#10000,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#10000,d2
	sub.l		d2,d1
	add.l		#48,d0
	bsr			print_caractere

	move.l		d1,d0
	divu		#1000,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#1000,d2
	sub.l		d2,d1
	add.l		#48,d0
	bsr			print_caractere

	move.l		d1,d0
	divu		#100,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#100,d2
	sub.l		d2,d1
	add.l		#48,d0
	bsr			print_caractere


	move.l		d1,d0
	divu		#10,d0
	and.l		#$FF,d0
	move.l		d0,d2
	mulu		#10,d2
	sub.l		d2,d1
	add.l		#48,d0
	bsr			print_caractere
	move.l		d1,d0
	add.l		#48,d0
	bsr			print_caractere
	movem.l (a7)+,d0-d7/a0-a6
	rts


; -----------------------------
; copie un caractere a l ecran
; d0.w=caractere

print_caractere:
	movem.l d0-d7/a0-a6,-(a7)



	cmp.b	#00,d0
	bne.s	print_caractere_pas_CLS
	move.l	#ecran1,A1_BASE			; = DEST
	move.l	#$0,A1_PIXEL
	move.l	#PIXEL16|XADDPHR|PITCH1,A1_FLAGS
	move.l	#ecran1+320*100,A2_BASE			; = source
	move.l	#$0,A2_PIXEL
	move.l	#PIXEL16|XADDPHR|PITCH1,A2_FLAGS
	
	move.w	#$00,B_PATD
	

	moveq	#0,d0
	move.w	#nb_octets_par_ligne,d0
	lsr.w	#1,d0
	move.w	#nb_lignes,d1
	mulu	d1,d0
	swap	d0
	move.w	#1,d0
	swap	d0
	;move.w	#65535,d0
	move.l	d0,B_COUNT
	move.l	#LFU_REPLACE|SRCEN|PATDSEL,B_CMD


	movem.l (a7)+,d0-d7/a0-a6
	rts
	
print_caractere_pas_CLS:

	cmp.b	#10,d0
	bne.s	print_caractere_pas_retourchariot
	move.w	#0,curseur_x
	add.w	#8,curseur_y
	movem.l (a7)+,d0-d7/a0-a6
	rts

print_caractere_pas_retourchariot:
	cmp.b	#09,d0
	bne.s	print_caractere_pas_retourdebutligne
	move.w	#0,curseur_x
	movem.l (a7)+,d0-d7/a0-a6
	rts

print_caractere_pas_retourdebutligne:
	cmp.b	#08,d0
	bne.s	print_caractere_pas_retourdebutligneaudessus
	move.w	#0,curseur_x
	sub.w	#8,curseur_y
	movem.l (a7)+,d0-d7/a0-a6
	rts


print_caractere_pas_retourdebutligneaudessus:

	lea		ecran1,a1
	moveq	#0,d1
	move.w	curseur_x,d1
	add.l	d1,a1
	moveq	#0,d1
	move.w	curseur_y,d1
	mulu	#nb_octets_par_ligne,d1
	add.l	d1,a1

	lsl.l	#3,d0		; * 8
	lea		fonte,a0
	add.l	d0,a0
	
	
; copie 1 lettre
	move.l	#8-1,d0
copieC_ligne:
	moveq	#8-1,d1
	move.b	(a0)+,d2
copieC_colonne:
	moveq	#0,d4
	btst	d1,d2
	beq.s	pixel_a_zero
	moveq	#0,d4
	move.w	couleur_char,d4
pixel_a_zero:
	move.b	d4,(a1)+
	dbf		d1,copieC_colonne
	lea		nb_octets_par_ligne-8(a1),a1
	dbf		d0,copieC_ligne

	move.w	curseur_x,d0
	add.w	#8,d0
	cmp.w	#320,d0
	blt		curseur_pas_fin_de_ligne
	moveq	#0,d0
	add.w	#8,curseur_y
curseur_pas_fin_de_ligne:
	move.w	d0,curseur_x

	movem.l (a7)+,d0-d7/a0-a6

	rts


;----------------------------------
; recopie l'object list dans la courante

copy_olist:
				move.l	#ob_list_courante,A1_BASE			; = DEST
				move.l	#$0,A1_PIXEL
				move.l	#PIXEL16|XADDPHR|PITCH1,A1_FLAGS
				move.l	#ob_liste_originale,A2_BASE			; = source
				move.l	#$0,A2_PIXEL
				move.l	#PIXEL16|XADDPHR|PITCH1,A2_FLAGS
				move.w	#1,d0
				swap	d0
				move.l	#fin_ob_liste_originale-ob_liste_originale,d1
				move.w	d1,d0
				move.l	d0,B_COUNT
				move.l	#LFU_REPLACE|SRCEN,B_CMD
				rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Procedure: InitVideo (same as in vidinit.s)
;;            Build values for hdb, hde, vdb, and vde and store them.
;;

InitVideo:
                movem.l d0-d6,-(sp)

				
				move.w	#-1,ntsc_flag
				move.l	#50,_50ou60hertz
	
				move.w  CONFIG,d0                ; Also is joystick register
                andi.w  #VIDTYPE,d0              ; 0 = PAL, 1 = NTSC
                beq     .palvals
				move.w	#1,ntsc_flag
				move.l	#60,_50ou60hertz
	

.ntscvals:		move.w  #NTSC_HMID,d2
                move.w  #NTSC_WIDTH,d0

                move.w  #NTSC_VMID,d6
                move.w  #NTSC_HEIGHT,d4
				
                bra     calc_vals
.palvals:
				move.w #PAL_HMID,d2
				move.w #PAL_WIDTH,d0

				move.w #PAL_VMID,d6				
				move.w #PAL_HEIGHT,d4

				
calc_vals:		
                move.w  d0,width
                move.w  d4,height
                move.w  d0,d1
                asr     #1,d1                   ; Width/2
                sub.w   d1,d2                   ; Mid - Width/2
                add.w   #4,d2                   ; (Mid - Width/2)+4
                sub.w   #1,d1                   ; Width/2 - 1
                ori.w   #$400,d1                ; (Width/2 - 1)|$400
                move.w  d1,a_hde
                move.w  d1,HDE
                move.w  d2,a_hdb
                move.w  d2,HDB1
                move.w  d2,HDB2
                move.w  d6,d5
                sub.w   d4,d5
                add.w   #16,d5
                move.w  d5,a_vdb
                add.w   d4,d6
                move.w  d6,a_vde
			
			    move.w  a_vdb,VDB
				move.w  a_vde,VDE    
				
				
				move.l  #0,BORD1                ; Black border
                move.w  #0,BG                   ; Init line buffer to black
                movem.l (sp)+,d0-d6
                rts

; - routines de debug
;
bus_error_68000:
	;.if			GD_DEBUG=1
	;lea			debug_USB_GD__bus_error,a0
	;jsr			fonction_GD_DebugString
	;.endif
	moveq		#0,d0
bus_error_68000_2:
	move.w	d0,BG						
	move.w	d0,BORD1
	addq.w	#1,d0
	bra.s	bus_error_68000_2

address_error_68000:
	;.if			GD_DEBUG=1
	;lea			debug_USB_GD__address_error,a0
	;jsr			fonction_GD_DebugString
	;.endif
	moveq		#0,d0
address_error_68000_2:
	move.w	d0,BG						
	addq.w	#1,d0
	bra.s	address_error_68000_2


	.68000
	.dphrase
	.text

	.phrase
	.include	"Paula_v2_1_include.s"
	
	.68000
	.text
	
	.rept				7
	dc.l				0
	.endr
	
	.data
	
	
	.dphrase
Paula_custom:
channela:	; $dff0a0
; total = 4+2+2+8 = 16
		dc.l		silence			; adresse debut sample .L														00
		dc.w		4				; taille en words du sample .W												04
		dc.w		0				; period/note du canal																06
Paula_custom_V1:
		dc.w		0				; volume .W 																					08
		dc.w		0				; length interne																			10
		dc.l			0		; location interne																		12
channelb:
		dc.l		silence			; adresse debut sample .L														16
		dc.w		4				; taille en words du sample .W												20
		dc.w		0				; period/note du canal																22
Paula_custom_V2:
		dc.w		0				; volume .W 																					24
		dc.w		0				; length interne																			26
		dc.l			0			; location interne																		28
channelc:
		dc.l		silence			; adresse debut sample .L														32
		dc.w		4				; taille en words du sample .W												36
		dc.w		0				; period/note du canal																38
Paula_custom_V3:
		dc.w		0				; volume .W 																					40
		dc.w		0				; length interne																			42
		dc.l			0		; location interne																		44
channeld:
		dc.l		silence			; adresse debut sample .L														48
		dc.w		4				; taille en words du sample .W												52
		dc.w		0				; period/note du canal																54
Paula_custom_V4:
		dc.w		0				; volume .W 																					56
		dc.w		0				; length interne																			58
		dc.l			0		; location interne																		60


chaine_HIPPEL:						dc.b	"protracker player for Jaguar  0.1 ",10,0
chaine_Hz_init_LSP:				dc.b	" Hz.",10,0
chaine_replay_frequency:		dc.b	"Replay frequency : ",0
chaine_frequency_correction:	dc.b	"Frequency correction : ",0
chaine_replay_volumes:			dc.b	"volumes music/SFX : ",0
		.phrase

        .68000
		.dphrase
ob_liste_originale:           				 ; This is the label you will use to address this in 68K code
        .objproc 							   ; Engage the OP assembler
		.dphrase

        .org    ob_list_courante			 ; Tell the OP assembler where the list will execute
;
        branch      VC < 0, .stahp    			 ; Branch to the STOP object if VC < 0
        branch      VC > 265, .stahp   			 ; Branch to the STOP object if VC > 241
			; bitmap data addr, xloc, yloc, dwidth, iwidth, iheight, bpp, pallete idx, flags, firstpix, pitch
        bitmap      ecran1, 16, 26, nb_octets_par_ligne/8, nb_octets_par_ligne/8, 246-26,3
		;bitmap		ecran1,16,24,40,40,255,3
        jump        .haha
.stahp:
        stop
.haha:
        jump        .stahp
		
		.68000
		.dphrase
fin_ob_liste_originale:


			.data		
			
	.dphrase

stoplist:		dc.l	0,4

fonte:	
	.include	"fonte1plan.s"
	even

couleur_char:				dc.w		25
curseur_x:					dc.w		0
curseur_y:					dc.w		curseur_Y_min
compteur_frame_music:			dc.w				0
		even



module_amiga:
		incbin				"ELYSIUM.MOD"
		.even

sample:
		incbin				"YIPPEE.raw"					; 8 bits signés, 8000Hz
fin_sample:



mt_FunkTable:       dc.b       0,5,6,7,8,10,11,13,16,19,22,26,32,43,64,128

mt_VibratoTable:
                   dc.b       0, 24, 49, 74, 97,120,141,161
                   dc.b       180,197,212,224,235,244,250,253
                   dc.b       255,253,250,244,235,224,212,197
                   dc.b       180,161,141,120, 97, 74, 49, 24

mt_PeriodTable:
; Tuning 0, Normal
                   dc.w       856,808,762,720,678,640,604,570,538,508,480,453
                   dc.w       428,404,381,360,339,320,302,285,269,254,240,226
                   dc.w       214,202,190,180,170,160,151,143,135,127,120,113
; Tuning 1
                   dc.w       850,802,757,715,674,637,601,567,535,505,477,450
                   dc.w       425,401,379,357,337,318,300,284,268,253,239,225
                   dc.w       213,201,189,179,169,159,150,142,134,126,119,113
; Tuning 2
                   dc.w       844,796,752,709,670,632,597,563,532,502,474,447
                   dc.w       422,398,376,355,335,316,298,282,266,251,237,224
                   dc.w       211,199,188,177,167,158,149,141,133,125,118,112
; Tuning 3
                   dc.w       838,791,746,704,665,628,592,559,528,498,470,444
                   dc.w       419,395,373,352,332,314,296,280,264,249,235,222
                   dc.w       209,198,187,176,166,157,148,140,132,125,118,111
; Tuning 4
                   dc.w       832,785,741,699,660,623,588,555,524,495,467,441
                   dc.w       416,392,370,350,330,312,294,278,262,247,233,220
                   dc.w       208,196,185,175,165,156,147,139,131,124,117,110
; Tuning 5
                   dc.w       826,779,736,694,655,619,584,551,520,491,463,437
                   dc.w       413,390,368,347,328,309,292,276,260,245,232,219
                   dc.w       206,195,184,174,164,155,146,138,130,123,116,109
; Tuning 6
                   dc.w       820,774,730,689,651,614,580,547,516,487,460,434
                   dc.w       410,387,365,345,325,307,290,274,258,244,230,217
                   dc.w       205,193,183,172,163,154,145,137,129,122,115,109
; Tuning 7
                   dc.w       814,768,725,684,646,610,575,543,513,484,457,431
                   dc.w       407,384,363,342,323,305,288,272,256,242,228,216
                   dc.w       204,192,181,171,161,152,144,136,128,121,114,108
; Tuning -8
                   dc.w       907,856,808,762,720,678,640,604,570,538,508,480
                   dc.w       453,428,404,381,360,339,320,302,285,269,254,240
                   dc.w       226,214,202,190,180,170,160,151,143,135,127,120
; Tuning -7
                   dc.w       900,850,802,757,715,675,636,601,567,535,505,477
                   dc.w       450,425,401,379,357,337,318,300,284,268,253,238
                   dc.w       225,212,200,189,179,169,159,150,142,134,126,119
; Tuning -6
                   dc.w       894,844,796,752,709,670,632,597,563,532,502,474
                   dc.w       447,422,398,376,355,335,316,298,282,266,251,237
                   dc.w       223,211,199,188,177,167,158,149,141,133,125,118
; Tuning -5
                   dc.w       887,838,791,746,704,665,628,592,559,528,498,470
                   dc.w       444,419,395,373,352,332,314,296,280,264,249,235
                   dc.w       222,209,198,187,176,166,157,148,140,132,125,118
; Tuning -4
                   dc.w       881,832,785,741,699,660,623,588,555,524,494,467
                   dc.w       441,416,392,370,350,330,312,294,278,262,247,233
                   dc.w       220,208,196,185,175,165,156,147,139,131,123,117
; Tuning -3
                   dc.w       875,826,779,736,694,655,619,584,551,520,491,463
                   dc.w       437,413,390,368,347,328,309,292,276,260,245,232
                   dc.w       219,206,195,184,174,164,155,146,138,130,123,116
; Tuning -2
                   dc.w       868,820,774,730,689,651,614,580,547,516,487,460
                   dc.w       434,410,387,365,345,325,307,290,274,258,244,230
                   dc.w       217,205,193,183,172,163,154,145,137,129,122,115
; Tuning -1
                   dc.w       862,814,768,725,684,646,610,575,543,513,484,457
                   dc.w       431,407,384,363,342,323,305,288,272,256,242,228
                   dc.w       216,203,192,181,171,161,152,144,136,128,121,114




	.bss

DEBUT_BSS:




;------------------------------------------------------

























	.phrase
taille_une_entree_buffer_asynchrone = 16	
; 4*
	; - .L : location / externe					00
	; - .W : length / externe						04
	; - .W : note/period									06
	; - .W : volume												08
	; - .W : length / interne						10				quand le dmacon est forcé
	; - .L : location / interne					12				quand le dmacon est forcé
	; => 16

buffers_paula_asynchrones:
		ds.b				taille_une_entree_buffer_asynchrone*4



_50ou60hertz:			ds.l	1
ntsc_flag:				ds.w	1
a_hdb:          		ds.w   1
a_hde:          		ds.w   1
a_vdb:          		ds.w   1
a_vde:          		ds.w   1
width:          		ds.w   1
height:         		ds.w   1
taille_liste_OP:		ds.l	1
vbl_counter:			ds.l	1

            .dphrase
ecran1:				ds.w		320*256				; 8 bitplanes
FIN_RAM:
