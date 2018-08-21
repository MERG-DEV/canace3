    TITLE   "Source for CAN accessory encoder using CBUS"
; filename CANACE3f.asm

; A control panel encoder for the SLiM model
; Uses two DIP switches to set NN and CAN ID
; Scans 128 toggles or 64 dual PBs, selected by a jumper
; Contains CAN ID read facility but no self enumeration
; Works with toggle switches
; Works with PBs
; Sends ON and OFF events
; Event numbers start at 1 and are sequential with columms and first 4 rows
; then columns and second 4 rows. Makes panel wiring simpler.
; 
; Present code and PCB has a two bit NN select capability.
; This would allow for 4 separate control panels, each with 128 switches.
; Note, NN starts at 1 so node has settable NNs from 1 to 4 (not 0 to 3)
; This keeps the default CAN ID of 0 for the 'base node' which usually would
; be the PC or CAN-RS adapter
; Tested and working  09/10/07
; 08/01/08. Fixed ON and OFF the wrong way round.
; Added RUN LED 
; Added change of NN while running

;CANACE3a   CANACE3 with the self enumeration removed.
;changed movff in load of TX buffer.
;CANACE3b is CANACE3a but with removal of toggle requirement for PB mode
;working 01/02/08   
;rewrite to not use EEPROM  - OK
;CANACE3c  mods to CANACE8b to correct toggle mode startup problem.
;seems OK  30/07/08
;CANACE3d is CANACE3c with RTR response added back for FLiM compatibility (31/08/08)
;added clear of Tx1 buffer. now rev e  12/09/08
;changed sendTX1 so message is not aborted.  Now rev f  (25/03/09)










; 
; Assembly options
  LIST  P=18F2480,r=hex,N=75,C=120,T=ON

  include   "p18f2480.inc"
  
  ;definitions  Change these to suit hardware.
  
S_PORT  equ  PORTB  ; port used for config.
S_BIT0  equ  4    ; bits used for node number and CAN ID
S_BIT1  equ  5
M_PORT  equ  PORTA
M_BIT equ  5    ;mode toggle or PB

;set config registers

; note. there seem to be differences in the naming of the CONFIG parameters between
; versions of the p18F2480.inf files

  CONFIG  FCMEN = OFF, OSC = HSPLL, IESO = OFF
  CONFIG  PWRT = ON,BOREN = BOHW, BORV=0
  CONFIG  WDT=OFF
  CONFIG  MCLRE = ON
  CONFIG  LPT1OSC = OFF, PBADEN = OFF
  CONFIG  DEBUG = OFF
  CONFIG  XINST = OFF,LVP = OFF,STVREN = ON,CP0 = OFF
  CONFIG  CP1 = OFF, CPB = OFF, CPD = OFF,WRT0 = OFF,WRT1 = OFF, WRTB = OFF
  CONFIG  WRTC = OFF,WRTD = OFF, EBTR0 = OFF, EBTR1 = OFF, EBTRB = OFF
  
;original CONFIG settings left here for reference
  
; __CONFIG  _CONFIG1H,  B'00100110' ;oscillator HS with PLL
; __CONFIG  _CONFIG2L,  B'00001110' ;brown out voltage and PWT  
; __CONFIG  _CONFIG2H,  B'00000000' ;watchdog time and enable (disabled for now)
; __CONFIG  _CONFIG3H,  B'10000000' ;MCLR enable  
; __CONFIG  _CONFIG4L,  B'10000001' ;B'10000001'  for   no debug
; __CONFIG  _CONFIG5L,  B'00001111' ;code protection (off)  
; __CONFIG  _CONFIG5H,  B'11000000' ;code protection (off)  
; __CONFIG  _CONFIG6L,  B'00001111' ;write protection (off) 
; __CONFIG  _CONFIG6H,  B'11100000' ;write protection (off) 
; __CONFIG  _CONFIG7L,  B'00001111' ;table read protection (off)  
; __CONFIG  _CONFIG7H,  B'01000000' ;boot block protection (off)





; processor uses 4 MHz resonator but clock is 16 MHz.

;********************************************************************************



;****************************************************************
; define RAM storage
  
  CBLOCK  0   ;file registers - access bank
          ;interrupt stack for low priority
          ;hpint uses fast stack
  W_tempL
  St_tempL
  Bsr_tempL
  PCH_tempH   ;save PCH in hpint
  PCH_tempL   ;save PCH in lpint (if used)
  Fsr_temp0L
  Fsr_temp0H 
  Fsr_temp1L
  Fsr_temp1H 
  Fsr_temp2L
  Fsr_hold
  Fsr_holx
  TempCANCON
  TempCANSTAT
  Datmode     ;flag for data waiting 
  Count     ;counter for loading
  Count1
  Dcount      ;used in delay only
  Dcount1
  Latcount    ;latency counter
  NodeID_h    ;holds the node ID number (16 bit)
  NodeID_l
  Temp      ;temps
  Temp1
  Atemp     ;Port temp value
  CANid     ;CAN ID from switches 
  CanID_tmp   ;temp for CAN Node ID
  IDtemph     ;used in ID shuffle
  IDtempl
  Eadr    ;temp eeprom address
  Column    ;column counter for keyboard scan
  

          ;the above variables must be in access space (00 to 5F)
            
  Buffer      ;temp buffer in access bank for data  
  Buffer1
  Buffer2
  Buffer3
  Buffer4   
  Buffer5
  Buffer6
  Buffer7
  Buffer8   
  Buffer9
  Buffer10
  Buffer11
  Buffer12    
  Buffer13
  Buffer14
  Buffer15
    
  
  
  
  
  

  ENDC      ;ends at 5F 
  

  
  CBLOCK  h'60' ;rest of bank 0
  
  Rx0con      ;start of receive packet 0
  Rx0sidh
  Rx0sidl
  Rx0eidh
  Rx0eidl
  Rx0dlc
  Rx0d0
  Rx0d1
  Rx0d2
  Rx0d3
  Rx0d4
  Rx0d5
  Rx0d6
  Rx0d7
  

  
  Tx1con      ;start of transmit frame  1
  Tx1sidh
  Tx1sidl
  Tx1eidh
  Tx1eidl
  Tx1dlc
  Tx1d0
  Tx1d1
  Tx1d2
  Tx1d3
  Tx1d4
  Tx1d5
  Tx1d6
  Tx1d7
  
  
  
  
  
  
  
  

  
  
  
  
  
  
  ;add variables to suit
  


  
  
  
  ;***************************************************************
  ; the following are used in the switch scanning
  Ccount    ;column counter for switch scan
  Oldrow    ;original row data
  Row     ;row data for switch scan
  Bitcng    ;bit change in scan
  Bitcnt    ;bit counter in scan
  Pointno   ;calculated point number for scan
  Turn    ;direction of changed switch
  Rowmsk    ;row mask
  PointnL   ;low byte of 16 bit point number
  PointnH   ;hi byte
  
  
  
  

  
  
  
  
    
  ENDC

;****************************************************************
;
;   start of program code

    ORG   0000h
    nop           ;for debug
    goto  setup

    ORG   0008h
    goto  hpint     ;high priority interrupt

    ORG   0018h 
    goto  lpint     ;low priority interrupt


;*******************************************************************

    ORG   0020h     ;start of program
; 
;
;   high priority interrupt. Used for CAN transmit error and enum response.


hpint movff CANCON,TempCANCON
    movff CANSTAT,TempCANSTAT
  
    movff PCLATH,PCH_tempH    ;save PCLATH
    clrf  PCLATH
  
    movff FSR0L,Fsr_temp0L    ;save FSR0
    movff FSR0H,Fsr_temp0H
    movff FSR1L,Fsr_temp1L    ;save FSR1
    movff FSR1H,Fsr_temp1H
    
    

  
    movf  TempCANSTAT,W     ;Jump table
    andlw B'00001110'
    addwf PCL,F     ;jump
    bra   back
    bra   errint      ;error interrupt
    bra   back
    bra   back
    bra   back
    bra   rxb1int     ;only receive interrupts used
    bra   rxb0int
    bra   back
    
rxb1int bcf   PIR3,RXB1IF   ;uses RB0 to RB1 rollover so may never use this
    
    lfsr  FSR0,Rx0con   ;
    
    goto  access
    
rxb0int bcf   PIR3,RXB0IF
  
    lfsr  FSR0,Rx0con
    
    goto  access
    
    ;error routine here. Only acts on lost arbitration  
errint  movlb .15         ;change bank      
    btfss TXB1CON,TXLARB
    bra   errbak        ;not lost arb.
    movf  Latcount,F      ;is it already at zero?
    bz    errbak
    decfsz  Latcount,F
    bra   errbak
    movlw B'00111111'
    andwf TXB1SIDH,F      ;change priority
txagain bsf   TXB1CON,TXREQ   ;try again
          
errbak  movlb .15
    bcf   RXB1CON,RXFUL
    movlb 0
    bcf   RXB0CON,RXFUL ;ready for next
          
    bra   back1

access  movf  CANCON,W        ;switch buffers
    andlw B'11110001'
    movwf CANCON
    movf  TempCANSTAT,W
    andlw B'00001110'
    iorwf CANCON
    lfsr  FSR1,RXB0CON  ;this is switched bank
load  movf  POSTINC1,W
    movwf POSTINC0
    movlw 0x6E      ;end of access buffer lo byte
    cpfseq  FSR1L
    bra   load
    
    btfsc Rx0dlc,RXRTR    ;is it RTR?
    bra   isRTR       ;only RTR as it is a SLiM producer
    
    
back  movlb .15
    bcf   RXB1CON,RXFUL
    movlb 0
    bcf   RXB0CON,RXFUL ;ready for next
  
back1 clrf  PIR3      ;clear all flags
    movf  CANCON,W
    andlw B'11110001'
    iorwf TempCANCON,W
    
    movwf CANCON
    movff PCH_tempH,PCLATH
    movff Fsr_temp0L,FSR0L    ;recover FSR0
    movff Fsr_temp0H,FSR0H

    movff Fsr_temp1L,FSR1L    ;recover FSR1
    movff Fsr_temp1H,FSR1H

    
    retfie  1       ;use shadow registers
    
isRTR movlb .15
    bsf   TXB2CON,TXREQ ;send ID frame - preloaded in TXB2
isRTR1  btfsc TXB2CON,TXREQ ;wait till sent
    bra   isRTR1
    movlb 0
    bra   back



    


;**************************************************************
;
;
;   low priority interrupt. (if used)
; 

lpint retfie      ;not used
                

;*********************************************************************


    org   0x200     ;set to page boundary   


main  movlw B'00110000'   ;get NN from switches
    andwf S_PORT,W
    movwf Temp
    swapf Temp,W
    addlw 1
    cpfseq  Atemp
    call  setid1
  
doscan  call  scan      ;main switch scan routine
    goto  main
  
main1 goto  main      ;temp loop      
  
    
  org 0x400         ;start new page for jump table
  
                ;main packet handling is here
    

    
    
    
    
;***************************************************************************
;   main setup routine
;*************************************************************************

setup clrf  INTCON      ;no interrupts yet
    clrf  ADCON0      ;turn off A/D, all digital I/O
    movlw B'00001111'
    movwf ADCON1
    
    ;port settings will be hardware dependent. RB2 and RB3 are for CAN.
    ;set S_PORT and S_BIT to correspond to port used for setup.
    ;rest are hardware options
    
  
    movlw B'00110000'   ;Port A 0 to 3 are column select, 5 is mode input
    movwf TRISA     ;
    movlw B'00111001'   ;RB0 is setup RB2 = CANTX, RB3 = CANRX, RB4,5 are logic 
            ;input - RB6,7 for debug and diagnostics LEDs
    movwf TRISB
    bsf   PORTB,2     ;CAN recessive
    movlw B'11111111'   ;Port C  row inputs.
    movwf TRISC
    
; next segment is essential.
    
    bsf   RCON,IPEN   ;enable interrupt priority levels
    clrf  BSR       ;set to bank 0
    clrf  EECON1      ;no accesses to program memory  
    
    clrf  Latcount
    clrf  ECANCON     ;CAN mode 0 for now
     
    bsf   CANCON,7    ;CAN to config mode
    movlw B'00000011'   ;set CAN bit rate at 125000 for now
    movwf BRGCON1
    movlw B'10011110'   ;set phase 1 etc
    movwf BRGCON2
    movlw B'00000011'   ;set phase 2 etc
    movwf BRGCON3
    movlw B'00100000'
    movwf CIOCON      ;CAN to high when off
    movlw B'00100100'
    movwf RXB0CON     ;enable double buffer of RX0
    

    
mskload lfsr  0,RXM0SIDH    ;Clear masks, point to start
mskloop clrf  POSTINC0    
    movlw LOW RXM1EIDL+1    ;end of masks
    cpfseq  FSR0L
    bra   mskloop
    
  
    clrf  CANCON      ;out of CAN setup mode
    
    
    movlw B'00100011'
    movwf IPR3      ;high priority CAN RX and Tx error interrupts(for now)
    clrf  IPR1      ;all peripheral interrupts are low priority
    clrf  IPR2
    clrf  PIE2



;next segment required
    
  
    
    clrf  INTCON2     ;
    clrf  INTCON3     ;
    

    movlw B'00100011'   ;Rx0 and RX1 interrupt and Tx error
    movwf PIE3
    bsf   PIE1,RCIE   ;enable interrupt on RS232 input  
    clrf  PIR1
    clrf  PIR2
    clrf  PIR3      ;clear all flags
    bcf   RXB0CON,RXFUL ;enable RX0 buffer
    
    
    movlw .16       ;clear buffer
    movwf Count
    lfsr  FSR0,Buffer
bclear  clrf  POSTINC0  

    decfsz  Count
    bra   bclear
    clrf  Tx1con
    clrf  Tx1eidh
    clrf  Tx1eidl
;********************************************

;   initialise buffer for toggle mode   
    
    clrf  Ccount      ;column count
    lfsr  FSR0,Buffer
  
inscan1 movlw B'00001111'
    andwf Ccount,F
    movf  Ccount,W
    movwf PORTA     ;set columns
    call  dely      ;let column settle
    movf  Ccount,W
    movff PORTC,PLUSW0  ;put row data
    
  
inscan2 incf  Ccount
    btfss Ccount,4    ;more than 15?
    bra   inscan1     ;next column
                ;finish scan        
  
;**********************************************   
setid movlw B'00110000'   ;get NN from switches
    andwf S_PORT,W
    movwf Temp
    swapf Temp,W
    addlw 1
    movwf Atemp       ;avoids NN of 0
    call  setid1
    
    bsf PORTB,7     ;put RUNNING light on (if fitted)
    movlw B'11000000'
    movwf INTCON      ;enable interrupts
      
main4 goto  main  
    
;****************************************************************************
;   start of subroutines  

;   Sets new Node ID from switches. Called during setup and if any changes while running.

setid1    movwf CANid     ;set CAN ID and NN from DIP switches
    movwf NodeID_l
    movwf Atemp
    movwf Tx1d2     ;preset NN in Tx buffer
    clrf  Tx1d1
    call  newid     ;put in TXB2 for RTR  
    return


;****************************************************************** 

;   Send contents of Tx1 buffer via CAN TXB1

sendTX1 lfsr  FSR0,Tx1con
    lfsr  FSR1,TXB1CON
    movlb .15       ;check for buffer access
tx1test   btfsc TXB1CON,TXREQ
    bra tx1test
    movlb 0
ldTX1 movf  POSTINC0,W
    movwf POSTINC1    ;load TXB1
    movlw Tx1d7+1
    cpfseq  FSR0L
    bra   ldTX1
    movlb .15       ;bank 15
    bsf   TXB1CON,TXREQ ;OK so send
    
tx1done movlb 0       ;bank 0
    return          ;successful send

    
    







    
;*****************************************************************************
;
;   shuffle for standard ID. Puts 8 bit ID into IDtemph and IDtempl for CAN frame
shuffle movf  CanID_tmp,W
    movwf IDtempl   ;get 8 bit ID
    swapf IDtempl,F
    rlncf IDtempl,W
    andlw B'11100000'
    movwf IDtempl         ;has sidl
    movf  CanID_tmp,W
    movwf IDtemph
    rrncf IDtemph,F
    rrncf IDtemph,F
    rrncf IDtemph,W
    andlw B'00011111'
    movwf IDtemph         ;has sidh
    return



;************************************************************************************
;   
eeread  bcf   EECON1,EEPGD  ;read a EEPROM byte, EEADR must be set before this sub.
    bcf   EECON1,CFGS
    bsf   EECON1,RD
    movf  EEDATA,W
    return

;**************************************************************************
eewrite movwf EEDATA      ;write to EEPROM, EEADR must be set before this sub.
    bcf   EECON1,EEPGD
    bcf   EECON1,CFGS
    bsf   EECON1,WREN
    
    clrf  INTCON  ;disable interrupts
    movlw 0x55
    movwf EECON2
    movlw 0xAA
    movwf EECON2
    bsf   EECON1,WR
eetest  btfsc EECON1,WR
    bra   eetest
    bcf   PIR2,EEIF
    bcf   EECON1,WREN
    movlw B'11000000'
    movwf INTCON    ;reenable interrupts
    
    return  
    

;*********************************************************
;   a delay routine
      
dely  movlw .10
    movwf Dcount1
dely2 clrf  Dcount
dely1 decfsz  Dcount,F
    goto  dely1
    decfsz  Dcount1
    bra   dely2
    return    
    
;************************************************************************ 



;   scans all 16 columns  

scan  clrf  Ccount      ;column count
  
scan1 movlw B'00001111'
    andwf Ccount,F
    movf  Ccount,W
    movwf PORTA     ;set columns
    call  dely      ;let column settle
    movf  PORTC,W     ;get row data
    movwf Row       ;row data
    lfsr  FSR0,Buffer
    movf  Ccount,W
    addwf FSR0L
    movf  INDF0,W
    movwf Oldrow
  
  
    btfsc PORTA,5     ;what scan mode?
    bra   bscan     ;pushbutton mode
    xorwf Row,W     ;compare with new
    bnz   change      ;a change
scan2 incf  Ccount
    btfss Ccount,4    ;more than 15?
    bra   scan1     ;next column
    return          ;finish scan    
     
change  movwf Bitcng      ;hold the bit change
    clrf  Bitcnt      ;which bit?
    clrf  Pointno     ;Point number
change1 rrcf  Bitcng,F    ;rotate to find which bit changed
    bc    gotbit
    incf  Bitcnt,F
    bra   change1
    
gotbit  movf  Ccount,W    ;set up to calculate point number
    movwf Pointno
    rlncf Pointno,F
    rlncf Pointno,W   ;multiply by 4
    addwf Bitcnt,W    ;add the row number
    movwf Pointno     ;got the point number
  
    movlw 3       ;do offset for upper rows
    cpfsgt  Bitcnt
    bra   restore     ;send CAN packet
    movlw .60       ;offset
    addwf Pointno,F
restore clrf  Rowmsk      ;work out new value for EEPROM
    bsf   Rowmsk,0    ;set rolling bit
    incf  Bitcnt
resto1  dcfsnz  Bitcnt
    bra   endroll
    rlncf Rowmsk,F    ;this rolls the bit to the changed bit position
    bra   resto1
endroll movf  Rowmsk,W
    andwf Row,W     ;what was the bit value
    bsf   Turn,0      ;not zero if new bit was a 1
    bz    bit0
    movf  Rowmsk,W
    iorwf Oldrow,W
bitback movwf INDF0     ;put back in buffer
    bra   sendpkt
bit0  bcf   Turn,0      ;reset
    comf  Rowmsk,W
    andwf Oldrow,W
    bra   bitback
    
sendpkt incf  Pointno,F   ;add 1 as numbers start at 1 not 0
    movf  Pointno,W
    movwf Tx1d4     ;create event by point no.
    clrf  Tx1d3     ;max of 128 events from this node
    btfsc Turn,0      ;which direction?
    bra   turnoff
    movlw 0x90      ;set command
    bra   sndpkt1
turnoff movlw 0x91      ;unset command
sndpkt1 movwf Tx1d0     ;put in CAN frame
    movlw B'00001111'   ;clear last priority
    andwf Tx1sidh,F
    movlw B'10110000'   ;starting priority
    iorwf Tx1sidh,F
    
    
    
    movlw 5
    movwf Tx1dlc      ;5 byte command
    movlw .10
    movwf Latcount
    call  sendTX1     ;send CAN frame
  
    
    bra   scan1         ;for now      
    
    
    
bscan comf  Row,F     ;button scan routine here. 
  
    
bscan2  movf  Row,W
    subwf Oldrow,W    ;has any bit changed?  (button still pressed?)
    bz    scan2     ;no change
    movf  Row,F
    bnz   bchange     ;is it zero
    movlw 0
    movwf INDF0     ;old row back to zero
    
    bra   scan2     ;
bchange movf  Row,W
    andwf Oldrow,F    ;clear any released buttons
    rrcf  Row,F     ;roll row to see which bit it was
    bc    n1        ;first point normal
    rrcf  Row,F
    bc    r1        ;first point reset
    rrcf  Row,F
    bc    n2
    rrcf  Row,F
    bc    r2
    rrcf  Row,F
    bc    n3
    rrcf  Row,F
    bc    r3
    rrcf  Row,F
    bc    n4
    rrcf  Row,F
    bc    r4
n1    btfsc Oldrow,0    ;is it still ON
    bra   scan2     ;do nothing
    bsf   Oldrow,0    ;set on
    movf  Oldrow,W
  
    movwf INDF0
    clrf  Turn      ;set direction
    rlncf Ccount,W    ;double column count
    movwf Pointno
    bra   sendpkt
r1    btfsc Oldrow,1    ;get old EEPROM row data
    bra   scan2
    bsf   Oldrow,1
    movf  Oldrow,W
    
    movwf INDF0
    bsf   Turn,0      ;set direction
    rlncf Ccount,W    ;double column count
    movwf Pointno
    bra   sendpkt
n2    btfsc Oldrow,2    ;get old EEPROM row data
    bra   scan2
    bsf   Oldrow,2
    movf  Oldrow,W
    
    movwf INDF0
    clrf  Turn      ;set direction
    rlncf Ccount,W    ;double column count
    movwf Pointno
    incf  Pointno
    bra   sendpkt
r2    btfsc Oldrow,3    ;get old EEPROM row data
    bra   scan2
    bsf   Oldrow,3
    movf  Oldrow,W
  
    movwf INDF0
    bsf   Turn,0      ;set direction
    rlncf Ccount,W    ;double column count
    movwf Pointno
    incf  Pointno
    bra   sendpkt
n3    btfsc Oldrow,4    ;get old EEPROM row data
    bra   scan2
    bsf   Oldrow,4
    movf  Oldrow,W
    
    movwf INDF0
    clrf  Turn      ;set direction
    rlncf Ccount,W    ;double column count
    addlw .32       ;point no. offset
    movwf Pointno
    bra   sendpkt
r3    btfsc Oldrow,5    ;get old EEPROM row data
    bra   scan2
    bsf   Oldrow,5
    movf  Oldrow,W
    
    movwf INDF0
    bsf   Turn,0      ;set direction
    rlncf Ccount,W    ;double column count
    addlw .32       ;point no. offset
    movwf Pointno
    bra   sendpkt
n4    btfsc Oldrow,6    ;get old EEPROM row data
    bra   scan2
    bsf   Oldrow,6
    movf  Oldrow,W
  
    movwf INDF0
    clrf  Turn      ;set direction
    rlncf Ccount,W    ;double column count
    addlw .32       ;point no. offset
    movwf Pointno
    incf  Pointno
    bra   sendpkt
r4    btfsc Oldrow,7    ;get old EEPROM row data
    bra   scan2
    bsf   Oldrow,7
    movf  Oldrow,W
  
    movwf INDF0
    bsf   Turn,0      ;set direction
    rlncf Ccount,W    ;double column count  
    addlw .32       ;point no. offset
    movwf Pointno
    incf  Pointno
    bra   sendpkt
  

    
;************************************************************************

newid movf  CANid,W     ;put in stored ID for RTR
    
    movwf CanID_tmp 
    call  shuffle
    movff IDtemph,Tx1sidh
    movff IDtempl,Tx1sidl
  
    movlb .15       ;put ID into TXB2 for enumeration response to RTR
    clrf  TXB2CON
    clrf  TXB2SIDH
    movf  IDtemph,W
    movwf TXB2SIDH
    movf  IDtempl,W
    movwf TXB2SIDL
    movlw 0xB0
    iorwf TXB2SIDH    ;set priority
    clrf  TXB2DLC     ;no data, no RTR
    movlb 0
    return

;***************************************************************************          
  


    
    end
