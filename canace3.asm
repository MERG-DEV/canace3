    TITLE   "Source for CAN accessory encoder using CBUS"
; filename ACE3_t.asm   start 10/05/09

; A control panel encoder for the FLiM model 
; Scans 128 toggles or 64 dual PBs, selected by a jumper

; Works with toggle switches
; Works with PBs
; Sends ON and OFF events
; Event numbers start at 1 and are sequential with columms and first 4 rows
; then columns and second 4 rows. Makes panel wiring simpler.
; 
; Present code works with the SLiM PCB.
; Based on the CANACE3c code
; Uses NV1 to set mode. 0 is toggle switches, 1 is pushbutton.
; This node is a producer only so has no event learning.

; The setup timer is TMR3. Used during self enumeration.
; CAN bit rate of 125 Kbits/sec
; Standard frame only

;Flash timer is TMR0.  

;node number release frame <0x51><NN hi><NN lo>
;keep alive frame  <0x52><NN hi><NN lo>
;read node parameters <0x10> Only works in setup mode. Sends string of 7 bytes as 
;<0xEF><para1><para2><para3><para4><para5><para6><para7>

;the only NV is to set the operation mode.

;set NV is  <0x96><NN hi><NN lo><NV#><NV val>
;read NV is <0x71><NN hi><NN lo><NV#>
;answer is
;<0x97><NN hi><NN lo><NV#><NV val>

;mods to hpint error routine 02/08/08
;mods to scan so uses sendTX and not sendTX1
;Tx error interrupt disabled  5/8/08
;mods to error handling in interupt
;Tx error enabled 06/09/08

; Version d 10/05/09
; Add ACE3 Id to params
; Fix buffer flow control bug in SendTx1 routine

; Changed to ACE3_a  30/12/09
; This is the combined FLiM / SLiM version with bootloader.
; ACE3_b  16/01/10  Mods for different switch cominations in FLiM mode
; Mode 0    All toggles
; Mode 1    All PB pairs
; Mode 2    Top 4 rows  Toggles,  bottom 4 rows PB pairs
; Mode 3    Top 4 rows  Toggles,  bottom 4 rows PB ON only
; Mode 4    Top 4 rows  PB pairs, bottom 4 rows PB ON only
; Mode 5    All PB ON only

; Rev c   24/01/10  Fix to event number offset in modes 2 and 3
; Rev d   27/01/10  Added back NNACK after enum.
;   Rev e   15/02/10 ignore zero length frames in hpint routine
;       test TXB2CON.TXREQ is zero before transmitting
; Rev f   TXB2CON test in newid_f
; Rev g   New enum sequence  24/03/10

; rev h   Changed HPINT error handling for RX buf overflow

; rev j   No rev i.
;       A major rewrite of rev h to allow event learning, including short events
;       for 'many to many' mode. 02/08/10
;       Working in SLiM mode OK. 11/08/10
; rev k   Added FLiM mode to rev j.  12/01/11  (not finished yet)
; rev m   As rev k with mods to F5 handling and Pointno in snd_inx
; rev n   Default switch numbers put back to starting at 1. 02/03/11 (temporary fix. never released)
; rev p   As rev m but events are sent as base 1 although stored with index 0
;     Incoming events also base 1 but stored as base 0.
;     Applies to both long and short events
;     Added WRACK frame following any FLASH wrirtes.  (OPC 0x59)
; rev q As rev p but distinguishes table Index from Pointno. 
;     Correction to para1rd for node number
;     Correction to restore on double teach in SLiM
;     Doesn't restore default in FLiM if same event sent twice
;     Added restore all events to default using OPC NNCLR (0x55) in learn mode
;   rev r 15/03/11 SLiM mode: Boot cmd uses SLiM NN,
;            OPC 0x73 RQNPN works with SLiM NN
;          Add FLiM mode test to packet routine
;          Remove FLiM mode tests from individual routines
; rev s Correction so responds to RTR in SLiM mode
; rev t Correction to putNN for EEPROM write 10/05/11


; End of comments for ACE3

; This is the bootloader section

;*  Filename Boot2.asm  30/10/09

;*************************************************************** * * * * * * * * * * * * * * ;*
;*  CBUS bootloader

;*  Based on the Microchip botloader 'canio.asm' tho which full acknowledgement is made.
;*  Relevant information is contained in the Microchip Application note AN247

;*
;* Basic Operation:
;* The following is a CAN bootloader designed for PIC18F microcontrollers
;* with built-in CAN such as the PIC18F458. The bootloader is designed to
;* be simple, small, flexible, and portable.
;*
;
;
;*
;* Commands:
;* Put commands received from source (Master --> Slave)
;* The count (DLC) can vary.
;* XXXXXXXXXXX 0 0 8 XXXXXXXX XXXXXX00 ADDRL ADDRH ADDRU RESVD CTLBT SPCMD CPDTL CPDTH
;* XXXXXXXXXXX 0 0 8 XXXXXXXX XXXXXX01 DATA0 DATA1 DATA2 DATA3 DATA4 DATA5 DATA6 DATA7
;*


;*
;* ADDRL - Bits 0 to 7 of the memory pointer.
;* ADDRH - Bits 8 - 15 of the memory pointer.
;* ADDRU - Bits 16 - 23 of the memory pointer.
;* RESVD - Reserved for future use.
;* CTLBT - Control bits.
;* SPCMD - Special command.
;* CPDTL - Bits 0 - 7 of 2s complement checksum
;* CPDTH - Bits 8 - 15 of 2s complement checksum
;* DATAX - General data.
;*
;* Control bits:
;* MODE_WRT_UNLCK-Set this to allow write and erase operations to memory.
;* MODE_ERASE_ONLY-Set this to only erase Program Memory on a put command. Must be on 64-byte
;*  boundary.
;* MODE_AUTO_ERASE-Set this to automatically erase Program Memory while writing data.
;* MODE_AUTO_INC-Set this to automatically increment the pointer after writing.
;* MODE_ACK-Set this to generate an acknowledge after a 'put' (PG Mode only)
;*
;* Special Commands:
;* CMD_NOP      0x00  Do nothing
;* CMD_RESET    0x01  Issue a soft reset after setting last EEPROM data to 0x00
;* CMD_RST_CHKSM  0x02  Reset the checksum counter and verify
;* CMD_CHK_RUN    0x03  Add checksum to special data, if verify and zero checksum
;* CMD_BOOT_TEST  0x04  Just sends a message frame back to verify boot mode.

;*  Modified version of the Microchip code by M Bolton  30/10/09
;
; The user program must have the folowing vectors

; User code reset vector  0x0800
; User code HPINT vector  0x0808
; user code LPINT vector  0x0818

; Checksum is 16 bit addition of all programmable bytes.
; User sends 2s complement of addition at end of program in command 0x03 (16 bits only)

;**********************************************************************************

; Assembly options
  LIST  P=18F2480,r=hex,N=75,C=120,T=ON

  include   "p18f2480.inc"
  
  ;definitions  Change these to suit hardware.
  
S_PORT  equ PORTB ;setup switch  Change as needed
S_BIT0  equ  4    ; bits used for node number and CAN ID
S_BIT1  equ  5
M_PORT  equ  PORTA
M_BIT equ  5    ;mode toggle or PB
S_BIT equ 4
Modstat equ 1   ;address in EEPROM
Man_no  equ .165  ;manufacturer number
Ver_no  equ "T" ;for now
ACE3_ID equ 4   ; id of ACE3
Para1 equ Man_no
Para2 equ Ver_no
Para3 equ ACE3_ID   
Para4 equ 0   ;node descriptors (temp values)
Para5 equ 0
Para6 equ 1
Para7 equ 0
NV_NUM  equ 1   ;only one NV

; definitions used by bootloader

#define MODE_SELF_VERIFY  ;Enable self verification of written data (undefine if not wanted)

#define HIGH_INT_VECT 0x0808  ;HP interrupt vector redirect. Change if target is different
#define LOW_INT_VECT  0x0818  ;LP interrupt vector redirect. Change if target is different.
#define RESET_VECT  0x0800  ;start of target
#define CAN_CD_BIT  RXB0EIDL,0  ;Received control / data select bit
#define CAN_PG_BIT  RXB0EIDL,1  ;Received PUT / GET bit
#define CANTX_CD_BIT  TXB0EIDL,0  ;Transmit control/data select bit
#define CAN_TXB0SIDH  B'10000000' ;Transmitted ID for target node
#define CAN_TXB0SIDL  B'00001000'
#define CAN_TXB0EIDH  B'00000000' ;
#define CAN_TXB0EIDL  B'00000100'
#define CAN_RXF0SIDH  B'00000000' ;Receive filter for target node
#define CAN_RXF0SIDL  B'00001000'
#define CAN_RXF0EIDH  B'00000000'
#define CAN_RXF0EIDL  B'00000111'
#define CAN_RXM0SIDH  B'11111111' ;Receive masks for target node
#define CAN_RXM0SIDL  B'11101011'
#define CAN_RXM0EIDH  B'11111111'
#define CAN_RXM0EIDL  B'11111000'
#define CAN_BRGCON1   B'00000011' ;CAN bit rate controls. As for other CBUS modules
#define CAN_BRGCON2   B'10011110'
#define CAN_BRGCON3   B'00000011'
#define CAN_CIOCON    B'00100000' ;CAN I/O control  

#ifndef EEADRH    
#define EEADRH  EEADR+ 1  
#endif      
#define TRUE  1 
#define FALSE 0 
#define WREG1 PRODH ; Alternate working register
#define WREG2 PRODL 
#define MODE_WRT_UNLCK  _bootCtlBits, 0 ; Unlock write and erase
#define MODE_ERASE_ONLY _bootCtlBits, 1 ; Erase without write
#define MODE_AUTO_ERASE _bootCtlBits, 2 ; Enable auto erase before write
#define MODE_AUTO_INC _bootCtlBits, 3 ; Enable auto inc the address
#define MODE_ACK    _bootCtlBits, 4 ; Acknowledge mode
#define ERR_VERIFY    _bootErrStat, 0 ; Failed to verify if set
#define CMD_NOP     0x00  
#define CMD_RESET   0x01  
#define CMD_RST_CHKSM 0x02  
#define CMD_CHK_RUN   0x03
#define CMD_BOOT_TEST   0x04  

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
  ; RAM addresses used by boot. can also be used by application.

  CBLOCK 0
  _bootCtlMem
  _bootAddrL    ; Address info
  _bootAddrH    
  _bootAddrU    
  _unused0    ;(Reserved)
  _bootCtlBits  ; Boot Mode Control bits
  _bootSpcCmd   ; Special boot commands
  _bootChkL   ; Chksum low byte fromPC
  _bootChkH   ; Chksum hi byte from PC    
  _bootCount    
  _bootChksmL   ; 16 bit checksum
  _bootChksmH   
  _bootErrStat  ;Error Status flags
  ENDC


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
  TempINTCON
  CANid   ;SLiM node CAN ID
  CanID_tmp ;temp for CAN Node ID
  IDtemph   ;used in ID shuffle
  IDtempl
  NN_temph  ;node number in RAM
  NN_templ
  NodeID_l  ;used in SLiM mode
  NV_temp   ;temp store of NV
  IDcount   ;used in self allocation of CAN ID.
  Datmode     ;flag for data waiting and other states
  Count     ;counter for loading
  Count1
  Count2
  Dcount      ;used in delay only
  Dcount1
  Latcount    ;latency counter
  Keepcnt     ;keep alive counter

  Temp      ;temps
  Temp1
  Atemp     ;Port temp value
  Dlc       ;data length
  Mode      ;for FLim / Slim
  Mode1     ;mode. toggle or PB in FLiM
  Mode2     ;mode. toggle or PB in SLiM

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
  
  Eadr    ;temp eeprom address
  

  Column    ;column counter for keyboard scan
  
    ;***************************************************************
  ; the following are used in the switch scanning
  Ccount    ;column counter for switch scan
  Oldrow    ;original row data
  Oldrow1
  Row     ;row data for switch scan
  Row1
  Bitcng    ;bit change in scan
  Bitcnt    ;bit counter in scan
  Pointno   ;calculated point number for scan
  Index   ;pointer to FLASH event table
  Turn    ;direction of changed switch
  Rowmsk    ;row mask
  PointnL   ;low byte of 16 bit point number
  PointnH   ;hi byte
  
  Roll    ;rolling bit for enum
  
  Fsr_tmp1Le  ;temp store for FSR1
  Fsr_tmp1He 
  Enum0   ;bits for new enum scheme.
  Enum1
  Enum2
  Enum3
  Enum4
  Enum5
  Enum6
  Enum7
  Enum8
  Enum9
  Enum10
  Enum11
  Enum12
  Enum13

  Flcount     ;flash memory counter
  Flcount1    ;flash memory counter low
  Flcounth    ;flash memory counter high
  Blk_st      ;first block for unlearn
  Blk_lst     ;last block for unlearn
  Blk_num     ;number of blocks to shift up
  ENcount     ;used in flash write
  ENtempl
  ENtemph
  
  
  ;add variables to suit
  
  

  ENDC  

  CBLOCK  0x100   ;buffer for FLASH write 
  Flash_buf

  ENDC  
  
  
    


;****************************************************************
; This is the bootloader
; ***************************************************************************** 
;_STARTUPCODE 0x00
  ORG 0x0000
; *****************************************************************************
  bra _CANInit
  bra _StartWrite
; ***************************************************************************** 
;_INTV_H CODE 0x08
  ORG 0x0008
; *****************************************************************************

  goto  HIGH_INT_VECT

; ***************************************************************************** 
;_INTV_L CODE 0x18
  ORG 0x0018
; *****************************************************************************

  goto  LOW_INT_VECT 

; ************************************************************** 
; Code start
; **************************************************************
  ORG 0x0020
;_CAN_IO_MODULE CODE
; ************************************************************ ** * * * * * * * * * * * * * * * 
; Function: VOID _StartWrite(WREG _eecon_data)
;PreCondition: Nothing
;Input: _eecon_data
;Output: Nothing. Self write timing started.
;Side Effects: EECON1 is corrupted; WREG is corrupted.
;Stack Requirements: 1 level.
;Overview: Unlock and start the write or erase sequence to protected
; memory. Function will wait until write is finished.
;
; ************************************************************ ** * * * * * * * * * * * * * * *
_StartWrite
  movwf   EECON1
  btfss   MODE_WRT_UNLCK  ; Stop if write locked
  return
  movlw   0x55  ; Unlock
  movwf    EECON2 
  movlw  0xAA 
  movwf    EECON2
  bsf  EECON1, WR ; Start the write
  nop
  btfsc   EECON1, WR  ; Wait (depends on mem type)
  bra $ - 2
  return
; ************************************************************ ** * * * * * * * * * * * * * * *

; Function: _bootChksm _UpdateChksum(WREG _bootChksmL)
;
; PreCondition: Nothing
; Input: _bootChksmL
; Output: _bootChksm. This is a static 16 bit value stored in the Access Bank.
; Side Effects: STATUS register is corrupted.
; Stack Requirements: 1 level.
; Overview: This function adds a byte to the current 16 bit checksum
; count. WREG should contain the byte before being called.
;
; The _bootChksm value is considered a part of the special
; register set for bootloading. Thus it is not visible. ;
;*************************************************************** * * * * * * * * * * * *
_UpdateChksum:
  addwf _bootChksmL,  F ; Keep a checksum
  btfsc STATUS, C
  incf  _bootChksmH,  F
  return
;************************************************************ ** * * * * * * * * * * * * * * *
;
; Function: VOID _CANInit(CAN,  BOOT)
;
; PreCondition: Enter only after a reset has occurred.
; Input: CAN control information, bootloader control information ; Output: None.
; Side Effects: N/A. Only run immediately after reset.
; Stack Requirements: N/A
; Overview: This routine is technically not a function since it will not
; return when called. It has been written in a linear form to
; save space.Thus 'call' and 'return' instructions are not
; included, but rather they are implied. ;
; This routine tests the boot flags to determine if boot mode is
; desired or normal operation is desired. If boot mode then the
; routine initializes the CAN module defined by user input. It
; also resets some registers associated to bootloading.
;
; ************************************************************ ** * * * * * * * * * * * * * * *
_CANInit:
  clrf  EECON1
  setf  EEADR ; Point to last location of EEDATA
  setf  EEADRH
  bsf EECON1, RD  ; Read the control code
  incfsz EEDATA, W

  goto  RESET_VECT


  clrf  _bootSpcCmd   ; Reset the special command register
  movlw   0x1C    ; Reset the boot control bits
  movwf   _bootCtlBits 
  movlb d'15'   ; Set Bank 15
  bcf   TRISB, CANTX  ; Set the TX pin to output 
  movlw   CAN_RXF0SIDH  ; Set filter 0
  movwf   RXF0SIDH
  movlw   CAN_RXF0SIDL 
  movwf   RXF0SIDL
  comf  WREG    ; Prevent filter 1 from causing a receive event





  movwf RXF1SIDL  ;   
  movlw CAN_RXF0EIDH  
  movwf RXF0EIDH  
  movlw CAN_RXF0EIDL  
  movwf RXF0EIDL  
  movlw CAN_RXM0SIDH  ; Set mask
  movwf RXM0SIDH  
  movlw CAN_RXM0SIDL  
  movwf RXM0SIDL  
  movlw CAN_RXM0EIDH  
  movwf RXM0EIDH  
  movlw CAN_RXM0EIDL  
  movwf RXM0EIDL  
  movlw CAN_BRGCON1 ; Set bit rate
  movwf BRGCON1 
  movlw CAN_BRGCON2 
  movwf BRGCON2 
  movlw CAN_BRGCON3 
  movwf BRGCON3 
  movlw CAN_CIOCON  ; Set IO
  movwf CIOCON  
  
  clrf  CANCON  ; Enter Normal mode
  movlw B'00001110'
  movwf ADCON1
  bcf TRISB,7
  bcf TRISB,6
  bsf PORTB,7   ;gren LED on
  bsf PORTB,6   ;yellow LED on


; ************************************************************ ** * * * * * * * * * * * * * * * 
; This routine is essentially a polling loop that waits for a
; receive event from RXB0 of the CAN module. When data is
; received, FSR0 is set to point to the TX or RX buffer depending
; upon whether the request was a 'put' or a 'get'.
; ************************************************************ ** * * * * * * * * * * * * * * * 
_CANMain
  
  bcf RXB0CON, RXFUL  ; Clear the receive flag
_wait clrwdt      ; Clear WDT while waiting
  btfss   RXB0CON, RXFUL  ; Wait for a message  
  bra _wait



_CANMainJp1
  lfsr  0, RXB0D0
  movf  RXB0DLC, W 
  andlw   0x0F
  movwf   _bootCount 
  movwf   WREG1
  bz  _CANMain 
_CANMainJp2       ;?
  


; ************************************************************** * * * * * * * * * * * * * * * 
; Function: VOID _ReadWriteMemory()
;
; PreCondition:Enter only after _CANMain().
; Input: None.
; Output: None.
; Side Effects: N/A.
; Stack Requirements: N/A
; Overview: This routine is technically not a function since it will not
; return when called. It has been written in a linear form to
; save space.Thus 'call' and 'return' instructions are not
; included, but rather they are implied.
;This is the memory I/O engine. A total of eight data bytes are received and decoded. In addition two control bits are received, put/get and control/data.
;A pointer to the buffer is passed via FSR0 for reading or writing. 
;The control register set contains a pointer, some control bits and special command registers.
;Control
;<PG><CD><ADDRL><ADDRH><ADDRU><_RES_><CTLBT>< SPCMD><CPDTL><CPDTH>
;Data
;<PG>< CD>< DATA0>< DATA1>< DATA2>< DATA3>< DATA4>< DATA5>< DATA6>< DATA7>
;PG bit:  Put = 0, Get = 1
;CD bit:  Control = 0, Data = 1

; ************************************************************ ** * * * * * * * * * * * * * * *
_ReadWriteMemory:
  btfsc CAN_CD_BIT  ; Write/read data or control registers
  bra _DataReg
; ************************************************************ ** * * * * * * * * * * * * * * * ; This routine reads or writes the bootloader control registers,
; then executes any immediate command received.
_ControlReg
  lfsr  1, _bootAddrL   ;_bootCtlMem
_ControlRegLp1

  movff   POSTINC0, POSTINC1 
  decfsz  WREG1, F
  bra _ControlRegLp1

; ********************************************************* 
; This is a no operation command.
  movf  _bootSpcCmd, W    ; NOP Command
  bz  _CANMain
; bz  _SpecialCmdJp2    ; or send an acknowledge

; ********************************************************* 
; This is the reset command.
  xorlw   CMD_RESET   ; RESET Command 
  btfss   STATUS, Z
  bra   _SpecialCmdJp4
  setf  EEADR   ; Point to last location of EEDATA
  setf  EEADRH
  clrf  EEDATA    ; and clear the data (FF for now)
  movlw   b'00000100' ; Setup for EEData
  rcall   _StartWrite
  bcf   PORTB,6   ;yellow LED off
  reset
; *********************************************************
; This is the Selfcheck reset command. This routine 
; resets the internal check registers, i.e. checksum and 
; self verify.
_SpecialCmdJp4
  movf  _bootSpcCmd, W 
  xorlw   CMD_RST_CHKSM
  bnz   _SpecialCmdJp1
  clrf  _bootChksmH
  clrf  _bootChksmL
  bcf   ERR_VERIFY    
  clrf  _bootErrStat
  bra   _CANMain
; RESET_CHKSM Command
; Reset chksum
; Clear the error verify flag

;This is the Test and Run command. The checksum is
; verified, and the self-write verification bit is checked. 
; If both pass, then the boot flag is cleared.
_SpecialCmdJp1
  movf  _bootSpcCmd, W    ; RUN_CHKSM Command
  xorlw   CMD_CHK_RUN 
  bnz _SpecialCmdJp3
  movf  _bootChkL, W  ; Add the control byte
  addwf  _bootChksmL, F
  bnz _SpecialCmdJp2
  movf  _bootChkH, W 
  addwfc  _bootChksmH, F
  bnz _SpecialCmdJp2
  btfsc   ERR_VERIFY    ; Look for verify errors
  bra _SpecialCmdJp2

  bra   _CANSendOK  ;send OK message


_SpecialCmdJp2

  bra _CANSendNOK ; or send an error acknowledge


_SpecialCmdJp3
  movf  _bootSpcCmd, W    ; RUN_CHKSM Command
  xorlw   CMD_BOOT_TEST 
  bnz _CANMain
  bra _CANSendBoot

; ************************************************************** * * * * * * * * * * * * * * * 
; This is a jump routine to branch to the appropriate memory access function.
; The high byte of the 24-bit pointer is used to determine which memory to access. 
; All program memories (including Config and User IDs) are directly mapped. 
; EEDATA is remapped.
_DataReg
; *********************************************************
_SetPointers
  movf  _bootAddrU, W ; Copy upper pointer
  movwf   TBLPTRU
  andlw   0xF0  ; Filter
  movwf   WREG2
  movf  _bootAddrH, W ; Copy the high pointer
  movwf   TBLPTRH
  movwf   EEADRH
  movf  _bootAddrL, W ; Copy the low pointer
  movwf   TBLPTRL
  movwf  EEADR
  btfss   MODE_AUTO_INC ; Adjust the pointer if auto inc is enabled
  bra _SetPointersJp1
  movf  _bootCount, W ; add the count to the pointer
  addwf  _bootAddrL, F 
  clrf  WREG
  addwfc   _bootAddrH, F 
  addwfc   _bootAddrU, F 

_SetPointersJp1     ;?

_Decode
  movlw   0x30
  cpfslt  WREG2
  bra _DecodeJp1



  bra _PMEraseWrite

_DecodeJp1
  movf  WREG2,W
  xorlw   0x30
  bnz _DecodeJp2



  bra _CFGWrite 
_DecodeJp2
  movf  WREG2,W 
  xorlw 0xF0
  bnz _CANMain
  bra _EEWrite

f 

; Program memory < 0x300000
; Config memory = 0x300000
; EEPROM data = 0xF00000
  
; ************************************************************ ** * 
; ************************************************************** * 
; Function: VOID _PMRead()
; VOID _PMEraseWrite ()
;
; PreCondition:WREG1 and FSR0 must be loaded with the count and address of
; the source data.
; Input: None.
; Output: None.
; Side Effects: N/A.
; Stack Requirements: N/A
; Overview: These routines are technically not functions since they will not
; return when called. They have been written in a linear form to
; save space.Thus 'call' and 'return' instructions are not
; included, but rather they are implied.
;These are the program memory read/write functions. Erase is available through control flags. An automatic erase option is also available.
; A write lock indicator is in place to ensure intentional write operations.
;Note: write operations must be on 8-byte boundaries and must be 8 bytes long. Also erase operations can only occur on 64-byte boundaries.
; ************************************************************ ** * * * * * * * * * * * * * * *



_PMEraseWrite:
  btfss   MODE_AUTO_ERASE
  bra _PMWrite
_PMErase:
  movf  TBLPTRL, W
  andlw b'00111111'
  bnz _PMWrite
_PMEraseJp1
  movlw b'10010100' 
  rcall   _StartWrite 
_PMWrite:
  btfsc   MODE_ERASE_ONLY


  bra _CANMain 

  movf  TBLPTRL, W
  andlw b'00000111'
  bnz _CANMain 
  movlw   0x08
  movwf WREG1

_PMWriteLp1         ; Load the holding registers
  movf  POSTINC0, W 
  movwf   TABLAT
  rcall  _UpdateChksum  ; Adjust the checksum
  tblwt*+
  decfsz   WREG1, F
  bra _PMWriteLp1

#ifdef MODE_SELF_VERIFY 
  movlw  0x08
  movwf   WREG1 
_PMWriteLp2
  tblrd*-     ; Point back into the block
  movf  POSTDEC0, W 
  decfsz   WREG1, F
  bra _PMWriteLp2
  movlw  b'10000100'  ; Setup writes
  rcall _StartWrite   ; Write the data
  movlw   0x08
  movwf   WREG1
_PMReadBackLp1
  tblrd*+     ; Test the data
  movf  TABLAT, W 
  xorwf   POSTINC0, W
  btfss STATUS, Z
  bsf ERR_VERIFY 
  decfsz  WREG1, F
  bra _PMReadBackLp1  ; Not finished then repeat
#else
  tblrd*-     ; Point back into the block
         ; Setup writes
  movlw   b'10000100'   ; Write the data
  rcall   _StartWrite   ; Return the pointer position
  tblrd*+
#endif

  bra _CANMain


; ************************************************************** * * * * * * * * * * * * * * *
 ; Function: VOID _CFGWrite()
; VOID _CFGRead()
;
; PreCondition:WREG1 and FSR0 must be loaded with the count and address of the source data. 
; Input: None.
; Output: None.
; Side Effects: N/A.
; Stack Requirements: N/A
; Overview: These routines are technically not functions since they will not
; return when called. They have been written in a linear form to
; save space. Thus 'call' and 'return' instructions are not
; included, but rather they are implied.
;
; These are the Config memory read/write functions. Read is
; actually the same for standard program memory, so any read
; request is passed directly to _PMRead.
;
; ************************************************************ ** * * * * * * * * * * * * * * *
_CFGWrite

#ifdef MODE_SELF_VERIFY   ; Write to config area
  movf  INDF0, W    ; Load data
#else
  movf  POSTINC0, W
#endif
  movwf   TABLAT
  rcall   _UpdateChksum ; Adjust the checksum
  tblwt*      ; Write the data
  movlw b'11000100' 
  rcall   _StartWrite
  tblrd*+     ; Move the pointers and verify
#ifdef MODE_SELF_VERIFY 
  movf  TABLAT, W 
  xorwf   POSTINC0, W

#endif
  decfsz  WREG1, F
  bra _CFGWrite ; Not finished then repeat

  bra _CANMain 



; ************************************************************** * * * * * * * * * * * * * * * 
; Function: VOID _EERead()
; VOID _EEWrite()
;
; PreCondition:WREG1 and FSR0 must be loaded with the count and address of
 ;  the source data.
; Input:  None.
; Output: None.
; Side Effects: N/A.
; Stack Requirements: N/A
; Overview: These routines are technically not functions since they will not
; return when called. They have been written in a linear form to
; save space. Thus 'call' and 'return' instructions are not
; included, but rather they are implied.
;
; This is the EEDATA memory read/write functions.
;
; ************************************************************ ** * * * * * * * * * * * * * * *


_EEWrite:

#ifdef MODE_SELF_VERIFY
  movf  INDF0, W
#else
  movf  POSTINC0, W 
#endif

  movwf   EEDATA
  rcall   _UpdateChksum 
  movlw b'00000100' 
  rcall  _StartWrite

#ifdef MODE_SELF_VERIFY 
  clrf  EECON1
  bsf EECON1, RD
  movf  EEDATA, W 
  xorwf   POSTINC0, W
  btfss STATUS, Z
  bsf ERR_VERIFY
#endif

  infsnz   EEADR, F 
  incf  EEADRH, F 
  decfsz  WREG1, F
  bra _EEWrite


  bra _CANMain 
  

; Read the data

; Adjust EEDATA pointer
; Not finished then repeat
; Load data
; Adjust the checksum 
; Setup for EEData
; and write
; Read back the data ; verify the data ; and adjust pointer
; Adjust EEDATA pointer
; Not finished then repeat

; ************************************************************** * * * * * * * * * * * * * * *
; Function: VOID _CANSendAck()
; VOID _CANSendResponce ()
;
; PreCondition:TXB0 must be preloaded with the data.
; Input: None.
; Output: None.
; Side Effects: N/A.
; Stack Requirements: N/A
; Overview: These routines are technically not functions since they will not
; return when called. They have been written in a linear form to
; save space. Thus 'call' and 'return' instructions are not
; included, but rather they are implied. ;
; These routines are used for 'talking back' to the source. The
; _CANSendAck routine sends an empty message to indicate
; acknowledgement of a memory write operation. The
; _CANSendResponce is used to send data back to the source. ;
; ************************************************************ ** * * * * * * * * * * * * * * *



_CANSendMessage
  btfsc   TXB0CON,TXREQ 
  bra $ - 2
  movlw   CAN_TXB0SIDH 
  movwf   TXB0SIDH
  movlw   CAN_TXB0SIDL 
  movwf   TXB0SIDL
  movlw   CAN_TXB0EIDH 
  movwf   TXB0EIDH  

  movlw CAN_TXB0EIDL
  movwf TXB0EIDL
  bsf CANTX_CD_BIT
  btfss CAN_CD_BIT 
  bcf CANTX_CD_BIT
  bsf TXB0CON, TXREQ
      bra  _CANMain ; Setup the command bit

_CANSendOK        ;send OK message 
  movlw 1     ;a 1 is OK
  movwf TXB0D0
  movwf TXB0DLC
  bra   _CANSendMessage
  
_CANSendNOK       ;send not OK message
  clrf  TXB0D0    ;a 0 is not OK
  movlw 1
  movwf TXB0DLC
  bra   _CANSendMessage

_CANSendBoot
  movlw 2     ;2 is confirm boot mode
  movwf TXB0D0
  movlw 1
  movwf TXB0DLC
  bra   _CANSendMessage
    
; Start the transmission

;   End of bootloader

;************************************************************************************

;
;   start of program code

    ORG   0800h
    nop           ;for debug
    goto  setup

    ORG   0808h
    goto  hpint     ;high priority interrupt

    ORG   0810h     ;node type parameters
node_ID db    Para1,Para2,Para3,Para4,Para5,Para6,Para7
                ;change these 7 bytes as required

    ORG   0818h 
    goto  lpint     ;low priority interrupt


;*******************************************************************

    ORG   0820h     ;start of program
; 
;
;   high priority interrupt. Used for CAN transmit error and enum response.


hpint movff CANCON,TempCANCON
    movff CANSTAT,TempCANSTAT
  
;   movff PCLATH,PCH_tempH    ;save PCLATH
  
  
    movff FSR0L,Fsr_temp0L    ;save FSR0
    movff FSR0H,Fsr_temp0H
    movff FSR1L,Fsr_temp1L    ;save FSR1
    movff FSR1H,Fsr_temp1H
    
    movlw 8
    movwf PCLATH

  
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
    btfsc Datmode,1     ;setup mode?
    bra   setmode 
    lfsr  FSR0,Rx0con
    
    goto  access
    
    ;error routine here. Only acts on lost arbitration in TX1
    ;used for increasing priority if send is delayed
  
errint  movlb .15         ;change bank      
    btfss TXB1CON,TXLARB
    bra   errbak        ;not lost arb in TX1
  
    movf  Latcount,F      ;is it already at zero?
    bz    errbak
    decfsz  Latcount,F
    bra   errbak
    bcf   TXB1CON,TXREQ   ;abort current transmit
    movlw B'00111111'
    andwf TXB1SIDH,F      ;change priority
txagain     bsf TXB1CON,TXREQ   ;try again
          
errbak    bcf   RXB1CON,RXFUL
    movlb 0
    bcf   RXB0CON,RXFUL   ;ready for next
    
    bcf   COMSTAT,RXB0OVFL  ;clear overflow flags if set
    bcf   COMSTAT,RXB1OVFL    
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
    bra   isRTR
;   btfsc Datmode,1     ;setup mode?
;   bra   setmode 
    movf  Rx0dlc,F
    bz    back        ;ignore zero length frames  
    bsf   Datmode,0     ;valid message frame
    
back  bcf   RXB0CON,RXFUL ;ready for next
  
back1 clrf  PIR3      ;clear all interrupt flags
    movf  CANCON,W
    andlw B'11110001'
    iorwf TempCANCON,W
    
    movwf CANCON
;   movff PCH_tempH,PCLATH
    movff Fsr_temp0L,FSR0L    ;recover FSR0
    movff Fsr_temp0H,FSR0H

    movff Fsr_temp1L,FSR1L    ;recover FSR1
    movff Fsr_temp1H,FSR1H

    
    retfie  1       ;use shadow registers

isRTR btfsc Datmode,1   ;setup mode?
    bra   back      ;back
;   btfss Mode,1      ;FLiM?  corrected in rev s
;   bra   back
    movlb .15
isRTR1  btfsc TXB2CON,TXREQ ;wait till sent
    bra   isRTR1    
    bsf   TXB2CON,TXREQ ;send ID frame - preloaded in TXB2

    movlb 0
    bra   back

setmode tstfsz  RXB0DLC
    bra   back        ;only zero length frames for setup
    
    swapf RXB0SIDH,W      ;get ID into one byte
    rrcf  WREG
    andlw B'01111000'     ;mask
    movwf Temp
    swapf RXB0SIDL,W
    rrncf WREG
    andlw B'00000111'
    iorwf Temp,W
    movwf IDcount       ;has current incoming CAN_ID

    lfsr  FSR1,Enum0      ;set enum to table
enum_st clrf  Roll        ;start of enum sequence
    bsf   Roll,0
    movlw 8
enum_1  cpfsgt  IDcount
    bra   enum_2
    subwf IDcount,F     ;subtract 8
    incf  FSR1L       ;next table byte
    bra   enum_1
enum_2  dcfsnz  IDcount,F
    bra   enum_3
    rlncf Roll,F
    bra   enum_2
enum_3  movf  Roll,W
    iorwf INDF1,F
    bra   back

    



    


;**************************************************************
;
;
;   low priority interrupt. (if used)
; 

lpint retfie      ;not used
                

;*********************************************************************


;   org   0xA00     ;set to page boundary   
;*************************************************************
main  btfsc Mode,1      ;is it SLiM?
    bra   mainf
    btfss Mode,2      ;SLiM learn?
    bra   main_a      ;no
    btfss INTCON,TMR0IF   ;is it flash?
    bra   main_a
    btg   PORTB,7     ;flash green
    bcf   INTCON,TMR0IF

main_a  movlw B'00110000'   ;get NN from switches
    andwf S_PORT,W
    movwf Temp
    swapf Temp,W
    addlw 1
    cpfseq  Atemp
    bra   setnew
  
    bra   no_new

setnew  movwf Atemp

    movwf NN_templ
    call  newid1
    call  fill_buf      ;reinstate new NN in FLASH buffer
    call  wrack     ;may not need in SLiM?
no_new  btfsc M_PORT,M_BIT
    bra   mset
    btfss Mode2,0
    bra   noflash       ;do nothing
    bcf   Mode2,0       ;mode change
    call  buf_init      ;re-initialise
    bra   noflash
mset  btfsc Mode2,0
    bra   noflash       ;do nothing
    bsf   Mode2,0       ;mode change
    call  buf_init      ;re-initialise
    bra   noflash

mainf btfss INTCON,TMR0IF   ;is it flash?
    bra   noflash
    btfss Datmode,2
    bra   nofl1
    
    btg   PORTB,6     ;flash LED
    
nofl1 bcf   INTCON,TMR0IF
    btfss Datmode,3   ;running mode
    bra   noflash
    decfsz  Keepcnt     ;send keep alive?
    bra   noflash
    movlw .10
    movwf Keepcnt
    movlw 0x52
;   call  nnrel     ;send keep alive frame (works OK, turn off for now)

noflash btfsc M_PORT,S_BIT  ;setup button?
    bra   main3
    movlw .100
    movwf Count
    clrf  Count1
    clrf  Count2
wait  decfsz  Count2
    goto  wait
    btfss Datmode,2
    bra   wait2
    btfss INTCON,TMR0IF   ;is it flash?
    bra   wait2
    btg   PORTB,6     ;flash LED
    bcf   INTCON,TMR0IF
wait2 decfsz  Count1
    goto  wait
    btfsc M_PORT,S_BIT
    bra   main4     ;not held long enough
    decfsz  Count
    goto  wait
    btfss Mode,1      ;is it in FLiM?
    bra   go_FLiM
    clrf  Datmode     ;back to virgin
    
    bcf   PORTB,6     ;LED off
    bsf   PORTB,7     ;Green LED on
    clrf  INTCON      ;interrupts off
    movlw 1
    movwf IDcount     ;back to start
    movlw Modstat
    movwf EEADR
    movlw   0
    call  eewrite     ;status to reset
    movlw 0x51      ;send node release frame
    call  nnrel
    clrf  NN_temph
    clrf  NN_templ
wait1 btfss M_PORT,S_BIT
    bra   wait1     ;wait till release
    call  ldely
    btfss M_PORT,S_BIT
    bra   wait1
    
    movlw LOW NodeID      ;put NN back to 0000
    movwf EEADR
    movlw 0
    call  eewrite
    incf  EEADR
    movlw 0
    call  eewrite
    btfss Mode,1
    bra   main5       ;FLiM setup
    movlw Modstat
    movwf EEADR
    movlw 0
    call  eewrite       ;mode back to SLiM
    clrf  Datmode
    bcf   Mode,1
    bcf   PORTB,6
    bsf   PORTB,7       ;green LED on
    clrf  Atemp       ;for new NN and ID
    clrf  Mode1
    clrf  Mode2
    btfsc M_PORT,M_BIT    ;check for SLiM scan mode
    bsf   Mode2,0
    call  buf_init    
    movlw B'11000000'
    movwf INTCON
    goto  main        ;setloop

main5 movlw Modstat
    movwf EEADR
    movlw 0
    call  eewrite       ;mode back to SLiM
    
    bsf   Mode,1        ;to FLiM
    movlw B'11000000'
    movwf INTCON
    goto  setloop
main4 btfsc Mode,1        ;is it SLiM?
    bra   main4a
    btfss Mode,2
    bra   main4b
    bcf   Mode,2        ;out of learn
    bcf   Mode,3
    bsf   PORTB,7       ;green back on
    bra   main1
main4b  bsf   Mode,2        ;into learn
    bra   main1

main4a  btfss Datmode,3   
    bra   main3
    btfss Datmode,2
    bra   set2
    bcf   Datmode,2
    bsf   PORTB,6     ;LED on
    bra   main3
set2  bsf   Datmode,2
    btfsc Mode,1      ;?FLiM
    call  nnack

main3 btfss Datmode,1   ;setup mode ?
    bra   main1
    btfss PIR2,TMR3IF   ;setup timer out?
    bra   main3     ;fast loop till timer out (main3?)
    bcf   T3CON,TMR3ON  ;timer off
    bcf   PIR2,TMR3IF   ;clear flag
    call  new_enum    ;enum routine
    movlw LOW CANid1      ;put new ID in EEPROM
    movwf EEADR
    movf  IDcount,W
    call  eewrite
    call  newid_f     ;put new ID in various buffers
    movlw Modstat
    movwf EEADR
    movlw 1
    call  eewrite     ;set to normal status
    bcf   Datmode,1   ;out of setup
    bsf   Datmode,2   ;wait for NN
    call  nnack     ;send blank NN for config
;   bsf   PORTB,7     ;on light
    bra   main      ;continue normally
    
    
go_FLiM bsf   Datmode,1   ;FLiM setup mode
    bcf   PORTB,7     ;green off
    bra   wait1
    
  
  
main1 btfsc Datmode,0   ;any new CAN frame received?
    bra   packet      ;yes

    bra   do        ;look for inputs

;set of jumps here as branches too long

params1 goto  params
fl_lrn1 goto  fl_lrn
fl_ulrn1  goto  fl_ulrn

teach1  btfss Mode,1      ; only valid in SLiM mode
    goto  teach     ; SLiM mode teach
    bra   main2
    
readEV1 goto  readEV
undo1 goto  undo
    

                ;main packet handling is here
    
packet
    movlw 0x90      ;is it an event to teach?
    subwf Rx0d0,W
    bz    teach1
    movlw 0x91      ;is it an event to teach?
    subwf Rx0d0,W
    bz    teach1
    movlw 0x98      ;is it a short event to teach?
    subwf Rx0d0,W
    bz    teach1
    movlw 0x99      ;is it a short event to teach?
    subwf Rx0d0,W
    bz    teach1
    movlw 0x5C      ;reboot
    subwf Rx0d0,W
    bz    reboot
    movlw 0x73
    subwf Rx0d0,W
    bz    para1a
    btfss Mode,1      ; FLiM mode?
    bra   main2
    movlw 0x42      ;set NN on 0x42
    subwf Rx0d0,W
    bz    setNN
    movlw 0x10      ;read manufacturer
    subwf Rx0d0,W
    bz    params1     ;read node parameters
    movlw 0x53      ;FLiM learn mode
    subwf Rx0d0,W
    bz    fl_lrn1
    movlw 0x54      ;FLiM out of learn mode
    subwf Rx0d0,W
    bz    fl_ulrn1      
    movlw 0x72      ;read back event by index
    subwf Rx0d0,W
    bz    readEV1
    movlw 0x96      ;set a NV
    subwf Rx0d0,W
    bz    setNV
    movlw 0x71      ;read a NV
    subwf Rx0d0,W
    bz    readNV
    movlw 0xF5      ;FLiM teach event
    subwf Rx0d0,W
    bz    fl_teach
    movlw 0x55      ;clear events back to default
    subwf Rx0d0,W
    bz    undo1
    bra   main2

    

reboot
    call  thisNN
    sublw 0
    bnz   notNN
reboot1 movlw 0xFF
    movwf EEADR
    movlw 0xFF
    call  eewrite     ;set last EEPROM byte to 0xFF
    reset         ;software reset to bootloader 

para1a  call  thisNN      ;read parameter by index
    sublw 0
    bnz   notNN
    call  para1rd
    bra   main2
      
main2 bcf   Datmode,0
    goto  main      ;loop
    
setNN btfss Datmode,2   ;in NN set mode?
    bra   main2     ;no
    call  putNN     ;put in NN
    bcf   Datmode,2
    bsf   Datmode,3
    movlw .10
    movwf Keepcnt     ;for keep alive
    movlw 0x52
    call  nnrel     ;confirm NN set
    call  modeload
    call  buf_init
    bsf   PORTB,6     ;LED ON
    bra   main2
    
sendNN  btfss Datmode,2   ;in NN set mode?
    bra   main2     ;no
    movlw 0x50      ;send back NN
    movwf Tx1d0
    movlw 3
    movwf Dlc
    call  sendTX
    bra   main2
    
setNV 
    call  thisNN      ;set a new NV
    sublw 0
    bnz   notNN
    movf  Rx0d3,W
    bz    notNX     ;index of 0 is invalid
    movf  Rx0d3,W
    sublw NV_NUM
    bn    notNX     ;too many
    
    movlw LOW NVstart     ;get pointer in EEPROM
    addwf Rx0d3,W
    movwf EEADR
    decf  EEADR,F     ;address offset is 0 to NV_NUM-1
    movf  Rx0d4,W
    call  eewrite     ;write new value
    call  buf_init
    call  modeload
    
    bra   main2

readNV
    call  thisNN      ;is it this node?
    sublw 0
    bnz   notNN
    movf  Rx0d3,W
    bz    notNX     ;index of 0 is invalid
    movf  Rx0d3,W
    sublw NV_NUM
    bn    notNX     ;too many
    movlw LOW NVstart   ;get pointer in EEPROM
    addwf Rx0d3,W
    movwf EEADR
    decf  EEADR,F     ;address is 0 to NV_NUM -1
    call  eeread
    movwf Tx1d4     ;NV value
    movff Rx0d3,Tx1d3   ;rewrite index value
rdNV1 movlw 0x97
    movwf Tx1d0
    movlw 5
    movwf Dlc
    call  sendTX
    bra   main2

notNX clrf  Tx1d3     ;invalid index
    clrf  Tx1d4
    bra   rdNV1

fl_lrn
    call  thisNN
    sublw 0
    bnz   notNN
    bsf   Mode,4      ;set FLiM learn mode
    bra   main2
fl_ulrn btfss Mode,1      ;is it in FLiM?
    bra   main2
    call  thisNN
    sublw 0
    bnz   notNN
    bcf   Mode,4      ;clear FLiM learn mode
    bra   main2

fl_teach
    bra   teach_f       ;put new event into FLASH

undo
    call  thisNN
    sublw 0
    bnz   notNN
    btfss Mode,4      ;is it in FLiM learn?
    bra   main2
    call  fill_buf
    call  wrack
    bra   main2

notNN bra   main2     ;not here

params  btfss Datmode,2   ;only in setup mode
    bra   main2
    call  parasend
    bra   main2

teach movlw B'00001100'   ;is it learn and button pressed?
    subwf Mode,W
    bnz   l_out2      ;no so do nothing
    call  learn
    bcf   Mode,3
    bcf   Mode,2
    bsf   PORTB,7     ;green steady
    bra   l_out2

teach_f btfss Mode,4      ;is it in learn mode?
    bra   l_out2
    movff Rx0d5,Index   ;Rx0d7 has index
    decf  Index,F     ;sent as base 1
  
    call  learn
    call  wrack   ;send write acknowledge
    bra   l_out2

readEV  
    call  thisNN
    sublw 0
    bnz   l_out2
    call  read
    bra   l_out2
    
l_out bcf   Datmode,4
    bcf   PORTB,6
l_out1  bcf   Datmode,6
l_out2  bcf   Datmode,0
    

    clrf  PCLATH
    goto  main2

  
do    btfss Mode,1      ;is it FLiM?
    bra   do2
    btfss Datmode,3   ;ignore if not set up
    bra   do1
    btfsc Datmode,2   ;don't do if in setup   
    bra   do1
do2   call  scan      ;scan inputs for change
    
                
do1   goto  main
    
    
    
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
    
  
    movlw B'00110000'   ;Port A 0 to 3 are column select, 4 is setup input
    movwf TRISA     ;
    movlw B'00111000'   ;RB2 = CANTX, RB3 = CANRX, RB4,5 are logic 
                ;input - RB6,7 for debug and diagnostics LEDs
    movwf TRISB
    bsf   PORTB,2     ;CAN recessive
    bcf   PORTB,6
    bcf   PORTB,7
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
    movlb .15       ;block extended frames
    bcf   RXF1SIDL,3    ;standard frames
    bcf   RXF0SIDL,3    ;standard frames
    bcf   RXB0CON,RXM1  ;frame type set by RXFnSIDL
    bcf   RXB0CON,RXM0
    bcf   RXB1CON,RXM1
    bcf   RXB1CON,RXM0
    movlb 0
  
    clrf  CANCON      ;out of CAN setup mode
    clrf  CCP1CON
    movlw B'10000100'
    movwf T0CON     ;set T0 for LED flash
    clrf  Tx1con
    movlw B'00100011'   ;High priority IRQ for CAN interrupts
    movwf IPR3      ;high priority CAN RX and Tx error interrupts(for now)
    clrf  IPR1      ;all peripheral interrupts are low priority
    clrf  IPR2
    clrf  PIE2
    clrf  PIR1
    clrf  PIR2


;next segment required
    
  
    movlw B'00000001'
    movwf IDcount     ;set at lowest value for starters
    clrf  INTCON2     ;
    clrf  INTCON3     ;
    

    movlw B'00100011'   ;B'00100011'Rx0 and RX1 interrupt and Tx error
              
    movwf PIE3

    
    
    
no_load   call  modeload    ;get FLiM mode from EEPROM
    clrf  Mode2
    btfsc M_PORT,M_BIT  ;initialise SLiM mode for scan
    bsf   Mode2,0
  
;********************************************

;   initialise buffer   

    call  buf_init  
    

  
;********************************************** 

    ;   test for setup mode
    clrf  Mode
    movlw Modstat     ;get setup status
    movwf EEADR
    call  eeread
    movwf Datmode
    sublw 0       ;is SLiM mode
    bnz   setid
    bra   slimset     ;set up in SLiM mode
    
  
setid bsf   Mode,1      ;flag FLiM
    call  newid_f     ;put ID into Tx1buf, TXB2 and ID number store
    
seten_f   
    movlb .15
    bcf RXB1CON,RXFUL
    movlb 0
    bcf RXB0CON,RXFUL   ;ready for next
    bcf COMSTAT,RXB0OVFL  ;clear overflow flags if set
    bcf COMSTAT,RXB1OVFL
    clrf  PIR3      ;clear all flags
    movlw B'11000000'
    movwf INTCON      ;enable interrupts
    bcf   PORTB,7
    bsf   PORTB,6     ;RUN LED on. (yellow for FLiM)
    bcf   Datmode,0
    goto  main

slimset movlw B'00110000'   ;get DIP switch setting
    andwf PORTB,W
    movwf Temp
    swapf Temp,W
    
    andlw B'00000011'
    
    addlw 1       ;NN start at 1
    movwf Atemp     ;for any changes

    clrf  NN_temph
    movwf NN_templ
    movlw LOW NodeID+1
    movwf EEADR
    call  eeread
    subwf NN_templ,W
    bz    not_new
    movf  NN_templ,W
    call  eewrite
    call  fill_buf
;   movwf CANid
;   call  putNNs      ;put in SLiM NN
;   call  setid1
not_new bcf   Mode,0
    
    bcf   Mode,1      ;not FLiM
  
    movf  NN_templ,W
    call  newid1      ;put ID into Tx1buf, TXB2 and ID number store
    movlw LOW Node_st
    movwf EEADR
    call  eeread
    sublw 0
    bnz   seten     ;table already loaded
    call  fill_buf
    call  wrack
    
    ;
seten   
    movlb .15
    bcf RXB1CON,RXFUL
    movlb 0
    bcf RXB0CON,RXFUL   ;ready for next
    bcf COMSTAT,RXB0OVFL  ;clear overflow flags if set
    bcf COMSTAT,RXB1OVFL
    clrf  PIR3      ;clear all flags
    
    movlw B'11000000'
    movwf INTCON      ;enable interrupts
    bcf   PORTB,6
    bsf   PORTB,7     ;RUN LED on. Green for SLiM
    goto  main
  

setloop   
    movlb .15
    bcf RXB1CON,RXFUL
    movlb 0
    bcf RXB0CON,RXFUL   ;ready for next
    bcf COMSTAT,RXB0OVFL  ;clear overflow flags if set
    bcf COMSTAT,RXB1OVFL
    clrf  PIR3      ;clear all flags
    movlw B'11000000'
    movwf INTCON      ;enable interrupts
    bsf   Datmode,1   ;setup mode

    call  enum      ;sends RTR frame
    bra   main      
    
;****************************************************************************
;   start of subroutines  



;   Send contents of Tx1 buffer via CAN TXB1

sendTX1 lfsr  0,Tx1con
    lfsr  1,TXB1CON
    
    movlb .15       ;check for buffer access
ldTx2 btfsc TXB1CON,TXREQ ; Tx buffer available...?
    bra   ldTx2     ;...not yet
    movlb 0
    
ldTX1 movf  POSTINC0,W
    movwf POSTINC1    ;load TXB1
    movlw Tx1d7+1
    cpfseq  FSR0L
    bra   ldTX1
    movlb .15       ;bank 15
tx1test btfsc TXB1CON,TXREQ ;test if clear to send
    bra   tx1test
    bsf   TXB1CON,TXREQ ;OK so send
    
tx1done movlb 0       ;bank 0
    return          ;successful send

    
sendTX  ;movff  NN_temph,Tx1d1
    ;movff  NN_templ,Tx1d2

sendTXa movf  Dlc,W       ;get data length
    movwf Tx1dlc
    movlw B'00001111'   ;clear old priority
    andwf Tx1sidh,F
    movlw B'10110000'
    iorwf Tx1sidh     ;low priority
    movlw .10
    movwf Latcount
    call  sendTX1     ;send frame
    return          


;*********************************************************************
;   put in NN from command

putNN movff Rx0d1,NN_temph
    movff Rx0d2,NN_templ
    movlw LOW NodeID
    movwf EEADR
    call  eeread
    subwf NN_temph,W
    bnz   notsame
    incf  EEADR
    call  eeread
    subwf NN_templ,W
    bnz   notsame
    bra   same
notsame movlw LOW NodeID    ;only reload table if NN is changed
    movwf EEADR
    movf  Rx0d1,W
    call  eewrite
    incf  EEADR
    movf  Rx0d2,W
    call  eewrite
    call  fill_buf      ;create new FLASH lookup table
    call  wrack
same  movlw Modstat
    movwf EEADR
    movlw B'00001000'   ;Module status has NN set
    call  eewrite
    return
  
;***********************************************************************

putNNs  movlw LOW NodeID    ;put SLiM NN into EEPROM
    movwf EEADR
    movff Atemp,NN_templ
    movf  NN_temph,W
    call  eewrite
    incf  EEADR
    movf  NN_templ,W
    call  eewrite
    movlw LOW CANid1
    movwf EEADR
    movf  NN_templ,W
    call  eewrite
    return

;**************************************************************************
    


newid1  movwf CanID_tmp           
    call  shuffle
    movlw B'11110000'
    andwf Tx1sidh
    movf  IDtemph,W   ;set current ID into CAN buffer
    iorwf Tx1sidh     ;leave priority bits alone
    movf  IDtempl,W
    movwf Tx1sidl     ;only top three bits used
    movlb .15       ;put ID into TXB2 for enumeration response to RTR
new_1 btfsc TXB2CON,TXREQ ;wait till sent
    bra   new_1
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
    
nnack btfss Mode,1      ;FLiM?
    return
    movlw 0x50      ;request frame for new NN or ack if not virgin
nnrel movwf Tx1d0
    movff NN_temph,Tx1d1
    movff NN_templ,Tx1d2
    movlw 3
    movwf Dlc
    call  sendTX
    return


;**********************************************************************
    

newid_f   movlw LOW CANid1      ;put in stored ID. FLiM mode
    movwf EEADR
    bsf   EECON1,RD
    movf  EEDATA,W
    movwf CanID_tmp     
    call  shuffle
    movlw B'11110000'
    andwf Tx1sidh
    movf  IDtemph,W   ;set current ID into CAN buffer
    iorwf Tx1sidh     ;leave priority bits alone
    movf  IDtempl,W
    movwf Tx1sidl     ;only top three bits used
    movlw LOW NodeID
    movwf EEADR
    call  eeread
    movwf NN_temph      ;get stored NN
    incf  EEADR
    call  eeread
    movwf NN_templ  
    
    movlb .15       ;put ID into TXB2 for enumeration response to RTR
new_1f  btfsc TXB2CON,TXREQ
    bra   new_1f
    clrf  TXB2SIDH
    movf  IDtemph,W
    movwf TXB2SIDH
    movf  IDtempl,W
    movwf TXB2SIDL
    movlw 0xB0
    iorwf TXB2SIDH    ;set priority
    clrf  TXB2DLC     ;no data, no RTR
    movlb 0
    btfsc Datmode,3   ;already set up?
    return


    
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

;*********************************************************************************

;   reverse shuffle for incoming ID. sidh and sidl into one byte.

shuffin movff Rx0sidl,IDtempl
    swapf IDtempl,F
    rrncf IDtempl,W
    andlw B'00000111'
    movwf IDtempl
    movff Rx0sidh,IDtemph
    rlncf IDtemph,F
    rlncf IDtemph,F
    rlncf IDtemph,W
    andlw B'01111000'
    iorwf IDtempl,W     ;returns with ID in W
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
    movff INTCON,TempINTCON
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
    
    movff TempINTCON,INTCON   ;reenable interrupts
    
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
;   longer delay

ldely movlw .100
    movwf Count2
ldely1  call  dely
    decfsz  Count2
    bra   ldely1
    
    return

;******************************************************************

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
    btfsc Mode,1      ;FLiM?
    bra   scan4
    btfsc Mode2,0
    bra   bscan
    bra   scan3
scan4 movlw 1       ;what scan mode?
    subwf Mode1,W
    bz    bscan     ;pushbutton only mode
    movlw 4
    subwf Mode1,W
    bz    bscan
    movlw 5
    subwf Mode1,W
    bz    bscan
    
scan3 movf  Oldrow,W    ;get old row
    xorwf Row,W     ;compare with new
    
    bnz   change      ;a change
scan2 incf  Ccount
    btfss Ccount,4    ;more than 15?
    bra   scan1     ;next column
    return          ;finish scan    
     
change  movwf Bitcng      ;hold the bit change
    btfss Mode1,1     ;is it half toggles
    bra   tog
    movlw B'00001111'   ;not a toggle change in modes 2 or 3
    andwf Bitcng,W
    bz    bscan
tog   clrf  Bitcnt      ;which bit?
    clrf  Index     ;table enry
change1 rrcf  Bitcng,F    ;rotate to find which bit changed
    bc    gotbit
    incf  Bitcnt,F
    bra   change1
    
gotbit  movf  Ccount,W    ;set up to calculate point number
    movwf Index
    rlncf Index,F
    rlncf Index,W   ;multiply by 4
    addwf Bitcnt,W    ;add the row number
    movwf Index     ;got the point number
  
    movlw 3       ;do offset for upper rows
    cpfsgt  Bitcnt
    bra   restore     ;send CAN packet
    movlw .60       ;offset
    addwf Index,F
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
    
sendpkt btfss Mode,2      ;is it learn mode?
    bra   sndpkt2
    bsf   Mode,3      ;has got button push
    return          ;yes so don't send
sndpkt2 call  get_event   ;gets event from table into Tx1
    btfsc Mode,4      ;FLiM learn?
    goto  snd_inx     ;send event with index
    
    movf  Tx1d1,F
    bnz   long
    movf  Tx1d2,F
    bnz   long

    btfsc Turn,0      ;which direction?
    bra   turnoff_s
    movlw 0x98      ;set command
    movwf Tx1d0
    bra   sndpkt4
turnoff_s movlw 0x99      ;unset command
    movwf Tx1d0     ;put in CAN frame
sndpkt4 movlw 5
    movwf Dlc       ;5 byte command

    movff NN_temph,Tx1d1  ;put in node NN for traceability
    movff NN_templ,Tx1d2
    bra sndpkt3


long  btfsc Turn,0      ;which direction?
    bra   turnoff
    movlw 0x90      ;set command
    bra   sndpkt1
turnoff movlw 0x91      ;unset command
sndpkt1 movwf Tx1d0     ;put in CAN frame
    bra   sndpkt4

sndpkt3 
sndpkt6   movlw B'00001111'   ;clear last priority
    andwf Tx1sidh,F
    movlw B'10110000'   ;starting priority
    iorwf Tx1sidh,F
    
    
    
  
    movlw .10
    movwf Latcount
    call  sendTX      ;send CAN frame
  
    
    bra   scan1         ;for now      
    
    
    
bscan btfsc Mode1,1     ;is it a half row state
    bra   bscan3
;   comf  Row,F     ;button scan routine here. 
  
    
bscan2  movf  Row,W
    subwf Oldrow,W    ;has any bit changed?  (button still pressed?)
    bz    scan2     ;no change
;   movf  Row,F
    incf  Row,W
    bnz   bchange     ;all now released?
    movlw 0xFF
    movwf INDF0     ;old row back to clear
    
    bra   scan2     ;

bchng2a goto  bchng2      ;branch too long

bchange ;movf Row,W
    ;andwf  Oldrow,F    ;clear any released buttons
    incf  Oldrow,W
    bnz   scan2
    comf  Row,F     ;asumes only one button pressed
    movlw 5
    subwf Mode1,W     ;is it all ON PBs?
    bz    bchng2a
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
n1    btfss Oldrow,0    ;is it still ON
    bra   scan2     ;do nothing
    bcf   Oldrow,0    ;set on
    movf  Oldrow,W
  
    movwf INDF0
    clrf  Turn      ;set direction
    rlncf Ccount,W    ;double column count
    movwf Index
    bra   sendpkt
r1    btfss Oldrow,1    ;get old row data
    bra   scan2
    bcf   Oldrow,1
    movf  Oldrow,W
    
    movwf INDF0
    bsf   Turn,0      ;set direction
    rlncf Ccount,W    ;double column count
    movwf Index
    bra   sendpkt
n2    btfss Oldrow,2    ;get old row data
    bra   scan2
    bcf   Oldrow,2
    movf  Oldrow,W
    
    movwf INDF0
    clrf  Turn      ;set direction
    rlncf Ccount,W    ;double column count
    movwf Index
    incf  Index
    bra   sendpkt
r2    btfss Oldrow,3    ;get old EEPROM row data
    bra   scan2
    bcf   Oldrow,3
    movf  Oldrow,W
    movwf INDF0
    bsf   Turn,0      ;set direction
    rlncf Ccount,W    ;double column count
    movwf Index
    incf  Index
    bra   sendpkt
n3    btfss Oldrow,4    ;get old EEPROM row data
    bra   scan2
    bcf   Oldrow,4
    movf  Oldrow,W
    movwf INDF0
    movlw 4
    subwf Mode1,W     ;is it half PB pairs?
    bz    n3b
    clrf  Turn      ;set direction
    rlncf Ccount,W    ;double column count
    addlw .32       ;point no. offset
    movwf Index
    bra   sendpkt
r3    btfss Oldrow,5    ;get old EEPROM row data
    bra   scan2
    bcf   Oldrow,5
    movf  Oldrow,W
    
    movwf INDF0
    movlw 4
    subwf Mode1,W     ;is it half PB pairs?
    bz    r3b
    bsf   Turn,0      ;set direction
    rlncf Ccount,W    ;double column count
    addlw .32       ;point no. offset
    movwf Index
    bra   sendpkt
n4    btfss Oldrow,6    ;get old row data
    bra   scan2
    bcf   Oldrow,6
    movf  Oldrow,W
  
    movwf INDF0
    movlw 4
    subwf Mode1,W     ;is it half PB pairs?
    bz    n4b
    clrf  Turn      ;set direction
    rlncf Ccount,W    ;double column count
    addlw .32       ;point no. offset
    movwf Index
    incf  Index
    bra   sendpkt
r4    btfss Oldrow,7    ;get old row data
    bra   scan2
    bcf   Oldrow,7
    movf  Oldrow,W
  
    movwf INDF0
    movlw 4
    subwf Mode1,W     ;is it half PB pairs?
    bz    r4b
    bsf   Turn,0      ;set direction
    rlncf Ccount,W    ;double column count  
    addlw .32       ;point no. offset
    movwf Index
    incf  Index
    bra   sendpkt

n3b   rlncf Ccount,W    ;double column count
    rlncf WREG  
    addlw .32       ;point no. offset
    movwf Index
    clrf  Turn      ;ON only
    bra   sendpkt

r3b   rlncf Ccount,W    ;double column count
    rlncf WREG  
    addlw .32       ;point no. offset
    movwf Index
    incf  Index,F
    clrf  Turn      ;ON only
    bra   sendpkt

n4b   rlncf Ccount,W    ;double column count
    rlncf WREG  
    addlw .32       ;point no. offset
    movwf Index
    incf  Index,F
    incf  Index,F
    clrf  Turn      ;ON only
    bra   sendpkt

r4b   rlncf Ccount,W    ;double column count
    rlncf WREG  
    addlw .32       ;point no. offset
    movwf Index
    incf  Index,F
    incf  Index,F
    incf  Index,F
    clrf  Turn      ;ON only
    bra   sendpkt

bchng2    rrcf  Row,F     ;roll row to see which bit it was
    bc    n1c       ;first point normal
    rrcf  Row,F
    bc    r1c       ;first point reset
    rrcf  Row,F
    bc    n2c
    rrcf  Row,F
    bc    r2c
    rrcf  Row,F
    bc    n3c
    rrcf  Row,F
    bc    r3c
    rrcf  Row,F
    bc    n4c
    rrcf  Row,F
    bc    r4c
n1c   btfss Oldrow,0    ;is it still ON
    bra   scan2     ;do nothing
    bcf   Oldrow,0    ;set on
    movf  Oldrow,W
    movwf INDF0
    clrf  Turn      ;set direction
    rlncf Ccount,W    ;double column count
    rlncf WREG
    movwf Index
    bra   sendpkt
r1c   btfss Oldrow,1    ;get old row data
    bra   scan2
    bcf   Oldrow,1
    movf  Oldrow,W
    movwf INDF0
    bcf   Turn,0      ;set direction
    rlncf Ccount,W    ;double column count
    rlncf WREG
    movwf Index
    incf  Index,F
    bra   sendpkt
n2c   btfss Oldrow,2    ;get old row data
    bra   scan2
    bcf   Oldrow,2
    movf  Oldrow,W
    
    movwf INDF0
    clrf  Turn      ;set direction
    rlncf Ccount,W    ;double column count
    rlncf WREG
    movwf Index
    incf  Index,F
    incf  Index,F
    bra   sendpkt
r2c   btfss Oldrow,3    ;get old EEPROM row data
    bra   scan2
    bcf   Oldrow,3
    movf  Oldrow,W
    movwf INDF0
    bcf   Turn,0      ;set direction
    rlncf Ccount,W    ;double column count
    rlncf WREG
    movwf Index
    incf  Index
    incf  Index,F
    incf  Index,F
    bra   sendpkt
n3c   btfss Oldrow,4    ;get old EEPROM row data
    bra   scan2
    bcf   Oldrow,4
    movf  Oldrow,W
    movwf INDF0
    
    clrf  Turn      ;set direction
    rlncf Ccount,W    ;double column count
    rlncf WREG
    addlw .64       ;point no. offset
    movwf Index
    bra   sendpkt
r3c   btfss Oldrow,5    ;get old EEPROM row data
    bra   scan2
    bcf   Oldrow,5
    movf  Oldrow,W
    movwf INDF0
    bcf   Turn,0      ;set direction
    rlncf Ccount,W    ;double column count
    rlncf WREG
    addlw .64       ;point no. offset
    movwf Index
    incf  Index,F
    bra   sendpkt
n4c   btfss Oldrow,6    ;get old row data
    bra   scan2
    bcf   Oldrow,6
    movf  Oldrow,W
    movwf INDF0
    clrf  Turn      ;set direction
    rlncf Ccount,W    ;double column count
    rlncf WREG
    addlw .64       ;point no. offset
    movwf Index
    incf  Index,F
    incf  Index,F
    bra   sendpkt
r4c   btfss Oldrow,7    ;get old row data
    bra   scan2
    bcf   Oldrow,7
    movf  Oldrow,W
    movwf INDF0
    bcf   Turn,0      ;set direction
    rlncf Ccount,W    ;double column count
    rlncf WREG  
    addlw .64       ;point no. offset
    movwf Index
    incf  Index,F
    incf  Index,F
    incf  Index,F
    bra   sendpkt

bscan3  movff Row,Row1      ;here for half PB mode
  ; comf  Row1,F
    movff Oldrow,Oldrow1
    movlw B'00001111'     ;mask non-PB rows
    iorwf Oldrow1,F
    iorwf Row1,F
    movf  Row1,W
    subwf Oldrow1,W
    bz    scan2a
    incf  Row1,W
    bnz   bchng1
    movlw B'11110000'
    iorwf INDF0,F       ;half row back to 1111
    bra   scan2a

bchng1  incf  Oldrow1,W   ;all previous buttons released?
    bnz   scan2a  
    comf  Row1,F  
    swapf Row1,F      ;top 4 bits
    rrcf  Row1,F      ;roll row to see which bit it was
    bc    n1a       ;first point normal
    rrcf  Row1,F
    bc    r1a       ;first point reset
    rrcf  Row1,F
    bc    n2a
    rrcf  Row1,F
    bc    r2a
  
  
n1a   btfss Oldrow,4    ;is it still ON
    bra   scan2     ;do nothing
    bcf   Oldrow,4    ;set on
    movf  Oldrow,W
  
    movwf INDF0
    clrf  Turn      ;set direction
    rlncf Ccount,F
    movf  Ccount,W    ;double column count
    btfsc Mode1,0     ;on only?
    rlncf Ccount,W
    addlw .64       ;point no. offset
    movwf Index

    bra   sendpkt
r1a   btfss Oldrow,5    ;get old row data
    bra   scan2
    bcf   Oldrow,5
    movf  Oldrow,W
    
    movwf INDF0
    btfss Mode1,0     ;on only
    bsf   Turn,0      ;set direction
    rlncf Ccount,F
    movf  Ccount,W    ;double column count
    btfsc Mode1,0     ;on ony?
    rlncf Ccount,W
    addlw .64       ;point no. offset
    movwf Index
    btfsc Mode1,0     ;on ony?
    incf  Index,F
    
    bra   sendpkt
n2a   btfss Oldrow,6    ;get old row data
    bra   scan2
    bcf   Oldrow,6
    movf  Oldrow,W
    
    movwf INDF0
    clrf  Turn      ;set direction
    rlncf Ccount,F
    movf  Ccount,W    ;double column count
    btfsc Mode1,0     ;on ony?
    rlncf Ccount,W
    addlw .64       ;point no. offset
    btfsc Mode1,0     ;on ony?
    addlw 1
    movwf Index 
    incf  Index
    bra   sendpkt
r2a   btfss Oldrow,7    ;get old EEPROM row data
    bra   scan2
    bcf   Oldrow,7
    movf  Oldrow,W
  
    movwf INDF0
    btfss Mode1,0     ;on only
    bsf   Turn,0      ;set direction
    rlncf Ccount,F
    movf  Ccount,W    ;double column count
    
    btfsc Mode1,0     ;on ony?
    rlncf Ccount,W
    addlw .64       ;point no. offset
    btfsc Mode1,0     ;on ony?
    addlw 2
    movwf Index   ;moves index
    
    bra   sendpkt

scan2a  goto  scan2

snd_inx movlw 0xB0
    movwf Tx1d0   ;event with data byte
    rrncf Index,F   ;pointno has been doubled in get_event
    bcf   Index,7   ;clear any carry
    movff Index,Tx1d5
    incf  Tx1d5,F   ;pointno is index +1
    movlw 6
    movwf Dlc
    bra   sndpkt6
;********************************************************************

; initialise matrix buffer

buf_init clrf Ccount      ;column count
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

    return

;***************************************************************
      
;     load mode from EEPROM
modeload  movlw LOW NVstart
      movwf EEADR
      call  eeread
      movwf Mode1
      
      return
    



;**************************************************************************

;   check if command is for this node

thisNN  movf  NN_temph,W
    subwf Rx0d1,W
    bnz   not_NN
    movf  NN_templ,W
    subwf Rx0d2,W
    bnz   not_NN
    retlw   0     ;returns 0 if match
not_NN  retlw 1

;************************************************************
;   send node parameter bytes (7 maximum)

parasend  
    movlw 0xEF
    movwf Tx1d0
    movlw LOW node_ID
    movwf TBLPTRL
    movlw 8
    movwf TBLPTRH   ;relocated code
    lfsr  FSR0,Tx1d1
    movlw 7
    movwf Count
    bsf   EECON1,EEPGD
    
para1 tblrd*+
    movff TABLAT,POSTINC0
    decfsz  Count
    bra   para1
    bcf   EECON1,EEPGD  
    movlw 8
    movwf Dlc
    call  sendTXa
    return

;**********************************************************************************

enum  clrf  Tx1con      ;CAN ID enumeration. Send RTR frame, start timer
    clrf  Enum0
    clrf  Enum1
    clrf  Enum2
    clrf  Enum3
    clrf  Enum4
    clrf  Enum5
    clrf  Enum6
    clrf  Enum7
    clrf  Enum8
    clrf  Enum9
    clrf  Enum10
    clrf  Enum11
    clrf  Enum12
    clrf  Enum13

  
    
    movlw B'10111111'   ;fixed node, default ID  
    movwf Tx1sidh
    movlw B'11100000'
    movwf Tx1sidl
    movlw B'01000000'   ;RTR frame
    movwf Dlc
    
    movlw 0x3C      ;set T3 to 100 mSec (may need more?)
    movwf TMR3H
    movlw 0xAF
    movwf TMR3L
    movlw B'10110001'
    movwf T3CON     ;enable timer 3
;   bsf   Datmode,1   ;used to flag setup state
    movlw .10
    movwf Latcount
    
    call  sendTXa     ;send RTR frame
    clrf  Tx1dlc      ;prevent more RTR frames
    return

;**********************************************************

;   send individual parameter

para1rd movlw 0x9B
    movwf Tx1d0
    movlw LOW node_ID
    movwf TBLPTRL
    movlw 8
    movwf TBLPTRH   ;relocated code
    decf  Rx0d3,W
    addwf TBLPTRL
    bsf   EECON1,EEPGD
    tblrd*
    movff TABLAT,Tx1d4
    bcf   EECON1,EEPGD
    movff Rx0d3,Tx1d3
    movlw 5
    movwf Dlc
    movff NN_temph,Tx1d1
    movff NN_templ,Tx1d2
    call  sendTX
    return  

;**********************************************************************

;   new enumeration scheme
;   here with enum array set
;
new_enum  movff FSR1L,Fsr_tmp1Le  ;save FSR1 just in case
      movff FSR1H,Fsr_tmp1He 
      clrf  IDcount
      incf  IDcount,F     ;ID starts at 1
      clrf  Roll
      bsf   Roll,0
      lfsr  FSR1,Enum0      ;set FSR to start
here1   incf  INDF1,W       ;find a space
      bnz   here
      movlw 8
      addwf IDcount,F
      incf  FSR1L
      bra   here1
here    movf  Roll,W
      andwf INDF1,W
      bz    here2
      rlcf  Roll,F
      incf  IDcount,F
      bra   here
here2   movlw .99         ;limit to ID
      cpfslt  IDcount
      call  segful        ;segment full
      movff Fsr_tmp1Le,FSR1L  ;
      movff Fsr_tmp1He,FSR1H 
      return

segful    movlw 7   ;segment full, no CAN_ID allocated
      call  errsub
      setf  IDcount
      bcf   IDcount,7
      return

;*************************************************************************

errsub  movwf Tx1d3   ;main eror message send. Error no. in WREG
    movlw 0x6F
    movwf Tx1d0
    movlw 4
    movwf Dlc
    call  sendTX
    return



    ; learn an EN. 
    ; set ENcount to event number
    ; now get table pointer

learn movff Index,ENtempl   ;Index for table is base 0

    clrf  ENtemph
    rlncf ENtempl,F     ;double it for pointing to Table 
    bcf   STATUS,C
    rlcf  ENtempl,F     ;double it
    rlcf  ENtemph,F     ;if carry from ENtempl
    movlw B'11000000'
    andwf ENtempl,W
    movwf TBLPTRL       ;start of 64 byte block
    movf  ENtemph,W
    addlw 0x20
    movwf TBLPTRH
    
    movlw .64         ;read block 
    movwf Flcount
    lfsr  FSR2,Flash_buf    ;point to holding buffer
    clrf  TBLPTRU
    
    
read_block  
    tblrd*+       ;read into TABLAT and increment
    movf  TABLAT,W
    movwf POSTINC2
    decfsz  Flcount
    bra   read_block
    
    
    lfsr  FSR2,Flash_buf
    movf  ENtempl,W
    andlw B'00111111'     ;64 byte range
    
    addwf FSR2L,F       ;FSR2 points to EN to change
    movf  POSTINC2,W      ;get old NN
    subwf Rx0d1,W
    bnz   new
    movf  POSTINC2,W      ;get old NN
    subwf Rx0d2,W
    bnz   new
    movf  POSTINC2,W      ;get old NN
    subwf Rx0d3,W
    bnz   new
    movf  POSTINC2,W      ;get old NN
    subwf Rx0d4,W
    bnz   new
    btfsc Mode,1        ;is it FLiM?
    bra   new
    lfsr  FSR2,Flash_buf    ;here if same so undo it
    movf  ENtempl,W
    andlw B'00111111'     ;64 byte range
    addwf FSR2L,F       ;FSR2 points to EN to change
    movff NN_temph,POSTINC2 ;restore original
    movff NN_templ,POSTINC2
    clrf  POSTINC2
    incf  Index,W
    movwf POSTINC2
    bra   new1

new   lfsr  FSR2,Flash_buf
    movf  ENtempl,W
    andlw B'00111111'     ;64 byte range
    
    addwf FSR2L,F       ;FSR2 points to EN to change

    movff Rx0d1,POSTINC2    ;NNhi
    movff Rx0d2,POSTINC2    ;NNlo
    movff Rx0d3,POSTINC2    ;ee hi
    movff Rx0d4,POSTINC2    ;ee lo

new1  call  erase       ;erase block
    movf  ENtempl,W     ;point to start of block rewrite
    movwf TBLPTRL
    movf  ENtemph,W
    addlw 0x20
    movwf TBLPTRH 
    call  f_write       ;write buffer back  

    return

;***************************************************
;   erase page of flash
    
erase movf  ENtempl,W     ;point to start of block to erase
    movwf TBLPTRL
    movf  ENtemph,W
    addlw 0x20
    movwf TBLPTRH
    bsf   EECON1,EEPGD    ;set up for erase
    bcf   EECON1,CFGS
    bsf   EECON1,WREN
    bsf   EECON1,FREE
    clrf  INTCON
    
    movlw 0x55
    movwf EECON2
    movlw 0xAA
    movwf EECON2
    bsf   EECON1,WR     ;erase
er_done   btfsc EECON1,WR     ;check erase is done
    movlw B'11000000'
    movwf INTCON        ;reenable interrupts
    return
    
;*********************************************************************

;   write flash buffer to flash
;   set table pointer first   
    
f_write movlw B'11000000'
    andwf TBLPTRL,F     ;put to 64 byte boundary
    tblrd*-           ;back 1
    lfsr  FSR2,Flash_buf    ;ready for rewrite
write2  movlw .8
    movwf Flcount
write movlw .8
    movwf Flcount1
  
    
write1  movf  POSTINC2,W
    movwf TABLAT
    tblwt+*
    decfsz  Flcount1
    bra   write1
    bsf   EECON1,EEPGD    ;set up for write back
    bcf   EECON1,CFGS
    bcf   EECON1,FREE
    bsf   EECON1,WREN
  
    clrf  INTCON
    
    
    movlw 0x55
    movwf EECON2
    movlw 0xAA
    movwf EECON2
    bsf   EECON1,WR     ;write back
wr_done   btfsc EECON1,WR     ;write done?
    bra wr_done
  
    decfsz  Flcount
    bra   write       ;next block of 8
    movlw B'11000000'
    movwf INTCON        ;reenable interrupts
    return

;**************************************************************
; fills flash buffer on first time only - or on full reset
; loses any stored non-default events

fill_buf clrf ENtempl     ;start at beginning
    clrf  ENtemph
    movlw .16       ;set for erase all. 16 blocks
    movwf Count
    clrf  TBLPTRU     ;set to flash start
    movlw 0x20
    movwf TBLPTRH
    clrf  TBLPTRL
    tblrd*-
c_buf call  erase
    decfsz  Count
    bra   c_buf1  
    clrf  Count1      ;button number
    incf  Count1      ;button numbers start at 1
    movlw 8
    movwf Count2
    clrf  TBLPTRU     ;set to flash start
    movlw 0x20
    movwf TBLPTRH
    clrf  TBLPTRL
    tblrd*-

fb1   lfsr  FSR2,Flash_buf
    movlw .16
    movwf Count
fb2   movff NN_temph,POSTINC2 
    movff NN_templ,POSTINC2
    clrf  POSTINC2
    movff Count1,POSTINC2
    incf  Count1
    decfsz  Count
    bra   fb2 
  
  

    lfsr  FSR2,Flash_buf  ;start of line
    call  write2      ;write block of 64
    decfsz  Count2      ;last block?
    bra   fb1
    
    nop
    movlw LOW Node_st   ;reset loaded status
    movwf EEADR
    movlw 1
    call  eewrite
    return

c_buf1  movlw .64       ;next block to erase
    bcf   STATUS,C
    addwf ENtempl
    bnc   c_buf
    incf  ENtemph
    bra   c_buf

;****************************************************************
;   gets event from FLASH table
;   Uses index
;   Result in Tx1 buffer

get_event clrf  TBLPTRU
      movlw 0x20
      movwf TBLPTRH
      rlcf  Index,F
      bcf   STATUS,C
      rlcf  Index,W
      bnc   get_1
      incf  TBLPTRH,F
get_1   andlw B'11111100'   ;mask ls bits (blocks of 4)
      movwf TBLPTRL
      tblrd*+       ;read into TABLAT and increment
      movf  TABLAT,W
      movwf Tx1d1   ;NNhi
      tblrd*+       
      movf  TABLAT,W
      movwf Tx1d2   ;NNlo
      tblrd*+         ;high event byte
      movf  TABLAT,W
      movwf Tx1d3
      tblrd*+
      movf  TABLAT,W
      movwf Tx1d4
      
      
      nop
no_inc    return

;***************************************************************
;     read an event back by index no.

read    movff Rx0d3,Index
      decf  Index,F     ;index sent as base 1
      call  get_event
      movlw 0xF2
      movwf Tx1d0     ;OPC for answer
      movff Rx0d3,Tx1d7   ;index
      movff Tx1d4,Tx1d6   ;shuffle
      movff Tx1d3,Tx1d5
      movff Tx1d2,Tx1d4
      movff Tx1d1,Tx1d3
      movff NN_templ,Tx1d2
      movff NN_temph,Tx1d1
      movlw 8
      movwf Dlc
      call  sendTXa
      return


;****************************************************************

; write acknowledge CAN frame for FLASH write timing

wrack movlw 0x59    ;set up WRACK frame
  movwf Tx1d0
  movff NN_temph,Tx1d1
  movff NN_templ,Tx1d2
  movlw 3
  movwf Dlc
  call  sendTXa
  return

;************************************************************************

  ORG 0x002000      ;start of event lookup table in FLASH
              ;64 lines of 8 bytes each. 4 bytes for each 'switch'


        
  
  ORG 0xF00000      ;EEPROM data. Defaults
  
CANid1  de  B'01111111',0 ;CAN id default and module status
NodeID  de  0,0     ;Node ID
NVstart de  0,0     ;Node variable  (mode)
Node_st de  0,0

  ORG 0xF000FE
    de  0,0     ;For boot   
    end
