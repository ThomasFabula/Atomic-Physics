      SUBROUTINE PLTSUB(N,WF,WZ,FZ,FW,SY,SX,KF,NPLT,KPLT,TXT,NRM,UNT,YM,        
     1NK,F0,BZ,KBZ,SKT)                                                         
      REAL*8 WF(1),WZ(1),FZ(1),FW(1),YMIN,YMAX,XT(2),YT(2),T0,TXT(1),           
     1       TK(2),YT1,YM,ZER,UNT,F0(1),BZ(1),BEZ(25)                           
      REAL*4 X(2),YL(2)/2*0.88/,YG(2)/2*0.84/, B(50)                            
      EQUIVALENCE (BEZ(1),B(1))                                                 
      DATA XT/'FREQUENZ',' (MHZ)'/,YT/'SIGNALZA','EHLRATE'/, T0/' '/            
      DATA TK/'LORENTZK','GAUSSK'/,T1/'URVE'/,YT1/'GESAMTZA'/, B0/' '/          
      IF(KF.NE.3) GO TO 1                                                       
      CALL ENDPLT                                                               
      RETURN                                                                    
    1 ISYM=MAX0(1,KF)*2.-0.9                                                    
      KF1=IABS(KF-1)+1                                                          
      LEN=10.1-2.*KF1                                                           
      IF(NPLT.EQ.0) GO TO 30                                                    
      GO TO 31                                                                  
   30 CALL QDTYPE(IDEV,UMAX,VMAX,NCOLOR,NLWDTH)                                 
      IF(IDEV.EQ.11)  IDEV=0                                                    
      IF(IDEV.EQ.901) IDEV=5                                                    
      IF(IDEV.EQ.101) IDEV=6                                                    
      IF(IDEV.EQ.201) IDEV=7                                                    
   31 IF(NPLT.NE.2) ZER=UNT                                                     
      IF(NRM.EQ.0.OR.NRM.GE.3) GO TO 11                                         
      DO 10 I=1,N                                                               
   10 FW(I)=FW(I)-ZER                                                           
C                                                                               
   11 IF(NPLT.EQ.2) GO TO 4                                                     
      NPLT=2                                                                    
      IFEL=0                                                                    
      DX=0.                                                                     
      YMIN=WZ(1)                                                                
      YMAX=WZ(1)                                                                
      IF(NRM.EQ.0.OR.NRM.GE.3) GO TO 7                                          
      YMIN=YMIN-ZER                                                             
      YMAX=YMAX-ZER                                                             
    7 DO 2 I=1,N                                                                
      IF(NRM.EQ.1.OR.NRM.EQ.2) WZ(I)=WZ(I)-ZER                                  
      YMIN=DMIN1(YMIN,WZ(I))                                                    
      YMAX=DMAX1(YMAX,WZ(I),FW(I))                                              
    2 IF(FZ(I).NE.1.) IFEL=-1                                                   
      YMIN=DMIN1(YMIN,0.D0)                                                     
      YMN=YMIN*2.                                                               
      YMX=YMAX*1.25                                                             
      IF(YM.NE.0.) YMX=YM                                                       
      IF(KPLT.GT.3) YMX=YMX/KPLT                                                
      DF=(WF(N)-WF(1))/(N-1)                                                    
      XMN=WF(1)-DF                                                              
      XMX=WF(N)+DF                                                              
      IF(KPLT.EQ.2) IFEL=0                                                      
      IS=1-IFEL                                                                 
      SCL=AMIN1(0.80,60./N)                                                     
      SCL=AMAX1(SCL,0.1)                                                        
      CALL NEWPAG                                                               
      CALL SYSCAL(YMN,YMX,0)                                                    
      CALL SXSCAL(XMN,XMX,0)                                                    
      CALL SMSCAL(SCL)                                                          
      CALL SCSCAL(SCL)                                                          
      IF(SY.LE.0.) SY=18.*(8-IDEV)                                              
      IF(SX.LE.0.) SX=25.*(8-IDEV)                                              
      IF(SY.GT.30.*(8-IDEV).AND.SY.LT.SX) SY=30.*(8-IDEV)                       
      IF(SX.GT.30.*(8-IDEV).AND.SY.GE.SX) SX=30.*(8-IDEV)                       
      IF(SX.LE.30.*(8-IDEV)) GO TO 35                                           
      GO TO 36                                                                  
   35 CALL DVROT(1)                                                             
      USIZE=SY*1.4                                                              
      GO TO 37                                                                  
   36 USIZE=SX*1.4                                                              
   37 IPEN=IDEV-5                                                               
      IF(IDEV.NE.6.AND.(SX*SY).GE.500.) IPEN=3                                  
      VSIZE=45.5                                                                
      IF(IDEV.EQ.6) VSIZE=83.5                                                  
      IF(IDEV.EQ.0) IPEN =1                                                     
      CALL DPBORD(0)                                                            
      CALL SLWDTH(IPEN)                                                         
      CALL DPSIZE(USIZE,VSIZE)                                                  
      CALL DVSIZE(SX,SY)                                                        
      IF(IS.EQ.2) IS=5                                                          
      CALL SMTYPE(IS)                                                           
      IF(IFEL.EQ.-1) IFEL=-2                                                    
      CALL PDATA(WF,2,WZ,2,N,IFEL,FZ,2)                                         
      IF(NRM.EQ.0.OR.NRM.GE.3) GO TO 9                                          
      DO 8 I=1,N                                                                
    8 WZ(I)=WZ(I)+ZER                                                           
    9 IF(KPLT.GE.3) GO TO 4                                                     
      DO 3 J=1,8                                                                
      IF(TXT(J).EQ.T0) GO TO 5                                                  
    3 CONTINUE                                                                  
      J=9                                                                       
    5 L1=8.*(J-1)+0.1                                                           
      L1=MAX0(1,L1)                                                             
      L2=MAX0(L1,40)                                                            
      SCL=(1.0-L2/118.)*SQRT(SX/SY)*ABS(SKT)                                    
      CALL SMSCAL(SCL)                                                          
      CALL SCSCAL(SCL)                                                          
      XCO=0.12-L1/800.                                                          
      CALL PTEXT(TXT,L1,XCO,0.94)                                               
      IF(KF.NE.2.OR.SKT.LT.0.) GO TO 4                                          
      X(1)=XCO                                                                  
      X(2)=XCO+0.08                                                             
      DX=0.1                                                                    
      CALL SLTYPE(1)                                                            
      CALL PDATA(X,1,YL,1,-2,4)                                                 
      CALL SLTYPE(3)                                                            
      CALL PDATA(X,1,YG,1,-2,4)                                                 
C                                                                               
    4 CALL SLTYPE(ISYM)                                                         
      CALL PDATA(WF,2,FW,2,N,5)                                                 
      IF(KPLT.GE.3.OR.SKT.LT.0.) GO TO 6                                        
      HT=0.91-MAX0(KF,1)/30.                                                    
      CALL SMSCAL(0.5)                                                          
      CALL SCSCAL(0.5)                                                          
      CALL PTEXT(TK(KF1),LEN,XCO+DX,HT)                                         
      CALL PTEXT(T1,4,999.,HT)                                                  
    6 IF(KF.EQ.2) RETURN                                                        
C                                                                               
      CALL SMSCAL(0.8)                                                          
      CALL SCSCAL(0.8)                                                          
      IF(NRM.GE.3) YT(1)=YT1                                                    
      CALL PYTEXT(YT,15)                                                        
      CALL PXTEXT(XT,14)                                                        
      IF(KBZ.EQ.0) GO TO 22                                                     
      SCL=.45*SQRT(SX/SY)                                                       
      CALL SMSCAL(SCL)                                                          
      CALL SCSCAL(SCL)                                                          
      I=1                                                                       
      DO 21 J=1,NK                                                              
      BEZ(J)=BZ(J)                                                              
   20 I=I+1                                                                     
      IF(F0(J).GE.(WF(I)-WF(1))) GO TO 20                                       
      FE=(F0(J)+DF)/(XMX-XMN)-0.01                                              
      IF(KBZ.EQ.1.AND.B(2*J).NE.B0) FE=FE-0.03                                  
      AE=(DMAX1(FW(I-1),FW(I))-YMN)/(YMX-YMN)+KBZ/30.                           
      IF(KPLT.GE.3.AND.AE.GT.0.98) AE=0.99+KBZ/30.                              
      IF(KPLT.LT.3.AND.(AE.GT.0.9.OR.(FE.LT.(XCO+0.18).AND.AE.GT.0.84)))        
     1GO TO 21                                                                  
      IF(DX.EQ.0.1.AND.AE.GT.0.81.AND.FE.LT.(XCO+0.25)) GO TO 21                
      CALL PTEXT(B(2*J-1),4,FE,AE)                                              
      IF(KBZ.EQ.1.AND.B(2*J).NE.B0) CALL PTEXT(B(2*J),4,999.,AE)                
      IF(KBZ.EQ.2) CALL PTEXT(B(2*J),4,FE,AE-0.03)                              
   21 CONTINUE                                                                  
   22 NPLT=1                                                                    
      RETURN                                                                    
      END                                                                       
                                                 