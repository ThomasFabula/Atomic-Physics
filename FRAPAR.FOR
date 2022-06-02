      SUBROUTINE FRAPAR                                                         
      IMPLICIT REAL*8(A-H,O-Z)                                                  
      INTEGER SEN1,SEN2                                                         
      REAL*8 CFP(10,16,16),V11(16,16),V12(16,16),V13(16,16),U2(16,16),          
     1Z1,BS1,BL1,BS2,BL2,V,U,W,X,T,Z,U3(16,16),V14(16,16),R,U4(16,16)           
      DIMENSION IS(16,11),IL(16,11),IV(16,11),NZ(11),Y(7),YY(16,11)             
      DATA NZ/1,1,5,8,3*16,8,5,1,1/                                             
      DATA IS/1,15*0,2,15*0,1,3,1,3,1,11*0,2,4,3*2,4,2*2,8*0,2*1,2*3,           
     12*1,3,5,1,2*3,2*1,2*3,1,2,6,2,4,3*2,4,2*2,4,2*2,4,2*2,2*1,2*3,            
     22*1,3,5,1,2*3,2*1,2*3,1,2,4,3*2,4,2*2,8*0,1,3,1,3,1,11*0,2,15*0,          
     31,15*0/                                                                   
      DATA IL/16*0,2,15*0,0,1,2,3,4,11*0,2*1,2*2,2*3,4,5,8*0,2*0,2*1,           
     14*2,3*3,3*4,5,6,2*0,2*1,4*2,3*3,3*4,5,6,2*0,2*1,4*2,3*3,3*4,5,6,          
     22*1,2*2,2*3,4,5,8*0,0,1,2,3,4,11*0,2,15*0,16*0/                           
      DATA IV/16*0,1,15*0,0,4*2,11*0,2*3,1,5*3,8*0,0,4,2,4,2,4*4,2,4,2,         
     14*4,2*5,2*3,1,3,2*5,3,5,2*3,2*5,3,5,0,4,2,4,2,4*4,2,4,2,4*4,2*3,          
     21,5*3,8*0,0,4*2,11*0,1,15*0,16*0/                                         
      DATA Y/'S','P','D','F','G','H','I'/                                       
      DO 600 N=1,11                                                             
      DO 600 K=1,16                                                             
      J=IL(K,N)+1                                                               
  600 YY(K,N)=Y(J)                                                              
      CALL PARENT(IS,IL,IV,NZ,CFP)                                              
      DO 400 N=1,10                                                             
      Z1=N                                                                      
      KK=NZ(N+1)                                                                
      LL=NZ(N)                                                                  
      DO 410 K=1,KK                                                             
      DO 420 L=1,LL                                                             
      CFP(N,K,L)=DSQRT(Z1)*CFP(N,K,L)                                           
  420 CONTINUE                                                                  
  410 CONTINUE                                                                  
  400 CONTINUE                                                                  
      KOLAST=20                                                                 
      RETURN                                                                    
C                                                                               
      ENTRY RMECFP(ND,SP1,BD1,IV1,SP2,BD2,IV2,FP,SUM,KENZ)                      
      FP=0.                                                                     
      SUM=0.                                                                    
      IF((ND.LT.1).OR.(ND.GT.10)) RETURN                                        
      KK=NZ(ND+1)                                                               
      LL=NZ(ND)                                                                 
      ISP1=2.*SP1+1.1                                                           
      IBD1=BD1+0.1                                                              
      DO 100 K=1,KK                                                             
      IF((ISP1.EQ.IS(K,ND+1)).AND.(IBD1.EQ.IL(K,ND+1)).AND.                     
     1(IV1.EQ.IV(K,ND+1))) GO TO 101                                            
  100 CONTINUE                                                                  
      NK=ND                                                                     
      GO TO 104                                                                 
  101 ISP2=2.*SP2+1.1                                                           
      IBD2=BD2+0.1                                                              
      DO 102 L=1,LL                                                             
      IF((ISP2.EQ.IS(L,ND)).AND.(IBD2.EQ.IL(L,ND)).AND.                         
     1(IV2.EQ.IV(L,ND))) GO TO 103                                              
  102 CONTINUE                                                                  
      NK=ND-1                                                                   
  104 WRITE(6,630) NK                                                           
      RETURN                                                                    
  103 FP=CFP(ND,K,L)                                                            
      IF(KENZ.EQ.0) RETURN                                                      
C                                                                               
      IF(ND.EQ.KOLAST) GO TO 20                                                 
      KOLAST=ND                                                                 
      CALL ATDATA(IS,IL,IV,CFP,ND,NZ,V11,V12,V13,V14,U2,U3,U4)                  
   20 DO 200 J=1,KK                                                             
      IF(IS(J,ND+1).NE.ISP1.OR.IABS(IL(J,ND+1)-IBD1).GT.2) GO TO 200            
      WL=DSQRT(2.D0*IL(J,ND+1)+1.)                                              
      SUM=SUM+WL*CFP(ND,J,L)*U2(J,K)                                            
  200 CONTINUE                                                                  
      RETURN                                                                    
C                                                                               
      ENTRY REDMAT(KONF,KTEST,BS1,BL1,SEN1,BS2,BL2,SEN2,V,U,W,X,T,Z,R,M)        
      IF((KONF.LT.1).OR.(KONF.GT.10)) GO TO 610                                 
      KS1=2.*BS1+1.1                                                            
      KS2=2.*BS2+1.1                                                            
      KL1=BL1+0.1                                                               
      KL2=BL2+0.1                                                               
      KK=NZ(KONF+1)                                                             
      DO 300 N=1,KK                                                             
      IF(IS(N,KONF+1).NE.KS1) GO TO 300                                         
      IF(IL(N,KONF+1).NE.KL1) GO TO 300                                         
      IF(IV(N,KONF+1).NE.SEN1) GO TO 300                                        
      N1=N                                                                      
      GO TO 301                                                                 
  300 CONTINUE                                                                  
      GO TO 605                                                                 
  301 DO 302 N=1,KK                                                             
      IF(IS(N,KONF+1).NE.KS2) GO TO 302                                         
      IF(IL(N,KONF+1).NE.KL2) GO TO 302                                         
      IF(IV(N,KONF+1).NE.SEN2) GO TO 302                                        
      N2=N                                                                      
      GO TO 303                                                                 
  302 CONTINUE                                                                  
      GO TO 605                                                                 
  303 IF(KONF.EQ.KOLAST) GO TO 1000                                             
      KOLAST=KONF                                                               
      NTEST=KTEST                                                               
      CALL ATDATA(IS,IL,IV,CFP,KONF,NZ,V11,V12,V13,V14,U2,U3,U4)                
 1000 V=V12(N1,N2)                                                              
      U=U2(N1,N2)                                                               
      T=U3(N1,N2)                                                               
      R=U4(N1,N2)                                                               
      W=V11(N1,N2)                                                              
      X=V13(N1,N2)                                                              
      Z=V14(N1,N2)                                                              
      IF((KTEST.EQ.0).OR.(NTEST.EQ.0.AND.M.EQ.0)) RETURN                        
      NTEST=0                                                                   
      IF(M.EQ.0) GO TO 901                                                      
      M=0                                                                       
      WRITE(6,540)                                                              
  540 FORMAT(1H1,'COEFFICIENTS OF FRACTIONAL PARENTAGE'/1H ,36(1H-))            
      DO 500 N=1,10                                                             
      KK=NZ(N+1)                                                                
      LL=NZ(N)                                                                  
      WRITE(6,520) ((IS(L,N),YY(L,N),IV(L,N)),L=1,LL)                           
  520 FORMAT(1H-,1X,16(I6,A1,I1))                                               
      DO 510 K=1,KK                                                             
      WRITE(6,530) IS(K,N+1),YY(K,N+1),IV(K,N+1),(CFP(N,K,L),L=1,LL)            
  530 FORMAT(1H ,I1,A1,I1,1X,16F8.4)                                            
  510 CONTINUE                                                                  
  500 CONTINUE                                                                  
  901 NN=NZ(KONF+1)                                                             
      WRITE(6,900) KONF                                                         
  900 FORMAT(1H1,'REDUZIERTE MATRIXELEMENTE  V11, V12, V13, V14, U2, U3,        
     1 U4  FUER DIE KONFIGURATION  D HOCH',I2/1H ,91(1H-))                      
      WRITE(6,520) ((IS(L,KONF+1),YY(L,KONF+1),IV(L,KONF+1)),L=1,NN)            
      DO 910 K=1,NN                                                             
      WRITE(6,920) IS(K,KONF+1),YY(K,KONF+1),IV(K,KONF+1),                      
     1(V11(K,L),L=1,NN)                                                         
      WRITE(6,930) (V12(K,L),L=1,NN)                                            
      WRITE(6,930) (V13(K,L),L=1,NN)                                            
      WRITE(6,930) (V14(K,L),L=1,NN)                                            
      WRITE(6,930) (U2(K,L),L=1,NN)                                             
      WRITE(6,930) (U3(K,L),L=1,NN)                                             
      WRITE(6,930) (U4(K,L),L=1,NN)                                             
  920 FORMAT(1H0,I1,A1,I1,1X,16F8.4)                                            
  930 FORMAT(1H ,4X,16F8.4)                                                     
  910 CONTINUE                                                                  
      WRITE(6,940)                                                              
  940 FORMAT(1H0)                                                               
      GO TO 650                                                                 
  605 WRITE(6,630) KONF                                                         
  630 FORMAT(1H0,'FALSCHER ZUSTAND IN KONFIGURATION  D HOCH',I3)                
      GO TO 640                                                                 
  610 WRITE(6,620) KONF                                                         
  620 FORMAT(1H0,'KONFIGURATION D HOCH',I3,'?')                                 
  640 U=0.                                                                      
      T=0.                                                                      
      R=0.                                                                      
      W=0.                                                                      
      V=0.                                                                      
      X=0.                                                                      
      Z=0.                                                                      
  650 RETURN                                                                    
      END                                                                       
       