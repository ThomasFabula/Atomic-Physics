      SUBROUTINE AFAK (BJ,CL,CLA,SI,BLI,SENI,S1I,BL1I,S2I,BL2I,SJ,BLJ,SE        
     1NJ,S1J,BL1J,S2J,BL2J,V,A,NCORE1,NCORE2)                                   
      IMPLICIT REAL*8 (A-H,O-Z)                                                 
      INTEGER SENI,SENJ                                                         
      DIMENSION A(1)                                                            
      DO 2 I=1,9                                                                
    2 A(I)=0.                                                                   
      D=((2.*BJ+1.)/(BJ*(BJ+1.)))**0.5                                          
      IF(IABS(NCORE1-NCORE2)-1) 100,250,500                                     
C                                                                               
  100 CALL NEUNJ (SI,SJ,1.,BLI,BLJ,2.,BJ,BJ,1.,RES1,IER)                        
      IF(IER.NE.0) GO TO 300                                                    
      CALL SECHSJ (SI,SJ,1.,S1J,S1I,S2I,RES2,IER)                               
      IF(IER.NE.0) GO TO 400                                                    
      CALL SECHSJ (BLI,BLJ,2.,BL1J,BL1I,CLA,RES3,IER)                           
      IF(IER.NE.0) GO TO 400                                                    
      W2=((CL*(CL+1.)*(2.*CL+1.))/((2.*CL-1.)*(2.*CL+3.)))**0.5                 
      Z2IJ=(DSQRT(30.D0)*RES1*((2.*SI+1.)*(2.*SJ+1.)*(2.*BLI+1.)*(2.*BLJ        
     1+1.))**0.5)                                                               
      IEX1=S1I+BL1I+S2I+CLA+1.1+SJ+BLJ                                          
      A(3)=Z2IJ*RES2*RES3*W2*D*V*((-1.)**(IEX1))                                
      IF(SENI.NE.SENJ) GO TO 500                                                
      IF(S1I.NE.S1J) GO TO 500                                                  
      IF(BL1I.NE.BL1J) GO TO 500                                                
      CALL SECHSJ (SI,SJ,1.,S2I,S2I,S1I,RES4,IER)                               
      IF(IER.NE.0) GO TO 400                                                    
      CALL SECHSJ(BLI,BLJ,2.,CLA,CLA,BL1I,RES5,IER)                             
      IF(IER.NE.0) GO TO 400                                                    
      W3=((3.*CLA*(CLA+1.)*(2.*CLA+1.))/(2.*(2.*CLA-1.)*(2.*CLA+3.)))**0        
     1.5                                                                        
      IEX2=S1I+BL1I+S2I+CLA+1.1+SI+BLI                                          
      A(4)=Z2IJ*RES4*RES5*W3*D*((-1.)**(IEX2))                                  
      IF(SI.NE.SJ) GO TO 200                                                    
      CALL SECHSJ(BJ,BJ,1.,BLJ,BLI,SI,RES6,IER)                                 
      IF(IER.NE.0) GO TO 400                                                    
      CALL SECHSJ(BLI,BLJ,1.,BL1I,BL1I,CLA,RES7,IER)                            
      IF(IER.NE.0) GO TO 400                                                    
      CALL SECHSJ(BLI,BLJ,1.,CLA,CLA,BL1I,RES8,IER)                             
      IF(IER.NE.0) GO TO 400                                                    
      Z1IJ=RES6*((2.*BLI+1.)*(2.*BLJ+1.))**0.5                                  
      IX3=SI+BLJ+BJ+BL1I+CLA+BLJ+0.1                                            
      A(1)=Z1IJ*RES7*D*((-1.)**IX3)*(BL1I*(BL1I+1.)*(2.*BL1I+1.))**0.5          
      W1=(CLA*(CLA+1.)*(2.*CLA+1.))**0.5                                        
      IX4=SI+BLJ+BJ+BL1I+CLA+BLI+0.1                                            
      A(2)=Z1IJ*RES8*D*W1*((-1.)**IX4)                                          
  200 IF(BLI.NE.BLJ) GO TO 500                                                  
      CALL SECHSJ(BJ,BJ,1.,SJ,SI,BLI,RES9,IER)                                  
      IF(IER.NE.0) GO TO 400                                                    
      CALL SECHSJ(SI,SJ,1.,S1I,S1I,S2I,RES10,IER)                               
      IF(IER.NE.0) GO TO 400                                                    
      CALL SECHSJ(SI,SJ,1.,S2I,S2I,S1I,RES11,IER)                               
      IF(IER.NE.0) GO TO 400                                                    
      Z3IJ=RES9*((2.*SI+1.)*(2.*SJ+1.))**0.5                                    
      I5=SI+BLI+BJ+S1I+S2I+SJ+0.1                                               
      A(5)=Z3IJ*D*RES10*((-1.)**I5)*(S1I*(S1I+1.)*(2.*S1I+1.))**0.5             
      I6=SI+BLI+BJ+S1I+S2I+SI+0.1                                               
      A(6)=Z3IJ*D*RES11*((-1.)**I6)*(S2I*(S2I+1.)*(2.*S2I+1.))**0.5             
      RETURN                                                                    
C                                                                               
  250 CALL NEUNJ (SI,SJ,1.,BLI,BLJ,2.,BJ,BJ,1.,RES1,IER)                        
      IF(IER.NE.0) GO TO 300                                                    
      IEX6=SI+S1J+S2J+0.1                                                       
      W6=((2.*SI+1.)*(2.*BLI+1.))**0.5                                          
      KENZ=0                                                                    
      CALL RMECFP(NCORE1,S1I,BLI,SENI,S1J,BLJ,SENJ,CFP,SUM,KENZ)                
      FAK1=(-1)**IEX6*D*W6*RES1*3.*CFP                                          
      IF(S2I.EQ.0.) GO TO 270                                                   
      CALL SECHSJ(SI,SJ,1.,S2I,S2I,S1I,RES11,IER)                               
      IF(IER.NE.0) GO TO 400                                                    
      FAK2=DSQRT(2.D0*S1I+1.)                                                   
      A(7)=FAK1*FAK2*RES11                                                      
      IF(BLI.NE.BLJ) RETURN                                                     
      CALL SECHSJ(BJ,BJ,1.,SJ,SI,BLI,RES9,IER)                                  
      IF(IER.NE.0) GO TO 400                                                    
      KENZ=1                                                                    
      CALL RMECFP(NCORE1,S1I,BLI,SENI,S1J,BLJ,SENJ,CFP,SUM,KENZ)                
      IEX7=SJ+2.*S1I+BJ+BLI+1.1                                                 
      FAK3=(-1)**IEX7*D*FAK2*DSQRT(3.D0*(2.*SI+1.)/1.D1)                        
      A(9)=FAK3*RES9*RES11*SUM/(2.*BLI+1.)                                      
      RETURN                                                                    
  270 CALL SECHSJ(SI,SJ,1.,S2J,S2J,S1J,RES12,IER)                               
      IF(IER.NE.0) GO TO 400                                                    
      A(8)=FAK1*DSQRT(2.D0*SJ+1.)*RES12                                         
      RETURN                                                                    
C                                                                               
  400 WRITE (6,401) IER                                                         
  401 FORMAT(1H0,'FEHLER',I4,'  SECHSJ')                                        
      GO TO 500                                                                 
  300 WRITE (6,301) IER                                                         
  301 FORMAT (1H0,'FEHLER',I4,'  NEUNJ')                                        
  500 RETURN                                                                    
      END                                                                       
      SUBROUTINE BFAK(BJ,CL,CLA,SI,BLI,SENI,S1I,BL1I,S2I,BL2I,SJ,BLJ,SEN        
     1J,S1J,BL1J,S2J,BL2J,U2,V11,V13,B,NCORE1,NCORE2)                           
      IMPLICIT REAL*8 (A-H,O-Z)                                                 
      INTEGER SENI,SENJ                                                         
      DIMENSION B(1)                                                            
      DO 2 I=1,9                                                                
    2 B(I)=0.                                                                   
      D= ((4.*BJ*(2.*BJ-1.)*(2.*BJ+1.))/((BJ+1.)*(2.*BJ+3.)))**0.5              
      IF(IABS(NCORE1-NCORE2)-1) 100,200,900                                     
C                                                                               
  100 E=D*((2.*SI+1.)*(2.*SJ+1.)*(2.*BLI+1.)*(2.*BLJ+1.))**0.5                  
      CALL NEUNJ(SI,SJ,1.,BLI,BLJ,3.,BJ,BJ,2.,RES21,IER)                        
      IF(IER.NE.0) GO TO 700                                                    
      CALL SECHSJ(SI,SJ,1.,S1J,S1I,S2I,RES22,IER)                               
      IF(IER.NE.0) GO TO 800                                                    
      CALL SECHSJ(BLI,BLJ,3.,BL1J,BL1I,CLA,RES23,IER)                           
      IF(IER.NE.0) GO TO 800                                                    
      IEY1=S1I+BL1I+S2I+CLA+SJ+BLJ+0.1                                          
      B(3)=E*RES21*RES22*RES23*((-1.)**IEY1)*V13                                
      CALL NEUNJ(SI,SJ,1.,BLI,BLJ,1.,BJ,BJ,2.,RES24,IER)                        
      IF(IER.NE.0) GO TO 700                                                    
      CALL SECHSJ(SI,SJ,1.,S1J,S1I,S2I,RES25,IER)                               
      IF(IER.NE.0) GO TO 800                                                    
      CALL SECHSJ(BLI,BLJ,1.,BL1J,BL1I,CLA,RES26,IER)                           
      B(5)=E*RES24*RES25*RES26*V11*((-1.)**IEY1)                                
      IF(S1I.NE.S1J) GO TO 900                                                  
      IF(SI.NE.SJ) GO TO 901                                                    
      CALL SECHSJ(BLI,BLJ,2.,BJ,BJ,SI,RES27,IER)                                
      IF(IER.NE.0) GO TO 800                                                    
      CALL SECHSJ(BLI,BLJ,2.,BL1J,BL1I,CLA,RES28,IER)                           
      IF(IER.NE.0) GO TO 800                                                    
      W4=((CL*(CL+1.)*(2.*CL+1.))/((2.*CL-1.)*(2.*CL+3.)))**0.5                 
      IEY2=S1I+BL1I+S2I+CLA+BLJ+BJ+SI+S1I+S2I+SI+SJ+BLJ+0.1                     
      B(1)=(E*RES27*RES28*W4/(2.*SI+1.))*((-1.)**IEY2)*U2                       
      IF(BL1I.NE.BL1J) GO TO 900                                                
      IF(SENI.NE.SENJ) GO TO 900                                                
      CALL SECHSJ(BLI,BLJ,2.,CLA,CLA,BL1I,RES29,IER)                            
      IF(IER.NE.0) GO TO 800                                                    
      W5=((CLA*(CLA+1.)*(2.*CLA+1.))/((2.*CLA-1.)*(2.*CLA+3.)))**0.5            
      IEY3=S1I+BL1I+S2I+CLA+BLJ+BJ+SI+S1I+S2I+SI+SI+BLI+0.1                     
      B(2)=((E*W5*RES27*RES29)/(2.*SI+1.))*((-1.)**IEY3)*U2                     
  901 IF(BL1I.NE.BL1J) GO TO 900                                                
      IF(SENI.NE.SENJ) GO TO 900                                                
      CALL SECHSJ(SI,SJ,1.,S2I,S2I,S1I,RES30,IER)                               
      IF(IER.NE.0) GO TO 800                                                    
      CALL SECHSJ(BLI,BLJ,3.,CLA,CLA,BL1I,RES31,IER)                            
      IF(IER.NE.0) GO TO 800                                                    
      IEY4=S1I+BL1I+S2I+CLA+SI+BLI+0.1                                          
      B(4)=E*RES21*RES30*RES31*DSQRT(1.5D0)*((-1.)**IEY4)*V13                   
      CALL SECHSJ(BLI,BLJ,1.,CLA,CLA,BL1I,RES32,IER)                            
      B(6)=E*RES32*RES24*RES30*DSQRT(1.5D0)*((-1.)**IEY4)*V11                   
      RETURN                                                                    
C                                                                               
  200 IF(SI.NE.SJ) RETURN                                                       
      IEX=BJ+BLJ+S1I-S2I+1.1                                                    
      CALL SECHSJ(BLI,BLJ,2.,BJ,BJ,SI,RES27,IER)                                
      KENZ=0                                                                    
      CALL RMECFP(NCORE1,S1I,BLI,SENI,S1J,BLJ,SENJ,CFP,SUM,KENZ)                
      FAK=((2.*BLI+1.)*(2.*S1I+1.)/(10.*SI+5.))**0.5                            
      K=7                                                                       
      IF(S2I.EQ.0.) K=8                                                         
      B(K)=(-1)**IEX*D*FAK*RES27*CFP                                            
      RETURN                                                                    
  800 WRITE (6,801) IER                                                         
  700 WRITE(6,701) IER                                                          
  801 FORMAT(1H0,'FEHLER',I4,'  SECHSJ')                                        
  701 FORMAT(1H0,'FEHLER',I4,'  NEUNJ')                                         
  900 RETURN                                                                    
      END                                                                       
      SUBROUTINE CFAK(BJ,CL,CLA,SI,BLI,SENI,S1I,BL1I,S2I,BL2I,SJ,BLJ,SEN        
     1J,S1J,BL1J,S2J,BL2J,U3,V14,V12,C,NCORE1,NCORE2)                           
      IMPLICIT REAL*8 (A-H,O-Z)                                                 
      INTEGER SENI,SENJ                                                         
      DIMENSION C(1)                                                            
      DO 2 I=1,9                                                                
    2 C(I)=0.                                                                   
      IF(NCORE1.NE.NCORE2) GO TO 600                                            
      IF(S2I.GE.1.D0) GO TO 600                                                 
      LTST=BL2I+BL2J+CLA+0.1                                                    
      IF(LTST.NE.0) GO TO 600                                                   
      IF((BL1I.NE.BLI).OR.(BL1J.NE.BLJ)) GO TO 600                              
      W6=((CL*(CL+1.)*(2.*CL+1.))/((2.*CL-1.)*(2.*CL+3.)))**0.5                 
      F=((BJ*(BJ-1.)*(2.*BJ-1.)*(2.*BJ+1.))/((2.*BJ+3.)*(BJ+1.)*(BJ+2.))        
     1)**0.5                                                                    
      CALL NEUNJ(SI,SJ,1.,BLI,BLJ,4.,BJ,BJ,3.,RES41,IER)                        
      IF(IER.NE.0) GO TO 650                                                    
      CALL NEUNJ(SI,SJ,1.,BLI,BLJ,2.,BJ,BJ,3.,RES42,IER)                        
      IF(IER.NE.0) GO TO 650                                                    
      CALL SECHSJ(SI,SJ,1.,S1J,S1I,S2I,RES43,IER)                               
      IF(IER.NE.0) GO TO 640                                                    
      F1=((2.*SI+1.)*(2.*SJ+1.))**0.5                                           
      IZ2=SJ+S1I+S2I-0.9                                                        
      G=W6*F*F1*RES43*((-1.)**IZ2)                                              
      C3=9.D0*(((7.D0*(CL+2.)*(CL-1.))/((2.*CL+5.)*(2.*CL-3.)))**0.5)           
      C5=(3.5D1/3.D0)**0.5                                                      
      C(3)=G*C3*RES41*V14                                                       
      C(5)=G*C5*RES42*V12                                                       
      IF(S1I.NE.S1J) GO TO 600                                                  
      IF(SI.NE.SJ) GO TO 600                                                    
      CALL SECHSJ(BLI,BLJ,3.,BJ,BJ,SI,RES44,IER)                                
      IF(IER.NE.0) GO TO 640                                                    
      IZ1=SJ+BLJ+BJ+1.1                                                         
      C1=((CL+2.)*(CL-1.))**0.5                                                 
      C(1)=W6*F*C1*RES44*((-1.)**IZ1)*U3                                        
      GO TO 600                                                                 
  640 WRITE(6,641) IER                                                          
  641 FORMAT(1H0,'FEHLER',I4,'  SECHSJ')                                        
      GO TO 600                                                                 
  650 WRITE(6,651) IER                                                          
  651 FORMAT(1H0,'FEHLER',I4,'  NEUNJ')                                         
  600 RETURN                                                                    
      END                                                                       
      
