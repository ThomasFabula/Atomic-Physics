      SUBROUTINE PARENT(IS,IL,IV,NZ,CFP)                                        
C     COEFFICIENTS OF FRACTIONAL PARENTAGE                                      
      REAL*8 CFP(10,16,16),Z1,Z2                                                
      DIMENSION IS(16,11),IL(16,11),IV(16,11),NZ(11),NCFP(5,16),                
     1ICFP(5,16,16)                                                             
      DO 100 N=1,5                                                              
      KK=NZ(N+1)                                                                
      LL=NZ(N)                                                                  
      DO 110 K=1,KK                                                             
      READ(11,120) NCFP(N,K)                                                    
      READ(11,130) (ICFP(N,K,L),L=1,LL)                                         
  120 FORMAT(I5)                                                                
  130 FORMAT(16I5)                                                              
  110 CONTINUE                                                                  
  100 CONTINUE                                                                  
      DO 200 N=1,5                                                              
      KK=NZ(N+1)                                                                
      LL=NZ(N)                                                                  
      DO 210 K=1,KK                                                             
      Z2=NCFP(N,K)                                                              
      DO 220 L=1,LL                                                             
      Z1=ICFP(N,K,L)                                                            
      IF(Z1.LT.0) GO TO 230                                                     
      CFP(N,K,L)=DSQRT(Z1/Z2)                                                   
      GO TO 220                                                                 
  230 CFP(N,K,L)=(-1.D0)*DSQRT(DABS(Z1/Z2))                                     
  220 CONTINUE                                                                  
  210 CONTINUE                                                                  
  200 CONTINUE                                                                  
      DO 300 N=6,10                                                             
      KK=NZ(N+1)                                                                
      LL=NZ(N)                                                                  
      DO 310 K=1,KK                                                             
       DO 320 L=1,LL                                                            
      N1=(IS(K,N+1)+IS(L,N)-3)/2+IL(K,N+1)+IL(L,N)-2                            
      Z1=IS(L,N)*(2.D0*IL(L,N)+1)*(11-N)                                        
      Z2=IS(K,N+1)*(2.D0*IL(K,N+1)+1)*N                                         
      CFP(N,K,L)=((-1)**N1)*DSQRT(Z1/Z2)*CFP(11-N,L,K)                          
      IF(N.NE.6) GO TO 320                                                      
      M5=(IV(L,N)-1)/2                                                          
      CFP(N,K,L)=((-1)**M5)*CFP(N,K,L)                                          
  320 CONTINUE                                                                  
  310 CONTINUE                                                                  
  300 CONTINUE                                                                  
      RETURN                                                                    
      END                                                                       
      SUBROUTINE ATDATA(IS,IL,IV,CFP,KONF,NZ,V11,V12,V13,V14,U2,U3,U4)          
      REAL*8 CFP(10,16,16),V11(16,16),V12(16,16),V13(16,16),V14(16,16),         
     1U2(16,16),U3(16,16),Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10,Y11,A,B,C,D,E,         
     2F,RES,U4(16,16)                                                           
      DIMENSION IS(16,11),IL(16,11),IV(16,11),NZ(11)                            
      N1=KONF+1                                                                 
      N=NZ(N1)                                                                  
      DO 700 K=1,N                                                              
      DO 710 L=1,N                                                              
      Y1=IS(K,N1)*IS(L,N1)*(2*IL(K,N1)+1)*(2*IL(L,N1)+1)                        
      Y8=(2*IL(K,N1)+1)*(2*IL(L,N1)+1)                                          
      Y2=DSQRT(3.D0*Y1/2.D0)                                                    
      Y9=DSQRT(Y8)                                                              
      N3=NZ(KONF)                                                               
      V11(K,L)=0.                                                               
      V12(K,L)=0.                                                               
      V13(K,L)=0.                                                               
      V14(K,L)=0.                                                               
      U2(K,L)=0.                                                                
      U3(K,L)=0.                                                                
      U4(K,L)=0.                                                                
      DO 720 J=1,N3                                                             
      N2=(IS(K,N1)+IS(J,KONF)-1)/2+IL(K,N1)+IL(J,KONF)+1                        
      N5=IL(K,N1)+IL(J,KONF)                                                    
      Y3=(-1)**(N2+1)                                                           
      Y4=(-1)**(N2+2)                                                           
      Y10=(-1)**N5                                                              
      Y11=(-1)**(N5+1)                                                          
      A=IS(K,N1)-1                                                              
      A=A/2.D0                                                                  
      B=1.D0                                                                    
      C=IS(L,N1)-1                                                              
      C=C/2.D0                                                                  
      D=0.5D0                                                                   
      E=IS(J,KONF)-1                                                            
      E=E/2.D0                                                                  
      F=0.5D0                                                                   
      CALL SECHSJ(A,B,C,D,E,F,RES,IER)                                          
      IF(IER.NE.0) GO TO 800                                                    
       Y6=RES                                                                   
      A=IL(K,N1)                                                                
      B=1.D0                                                                    
      C=IL(L,N1)                                                                
      D=2.D0                                                                    
      E=IL(J,KONF)                                                              
       F=2.D0                                                                   
      CALL SECHSJ(A,B,C,D,E,F,RES,IER)                                          
      IF(IER.NE.0) GO TO 800                                                    
      Y7=RES                                                                    
      V11(K,L)=V11(K,L)+Y3*Y6*Y7*CFP(KONF,K,J)*CFP(KONF,L,J)                    
      B=2.D0                                                                    
      CALL SECHSJ(A,B,C,D,E,F,RES,IER)                                          
      IF(IER.NE.0) GO TO 800                                                    
      Y7=RES                                                                    
      V12(K,L)=V12(K,L)+Y4*Y6*Y7*CFP(KONF,K,J)*CFP(KONF,L,J)                    
      IF(IS(K,N1).NE.IS(L,N1)) GO TO 850                                        
      U2(K,L)=U2(K,L)+Y10*Y7*CFP(KONF,K,J)*CFP(KONF,L,J)                        
      GO TO 860                                                                 
  850 U2(K,L)=0                                                                 
  860 B=3.D0                                                                    
      CALL SECHSJ(A,B,C,D,E,F,RES,IER)                                          
      IF(IER.NE.0) GO TO 800                                                    
      Y7=RES                                                                    
      V13(K,L)=V13(K,L)+Y3*Y6*Y7*CFP(KONF,K,J)*CFP(KONF,L,J)                    
  750 B=4.D0                                                                    
      CALL SECHSJ(A,B,C,D,E,F,RES,IER)                                          
      IF(IER.NE.0) GO TO 800                                                    
      Y5=RES                                                                    
      V14(K,L)=V14(K,L)+Y4*Y6*Y5*CFP(KONF,K,J)*CFP(KONF,L,J)                    
      IF(IS(K,N1).NE.IS(L,N1)) GO TO 740                                        
      U3(K,L)=U3(K,L)+Y11*Y7*CFP(KONF,K,J)*CFP(KONF,L,J)                        
      U4(K,L)=U4(K,L)+Y10*Y5*CFP(KONF,K,J)*CFP(KONF,L,J)                        
      GO TO 720                                                                 
  740 U3(K,L)=0                                                                 
      U4(K,L)=0                                                                 
  720 CONTINUE                                                                  
      V11(K,L)=Y2*V11(K,L)                                                      
      V12(K,L)=Y2*V12(K,L)                                                      
      V13(K,L)=Y2*V13(K,L)                                                      
      V14(K,L)=Y2*V14(K,L)                                                      
      U2(K,L)=Y9*U2(K,L)                                                        
      U3(K,L)=Y9*U3(K,L)                                                        
      U4(K,L)=Y9*U4(K,L)                                                        
  710 CONTINUE                                                                  
  700 CONTINUE                                                                  
      GO TO 810                                                                 
  800 WRITE(6,820) IER,A,B,C,D,E,F                                              
  820 FORMAT(1H0,'FEHLER',I2,'BEIM 6-J-SYMBOL',6D12.3)                          
  810 RETURN                                                                    
      END                                                                       
                                                                                                               