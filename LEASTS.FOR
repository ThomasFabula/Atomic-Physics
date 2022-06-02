      SUBROUTINE LEASTS(X,C,Y,N,R,CA,A,CHISQU,IER,Z,L,M)                        
C     LEAST SQUARES FIT                                                         
C     X   N*R-KOEFFIZIENTENMATRIX                                               
C     C   N-VARIANZVEKTOR                                                       
C     Y   N-MESSWERTVEKTOR                                                      
C     N   ZAHL DER MESSUNGEN                                                    
C     R   ZAHL DER PARAMETER                                                    
C     CA   R*R-KOVARIANZMATRIX                                                  
C     A   R-VEKTOR DER BEST-FIT-PARAMETER                                       
C     CHISQU   CHIQUADRAT                                                       
C     IER 1   N KLEINER R                                                       
C     IER 2   KOVARIANZMATRIX DER MESSWERTE HAT RANG KLEINER N                  
C     IER 3   RANG VON X KLEINER R                                              
C     IER 4   R = 0                                                             
C     IER 0   O.K.                                                              
C     Z   ARBEITSVEKTOR DER DIMENSION R*R                                       
C     L,M   ARBEITSVEKTOREN DER DIMENSION R                                     
C     NOTWENDIGE SUBROUTINEN   DMINV                                            
      DIMENSION X(1),C(1),Y(1),CA(1),A(1),Z(1),L(1),M(1)                        
      REAL*8 X,C,Y,CA,A,CHISQU,Z,D                                              
      INTEGER R,RR                                                              
C     TEST OF DIMENSIONS                                                        
      IF(R.GT.0) GO TO 15                                                       
      IER=4                                                                     
      RETURN                                                                    
   15 IF(N.GE.R) GO TO 1                                                        
      IER=1                                                                     
      RETURN                                                                    
C     TEST RANG KOVARIANZMATRIX                                                 
    1 DO 2 I=1,N                                                                
      IF(C(I).LE.0) GO TO 3                                                     
    2 CONTINUE                                                                  
      GO TO 4                                                                   
    3 IER=2                                                                     
      RETURN                                                                    
C     KOVARIANZMATRIX DER PARAMETER                                             
    4 DO 5 I=1,R                                                                
      DO 5 J=1,R                                                                
      I1=(J-1)*R+I                                                              
      Z(I1)=0.D0                                                                
      DO 6 K=1,N                                                                
      I2=(I-1)*N+K                                                              
      I3=(J-1)*N+K                                                              
      Z(I1)=Z(I1)+X(I2)*X(I3)/C(K)                                              
    6 CONTINUE                                                                  
    5 CONTINUE                                                                  
      CALL DMINV(Z,R,D,L,M)                                                     
      IF(D.NE.0) GO TO 7                                                        
      IER=3                                                                     
      RETURN                                                                    
    7 RR=R*R                                                                    
      DO 8 I=1,RR                                                               
      CA(I)=Z(I)                                                                
    8 CONTINUE                                                                  
C     PARAMETER                                                                 
      DO 9 J=1,R                                                                
      Z(J)=0.D0                                                                 
      DO 10 K=1,N                                                               
      I1=(J-1)*N+K                                                              
      Z(J)=Z(J)+Y(K)*X(I1)/C(K)                                                 
   10 CONTINUE                                                                  
    9 CONTINUE                                                                  
      DO 11 J=1,R                                                               
      A(J)=0.D0                                                                 
      DO 12 K=1,R                                                               
      I1=(K-1)*R+J                                                              
      A(J)=A(J)+Z(K)*CA(I1)                                                     
   12 CONTINUE                                                                  
   11 CONTINUE                                                                  
C     CHISQUARE                                                                 
      CHISQU=0.D0                                                               
      DO 13 K=1,N                                                               
      D=-Y(K)                                                                   
      DO 14 J=1,R                                                               
      I1=(J-1)*N+K                                                              
      D=D+A(J)*X(I1)                                                            
   14 CONTINUE                                                                  
      CHISQU=CHISQU+D*D/C(K)                                                    
   13 CONTINUE                                                                  
      IER=0                                                                     
      RETURN                                                                    
      END                                                                       
                                                           