      SUBROUTINE DMINV (A,N,D,L,M)                                      00000440
      DIMENSION A(1),L(1),M(1)                                          00000450
      DOUBLE PRECISION A,D,BIGA,HOLD                                    00000460
C                                                                       00000470
C        SEARCH FOR LARGEST ELEMENT                                     00000480
C                                                                       00000490
      D=1.0                                                             00000500
      NK=-N                                                             00000510
      DO 80 K=1,N                                                       00000520
      NK=NK+N                                                           00000530
      L(K)=K                                                            00000540
      M(K)=K                                                            00000550
      KK=NK+K                                                           00000560
      BIGA=A(KK)                                                        00000570
      DO 20 J=K,N                                                       00000580
      IZ=N*(J-1)                                                        00000590
      DO 20 I=K,N                                                       00000600
      IJ=IZ+I                                                           00000610
   10 IF (DABS(BIGA)-DABS(A(IJ)))  15,20,20                             00000620
   15 BIGA=A(IJ)                                                        00000630
      L(K)=I                                                            00000640
      M(K)=J                                                            00000650
   20 CONTINUE                                                          00000660
C                                                                       00000670
C        INTERCHANGE ROWS                                               00000680
C                                                                       00000690
      J=L(K)                                                            00000700
      IF(J-K) 35,35,25                                                  00000710
   25 KI=K-N                                                            00000720
      DO 30 I=1,N                                                       00000730
      KI=KI+N                                                           00000740
      HOLD=-A(KI)                                                       00000750
      JI=KI-K+J                                                         00000760
      A(KI)=A(JI)                                                       00000770
   30 A(JI) =HOLD                                                       00000780
C                                                                       00000790
C        INTERCHANGE COLUMNS                                            00000800
C                                                                       00000810
   35 I=M(K)                                                            00000820
      IF(I-K) 45,45,38                                                  00000830
   38 JP=N*(I-1)                                                        00000840
      DO 40 J=1,N                                                       00000850
      JK=NK+J                                                           00000860
      JI=JP+J                                                           00000870
      HOLD=-A(JK)                                                       00000880
      A(JK)=A(JI)                                                       00000890
   40 A(JI) =HOLD                                                       00000900
C                                                                       00000910
C        DIVIDE COLUMN BY MINUS PIVOT (VALUE OF PIVOT ELEMENT IS        00000920
C        CONTAINED IN BIGA)                                             00000930
C                                                                       00000940
   45 IF(BIGA) 48,46,48                                                 00000950
   46 D=0.0                                                             00000960
      RETURN                                                            00000970
   48 DO 55 I=1,N                                                       00000980
      IF(I-K) 50,55,50                                                  00000990
   50 IK=NK+I                                                           00001000
      A(IK)=A(IK)/(-BIGA)                                               00001010
   55 CONTINUE                                                          00001020
C                                                                       00001030
C        REDUCE MATRIX                                                  00001040
C                                                                       00001050
      DO 65 I=1,N                                                       00001060
      IK=NK+I                                                           00001070
      HOLD=A(IK)                                                        00001080
      IJ=I-N                                                            00001090
      DO 65 J=1,N                                                       00001100
      IJ=IJ+N                                                           00001110
      IF(I-K) 60,65,60                                                  00001120
   60 IF(J-K) 62,65,62                                                  00001130
   62 KJ=IJ-I+K                                                         00001140
      A(IJ)=HOLD*A(KJ)+A(IJ)                                            00001150
   65 CONTINUE                                                          00001160
C                                                                       00001170
C        DIVIDE ROW BY PIVOT                                            00001180
C                                                                       00001190
      KJ=K-N                                                            00001200
      DO 75 J=1,N                                                       00001210
      KJ=KJ+N                                                           00001220
      IF(J-K) 70,75,70                                                  00001230
   70 A(KJ)=A(KJ)/BIGA                                                  00001240
   75 CONTINUE                                                          00001250
C                                                                       00001260
C        PRODUCT OF PIVOTS                                              00001270
C                                                                       00001280
      D=D*BIGA                                                          00001290
C                                                                       00001300
C        REPLACE PIVOT BY RECIPROCAL                                    00001310
C                                                                       00001320
      A(KK)=1.0/BIGA                                                    00001330
   80 CONTINUE                                                          00001340
C                                                                       00001350
C        FINAL ROW AND COLUMN INTERCHANGE                               00001360
C                                                                       00001370
      K=N                                                               00001380
  100 K=(K-1)                                                           00001390
      IF(K) 150,150,105                                                 00001400
  105 I=L(K)                                                            00001410
      IF(I-K) 120,120,108                                               00001420
  108 JQ=N*(K-1)                                                        00001430
      JR=N*(I-1)                                                        00001440
      DO 110 J=1,N                                                      00001450
      JK=JQ+J                                                           00001460
      HOLD=A(JK)                                                        00001470
      JI=JR+J                                                           00001480
      A(JK)=-A(JI)                                                      00001490
  110 A(JI) =HOLD                                                       00001500
  120 J=M(K)                                                            00001510
      IF(J-K) 100,100,125                                               00001520
  125 KI=K-N                                                            00001530
      DO 130 I=1,N                                                      00001540
      KI=KI+N                                                           00001550
      HOLD=A(KI)                                                        00001560
      JI=KI-K+J                                                         00001570
      A(KI)=-A(JI)                                                      00001580
  130 A(JI) =HOLD                                                       00001590
      GO TO 100                                                         00001600
  150 RETURN                                                            00001610
      END                                                               00001620
      
