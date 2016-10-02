        PROGRAM EUL205
        IMPLICIT NONE
        INTEGER    CUB,PYR
        REAL*8     PCUB,PPYR
        REAL*8     TOTP 
        DIMENSION  CUB(36,6)
        DIMENSION  PYR(36,9)
        DIMENSION  PCUB(6:36),PPYR(9:36)
        COMMON     /MEMO/ CUB, PYR
        
        INTEGER    GETCUB, GETPYR
        EXTERNAL   GETCUB, GETPYR
        
        INTEGER    I,II

        CALL ISET(CUB,36*6,-1)
        CALL ISET(PYR,36*9,-1)
        
        DO 10 I = 36, 9, -1
10      PPYR(I) = GETPYR(9,4,I,GETPYR)*1.0D0/(4**9)

        DO 11 I = 36, 6, -1
11      PCUB(I) = GETCUB(6,6,I,GETCUB)*1.0D0/(6**6)

        TOTP = 0.0D0

        DO 20 I  = 6, 36 
        DO 20 II = MAX(9,I+1),36
20      TOTP = TOTP + PCUB(I) * PPYR(II)
          
        WRITE(6,1805) TOTP

1805    FORMAT("ANSWER TO EULER 205 = ", F8.7)

        RETURN
        END
!***************************************************!
!***************************************************!
        INTEGER   FUNCTION GETCUB(ND,SZ,VAL,FUN)
        IMPLICIT NONE
        INTEGER   ND, SZ, VAL
        INTEGER   CUB(36,6), PYR(36,9)
        INTEGER   FUN, GETPYR
        INTEGER   I 
        EXTERNAL  FUN
        COMMON    /MEMO/CUB,PYR

        GETCUB = 0

        IF(VAL.LE.0) THEN
          GETCUB = 0 
          RETURN
        ELSEIF(ND.EQ.1) THEN
          IF(VAL.LT.1.OR.VAL.GT.6) THEN
          GETCUB = 0
          ELSE
          GETCUB = 1
          ENDIF
          RETURN
        ELSEIF(VAL.GE.1.AND.VAL.LE.36) THEN
          IF(CUB(VAL,ND).GE.0) THEN
          GETCUB = CUB(VAL,ND)
          RETURN 
          ENDIF
          DO 10 I = 1 , 6 
10        GETCUB = GETCUB + FUN(ND-1,6,VAL-I,FUN)         
        ELSE    
         STOP
        ENDIF
        
        IF((ND.GE.1.AND.ND.LE.6).AND.(VAL.LE.36.AND.VAL.GE.1))
     $  CUB(VAL,ND) = GETCUB 
          
        RETURN
        
        ENTRY GETPYR(ND,SZ,VAL,FUN)
        
        GETPYR = 0  
         
        IF(VAL.LE.0) THEN
          GETPYR = 0 
          RETURN 
        ELSEIF(ND.EQ.1) THEN
          IF(VAL.LT.1.OR.VAL.GT.4) THEN
          GETPYR = 0
          ELSE 
          GETPYR = 1
          ENDIF
          RETURN
        ELSEIF(VAL.GE.1.AND.VAL.LE.36) THEN
          IF(PYR(VAL,ND).GE.0) THEN
          GETPYR = PYR(VAL,ND)
          RETURN
          ENDIF
          DO 20 I = 1 , 4 
20        GETPYR = GETPYR + FUN(ND-1,4,VAL-I,FUN)         
        ELSE 
         STOP 
        ENDIF
        
        IF((ND.GE.1.AND.ND.LE.9).AND.(VAL.LE.36.AND.VAL.GE.1))
     $  PYR(VAL,ND) = GETPYR 
          
        RETURN
        END

        SUBROUTINE ISET(A,N,VAL)       
        IMPLICIT INTEGER  (A-Z)
        DIMENSION A(*)
        DO 10 I = 1 , N 
10        A(I)  = VAL
        RETURN
        END
