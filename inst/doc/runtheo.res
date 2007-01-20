$PROB  THEOPHYLLINE   POPULATION DATA
$INPUT      ID DOSE=AMT TIME DV WT
$DATA       theopp.tab
$SUBROUTINES  ADVAN2 TRANS2 

$PK
;THETA(1)=MEAN ABSORPTION RATE CONSTANT (1/HR)
;THETA(2)=MEAN ELIMINATION RATE CONSTANT (1/HR)
;THETA(3)=SLOPE OF CLEARANCE VS WEIGHT RELATIONSHIP (LITERS/HR/KG)
   CALLFL=1
   KA=THETA(1)*EXP(ETA(1))
   V=THETA(2)*EXP(ETA(2))
   K=THETA(3)*EXP(ETA(3))
   CL=K*V
   S2=V

$ERROR
CV=THETA(4)
TAL=THETA(5)
IPRED=F
W=CV*F+TAL
Y=F+W*EPS(1)
IRES=IPRED-DV
IWRES=IRES/W

$THETA  (.1,3,5) (0,0.5,) (.004,.1,2) (0,0.2,) (0,0.1,)
$OMEGA 0.2 
$OMEGA BLOCK(2) 0.2 0.05 0.2
$SIGMA 1 FIX

$EST NOABORT METHOD=COND INTERACTION MAXEVAL=2000  PRINT=5
$COV
1NONLINEAR MIXED EFFECTS MODEL PROGRAM (NONMEM)    DOUBLE PRECISION NONMEM    VERSION V LEVEL 1.1  
 DEVELOPED AND PROGRAMMED BY STUART BEAL AND LEWIS SHEINER
 
 PROBLEM NO.:         1
 THEOPHYLLINE   POPULATION DATA                                          
0DATA CHECKOUT RUN:              NO 
 DATA SET LOCATED ON UNIT NO.:    2
 THIS UNIT TO BE REWOUND:        NO 
 NO. OF DATA RECS IN DATA SET:  144
 NO. OF DATA ITEMS IN DATA SET:   7
 ID DATA ITEM IS DATA ITEM NO.:   1
 DEP VARIABLE IS DATA ITEM NO.:   4
 MDV DATA ITEM IS DATA ITEM NO.:  7
0INDICES PASSED TO SUBROUTINE PRED:
  6  3  2  0  0  0  0  0  0
  0  0
0LABELS FOR DATA ITEMS:
   ID    DOSE    TIME      DV      WT    EVID     MDV
0FORMAT FOR DATA:
 (5E6.0,2F2.0)                                                                   
 
 TOT. NO. OF OBS RECS:     132
 TOT. NO. OF INDIVIDUALS:   12
0LENGTH OF THETA:  5
0OMEGA HAS BLOCK FORM:
  1
  0  2
  0  2  2
0SIGMA HAS SIMPLE DIAGONAL FORM WITH DIMENSION:  1
0INITIAL ESTIMATE OF THETA:
 LOWER BOUND    INITIAL EST    UPPER BOUND
  0.1000E+00     0.3000E+01     0.5000E+01
  0.0000E+00     0.5000E+00     0.1000E+07
  0.4000E-02     0.1000E+00     0.2000E+01
  0.0000E+00     0.2000E+00     0.1000E+07
  0.0000E+00     0.1000E+00     0.1000E+07
0INITIAL ESTIMATE OF OMEGA:
 BLOCK SET NO.   BLOCK                                                                    FIXED
        1                                                                                   NO 
                  0.2000E+00
        2                                                                                   NO 
                  0.2000E+00
                  0.5000E-01   0.2000E+00
0INITIAL ESTIMATE OF SIGMA:
 0.1000E+01
0SIGMA CONSTRAINED TO BE THIS INITIAL ESTIMATE
0ESTIMATION STEP OMITTED:           NO 
 CONDITIONAL ESTIMATES USED:       YES 
 CENTERED ETA:                      NO 
 EPS-ETA INTERACTION:              YES 
 LAPLACIAN OBJ. FUNC.:              NO 
 NO. OF FUNCT. EVALS. ALLOWED:    2000
 NO. OF SIG. FIGURES REQUIRED:       3
 INTERMEDIATE PRINTOUT:            YES 
 ESTIMATE OUTPUT TO MSF:            NO 
 ABORT WITH PRED EXIT CODE 1:       NO 
0COVARIANCE STEP OMITTED:    NO 
 EIGENVLS. PRINTED:    NO 
 SPECIAL COMPUTATION:  NO 
 COMPRESSED FORMAT:    NO 
1DOUBLE PRECISION PREDPP VERSION IV LEVEL 1.1  
 
 ONE COMPARTMENT MODEL WITH FIRST-ORDER ABSORPTION (ADVAN2)
0MAXIMUM NO. OF BASIC PK PARAMETERS:   3
0BASIC PK PARAMETERS (AFTER TRANSLATION):
   ELIMINATION RATE (K) IS BASIC PK PARAMETER NO.:  1
   ABSORPTION RATE (KA) IS BASIC PK PARAMETER NO.:  3
 
 TRANSLATOR WILL CONVERT PARAMETERS CLEARANCE (CL) AND VOLUME (V) TO K (TRANS2)
0COMPARTMENT ATTRIBUTES 
 COMPT. NO.   FUNCTION   INITIAL    ON/OFF      DOSE      DEFAULT    DEFAULT
                         STATUS     ALLOWED    ALLOWED    FOR DOSE   FOR OBS.
    1         DEPOT        OFF        YES        YES        YES        NO 
    2         CENTRAL      ON         NO         YES        NO         YES
    3         OUTPUT       OFF        YES        NO         NO         NO 
1
 ADDITIONAL PK PARAMETERS - ASSIGNMENT OF ROWS IN GG
 COMPT. NO.                             INDICES
              SCALE      BIOAVAIL.   ZERO-ORDER  ZERO-ORDER  ABSORB
                         FRACTION    RATE        DURATION    LAG  
    1           *           *           *           *           *
    2           4           *           *           *           *
    3           *           -           -           -           -
             - PARAMETER IS NOT ALLOWED FOR THIS MODEL
             * PARAMETER IS NOT SUPPLIED BY PK SUBROUTINE;
               WILL DEFAULT TO ONE IF APPLICABLE
0DATA ITEM INDICES USED BY PRED ARE:
   EVENT ID DATA ITEM IS DATA ITEM NO.:      6
   TIME DATA ITEM IS DATA ITEM NO.:          3
   DOSE AMOUNT DATA ITEM IS DATA ITEM NO.:   2
 
0PK SUBROUTINE CALLED ONCE PER INDIVIDUAL RECORD.
 PK SUBROUTINE NOT CALLED AT NONEVENT (ADDITIONAL OR LAGGED) DOSE TIMES.
0ERROR SUBROUTINE CALLED WITH EVERY EVENT RECORD.
1
 MONITORING OF SEARCH:

0ITERATION NO.:    0     OBJECTIVE VALUE:  0.1994E+03     NO. OF FUNC. EVALS.: 7
 CUMULATIVE NO. OF FUNC. EVALS.:    7
 PARAMETER:  0.1000E+00  0.1000E+00  0.1000E+00  0.1000E+00  0.1000E+00  0.1000E+00  0.1000E+00  0.1000E+00  0.1000E+00
 GRADIENT:   0.8889E+03 -0.1307E+03  0.4716E+03  0.1397E+04 -0.1922E+04 -0.4649E+03  0.1974E+03  0.3188E+01  0.1252E+03
0ITERATION NO.:    5     OBJECTIVE VALUE:  0.1098E+03     NO. OF FUNC. EVALS.: 9
 CUMULATIVE NO. OF FUNC. EVALS.:   57
 PARAMETER:  0.5636E-01  0.1019E+00  0.9154E-01  0.7307E-01  0.1291E+00  0.1446E+00  0.1926E-01  0.9902E-01  0.2741E-01
 GRADIENT:  -0.5050E+03  0.4292E+04 -0.6128E+03 -0.6072E+03 -0.5258E+03 -0.4173E+02 -0.1242E+04  0.1923E+02  0.7034E+02
0ITERATION NO.:   10     OBJECTIVE VALUE:  0.9415E+02     NO. OF FUNC. EVALS.: 8
 CUMULATIVE NO. OF FUNC. EVALS.:   99
 PARAMETER:  0.5840E-01  0.9808E-01  0.9169E-01  0.7349E-01  0.1332E+00  0.1513E+00  0.2866E-01  0.1417E+00  0.2658E-01
 GRADIENT:  -0.2765E+03  0.1201E+04 -0.5368E+03 -0.5413E+01 -0.3212E+03 -0.5355E+01 -0.3246E+02  0.3167E+02  0.8888E+02
0ITERATION NO.:   15     OBJECTIVE VALUE:  0.8671E+02     NO. OF FUNC. EVALS.: 8
 CUMULATIVE NO. OF FUNC. EVALS.:  139
 PARAMETER:  0.6467E-01  0.9573E-01  0.9298E-01  0.6657E-01  0.1601E+00  0.1448E+00  0.2664E-01  0.1222E+00 -0.2066E-02
 GRADIENT:   0.1423E+02 -0.1170E+03 -0.4675E+01  0.1044E+02 -0.1287E+01 -0.7467E+01 -0.8648E+01  0.1893E+01 -0.3687E+01
0ITERATION NO.:   20     OBJECTIVE VALUE:  0.8666E+02     NO. OF FUNC. EVALS.:11
 CUMULATIVE NO. OF FUNC. EVALS.:  182
 PARAMETER:  0.6442E-01  0.9596E-01  0.9306E-01  0.6636E-01  0.1606E+00  0.1488E+00  0.2689E-01  0.1206E+00 -0.1158E-03
 GRADIENT:  -0.2255E+01 -0.2995E+02 -0.1604E+02 -0.6389E+01 -0.1346E+01 -0.1823E-01  0.3044E+00  0.8156E-01 -0.1892E+00
0ITERATION NO.:   21     OBJECTIVE VALUE:  0.8666E+02     NO. OF FUNC. EVALS.: 0
 CUMULATIVE NO. OF FUNC. EVALS.:  182
 PARAMETER:  0.6442E-01  0.9596E-01  0.9306E-01  0.6636E-01  0.1606E+00  0.1488E+00  0.2689E-01  0.1206E+00 -0.1158E-03
 GRADIENT:  -0.2255E+01 -0.2995E+02 -0.1604E+02 -0.6389E+01 -0.1346E+01 -0.1823E-01  0.3044E+00  0.8156E-01 -0.1892E+00
0MINIMIZATION SUCCESSFUL
 NO. OF FUNCTION EVALUATIONS USED:  182
 NO. OF SIG. DIGITS IN FINAL EST.:  3.1

 ETABAR IS THE ARITHMETIC MEAN OF THE ETA-ESTIMATES,
 AND THE P-VALUE IS GIVEN FOR THE NULL HYPOTHESIS THAT THE TRUE MEAN IS 0.

 ETABAR:  -0.73E-02  0.46E-03  0.51E-03

 P VAL.:   0.97E+00  0.99E+00  0.99E+00
1
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                           MINIMUM VALUE OF OBJECTIVE FUNCTION                  ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 





 **************************************************         86.664     **************************************************
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                                  FINAL PARAMETER ESTIMATE                      ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


            TH 1      TH 2      TH 3      TH 4      TH 5
 
         1.51E+00  4.60E-01  8.73E-02  8.81E-02  2.58E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3
 
 ETA1
+        4.43E-01
 
 ETA2
+        0.00E+00  1.45E-02
 
 ETA3
+        0.00E+00  1.62E-02  1.82E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1
 
 EPS1
+        1.00E+00
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                             STANDARD ERROR OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 


 THETA - VECTOR OF FIXED EFFECTS PARAMETERS   *********


            TH 1      TH 2      TH 3      TH 4      TH 5
 
         3.08E-01  1.98E-02  4.47E-03  3.29E-02  1.11E-01
 


 OMEGA - COV MATRIX FOR RANDOM EFFECTS - ETAS  ********


            ETA1      ETA2      ETA3
 
 ETA1
+        2.56E-01
 
 ETA2
+       .........  5.71E-03
 
 ETA3
+       .........  9.61E-03  1.81E-02
 


 SIGMA - COV MATRIX FOR RANDOM EFFECTS - EPSILONS  ****


            EPS1
 
 EPS1
+       .........
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                          COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11

 
 TH 1
+        9.48E-02
 
 TH 2
+       -3.87E-04  3.93E-04
 
 TH 3
+       -2.31E-04  4.17E-05  2.00E-05
 
 TH 4
+       -2.90E-04  2.73E-04  1.13E-04  1.08E-03
 
 TH 5
+       -3.04E-03 -1.25E-03 -3.96E-04 -3.12E-03  1.23E-02
 
 OM11
+        4.98E-02 -1.39E-03 -1.79E-05 -4.68E-04 -5.95E-03  6.58E-02
 
 OM12
+       ......... ......... ......... ......... ......... ......... .........
 
 OM13
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        1.21E-03  2.03E-05 -7.12E-06 -6.45E-05  7.85E-05  7.94E-04 ......... .........  3.26E-05
 
 OM23
+        5.61E-04 -6.72E-05 -3.58E-05 -2.58E-04  9.67E-04 -1.15E-04 ......... .........  2.65E-05  9.24E-05
 
 OM33
+       -1.95E-04 -1.60E-04 -6.61E-05 -4.60E-04  1.94E-03 -1.22E-03 ......... .........  1.88E-05  1.65E-04  3.28E-04
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                         CORRELATION MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11

 
 TH 1
+        1.00E+00
 
 TH 2
+       -6.33E-02  1.00E+00
 
 TH 3
+       -1.68E-01  4.69E-01  1.00E+00
 
 TH 4
+       -2.86E-02  4.18E-01  7.66E-01  1.00E+00
 
 TH 5
+       -8.88E-02 -5.68E-01 -7.96E-01 -8.54E-01  1.00E+00
 
 OM11
+        6.31E-01 -2.73E-01 -1.56E-02 -5.55E-02 -2.09E-01  1.00E+00
 
 OM12
+       ......... ......... ......... ......... ......... ......... .........
 
 OM13
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+        6.88E-01  1.80E-01 -2.79E-01 -3.43E-01  1.24E-01  5.43E-01 ......... .........  1.00E+00
 
 OM23
+        1.90E-01 -3.52E-01 -8.31E-01 -8.16E-01  9.05E-01 -4.68E-02 ......... .........  4.83E-01  1.00E+00
 
 OM33
+       -3.50E-02 -4.45E-01 -8.16E-01 -7.73E-01  9.66E-01 -2.63E-01 ......... .........  1.82E-01  9.48E-01  1.00E+00
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
1
 ************************************************************************************************************************
 ********************                                                                                ********************
 ********************                  INVERSE COVARIANCE MATRIX OF ESTIMATE                         ********************
 ********************                                                                                ********************
 ************************************************************************************************************************
 

            TH 1      TH 2      TH 3      TH 4      TH 5      OM11      OM12      OM13      OM22      OM23      OM33      SG11

 
 TH 1
+        4.44E+01
 
 TH 2
+        8.69E+02  5.85E+04
 
 TH 3
+        1.98E+03  4.50E+04  5.74E+05
 
 TH 4
+        4.34E+02  3.58E+04  3.95E+04  3.43E+04
 
 TH 5
+        3.44E+02  2.51E+04  5.61E+03  1.97E+04  1.49E+04
 
 OM11
+        2.86E+01  2.89E+03  2.37E+03  1.66E+03  1.10E+03  1.87E+02
 
 OM12
+       ......... ......... ......... ......... ......... ......... .........
 
 OM13
+       ......... ......... ......... ......... ......... ......... ......... .........
 
 OM22
+       -1.50E+04 -6.96E+05 -2.46E+06 -6.08E+05 -2.69E+05 -3.11E+04 ......... .........  1.98E+07
 
 OM23
+        2.36E+04  1.11E+06  4.41E+06  1.07E+06  4.47E+05  4.71E+04 ......... ......... -3.50E+07  6.26E+07
 
 OM33
+       -1.15E+04 -5.69E+05 -1.91E+06 -5.39E+05 -2.52E+05 -2.35E+04 ......... .........  1.62E+07 -2.90E+07  1.37E+07
 
 SG11
+       ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... ......... .........
 
