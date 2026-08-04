/*****************************************************************************
* SASload_NIS_2023_Core.SAS
* This program will load the NIS_2023_Core ASCII File into SAS.
* Created on 12/10/2025.
*****************************************************************************/

Options Compress=Yes;

*** Create SAS informats for missing values ***;
PROC FORMAT;
  INVALUE N2PF
    '-9' = .
    '-8' = .A
    '-6' = .C
    '-5' = .N
    OTHER = (|2.|)
  ;
  INVALUE N3PF
    '-99' = .
    '-88' = .A
    '-66' = .C
    OTHER = (|3.|)
  ;
  INVALUE N4PF
    '-999' = .
    '-888' = .A
    '-666' = .C
    OTHER = (|4.|)
  ;
  INVALUE N4P1F
    '-9.9' = .
    '-8.8' = .A
    '-6.6' = .C
    OTHER = (|4.1|)
  ;
  INVALUE N5PF
    '-9999' = .
    '-8888' = .A
    '-6666' = .C
    OTHER = (|5.|)
  ;
  INVALUE N5P2F
    '-9.99' = .
    '-8.88' = .A
    '-6.66' = .C
    OTHER = (|5.2|)
  ;
  INVALUE N6PF
    '-99999' = .
    '-88888' = .A
    '-66666' = .C
    OTHER = (|6.|)
  ;
  INVALUE N6P2F
    '-99.99' = .
    '-88.88' = .A
    '-66.66' = .C
    OTHER = (|6.2|)
  ;
  INVALUE N7P2F
    '-999.99' = .
    '-888.88' = .A
    '-666.66' = .C
    OTHER = (|7.2|)
  ;
  INVALUE N8PF
    '-9999999' = .
    '-8888888' = .A
    '-6666666' = .C
    OTHER = (|8.|)
  ;
  INVALUE N8P2F
    '-9999.99' = .
    '-8888.88' = .A
    '-6666.66' = .C
    OTHER = (|8.2|)
  ;
  INVALUE N8P4F
    '-99.9999' = .
    '-88.8888' = .A
    '-66.6666' = .C
    OTHER = (|8.4|)
  ;
  INVALUE N10PF
    '-999999999' = .
    '-888888888' = .A
    '-666666666' = .C
    OTHER = (|10.|)
  ;
  INVALUE N10P4F
    '-9999.9999' = .
    '-8888.8888' = .A
    '-6666.6666' = .C
    OTHER = (|10.4|)
  ;
  INVALUE N10P5F
    '-999.99999' = .
    '-888.88888' = .A
    '-666.66666' = .C
    OTHER = (|10.5|)
  ;
  INVALUE DATE10F
    '-999999999' = .
    '-888888888' = .A
    '-666666666' = .C
    OTHER = (|MMDDYY10.|)
  ;
  INVALUE N11P7F
    '-99.9999999' = .
    '-88.8888888' = .A
    '-66.6666666' = .C
    OTHER = (|11.7|)
  ;
  INVALUE N12P2F
    '-99999999.99' = .
    '-88888888.88' = .A
    '-66666666.66' = .C
    OTHER = (|12.2|)
  ;
  INVALUE N12P5F
    '-99999.99999' = .
    '-88888.88888' = .A
    '-66666.66666' = .C
    OTHER = (|12.5|)
  ;
  INVALUE N13PF
    '-999999999999' = .
    '-888888888888' = .A
    '-666666666666' = .C
    OTHER = (|13.|)
  ;
  INVALUE N15P2F
    '-99999999999.99' = .
    '-88888888888.88' = .A
    '-66666666666.66' = .C
    OTHER = (|15.2|)
  ;
RUN;

*** Data Step to load the file ***;
DATA NIS_2023_Core;
INFILE 'NIS_2023_Core.ASC' LRECL = 643;

*** Define data element attributes ***;
ATTRIB
  HOSP_NIS                   LENGTH=4            FORMAT=5.
  LABEL="NIS hospital number"

  KEY_NIS                    LENGTH=5            FORMAT=8.
  LABEL="NIS record number"

  NIS_STRATUM                LENGTH=4            FORMAT=4.
  LABEL="NIS hospital stratum"

  AGE                        LENGTH=3
  LABEL="Age in years at admission"

  AGE_NEONATE                LENGTH=3
  LABEL="Neonatal age (first 28 days after birth) indicator"

  AMONTH                     LENGTH=3
  LABEL="Admission month"

  AWEEKEND                   LENGTH=3
  LABEL="Admission day is a weekend"

  DIED                       LENGTH=3
  LABEL="Died during hospitalization"

  DISCWT                     LENGTH=8
  LABEL="NIS discharge weight"

  DISPUNIFORM                LENGTH=3
  LABEL="Disposition of patient (uniform)"

  DQTR                       LENGTH=3
  LABEL="Discharge quarter"

  DRG                        LENGTH=3
  LABEL="DRG in effect on discharge date"

  DRGVER                     LENGTH=3
  LABEL="DRG grouper version used on discharge date"

  DRG_NoPOA                  LENGTH=3
  LABEL="DRG in use on discharge date, calculated without POA"

  ELECTIVE                   LENGTH=3
  LABEL="Elective versus non-elective admission"

  FEMALE                     LENGTH=3
  LABEL="Indicator of sex"

  HCUP_ED                    LENGTH=3
  LABEL="HCUP Emergency Department service indicator"

  I10_BIRTH                  LENGTH=3
  LABEL="ICD-10-CM Birth Indicator"

  I10_DELIVERY               LENGTH=3
  LABEL="ICD-10-CM Delivery Indicator"

  I10_DX1                    LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 1"

  I10_DX2                    LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 2"

  I10_DX3                    LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 3"

  I10_DX4                    LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 4"

  I10_DX5                    LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 5"

  I10_DX6                    LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 6"

  I10_DX7                    LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 7"

  I10_DX8                    LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 8"

  I10_DX9                    LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 9"

  I10_DX10                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 10"

  I10_DX11                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 11"

  I10_DX12                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 12"

  I10_DX13                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 13"

  I10_DX14                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 14"

  I10_DX15                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 15"

  I10_DX16                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 16"

  I10_DX17                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 17"

  I10_DX18                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 18"

  I10_DX19                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 19"

  I10_DX20                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 20"

  I10_DX21                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 21"

  I10_DX22                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 22"

  I10_DX23                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 23"

  I10_DX24                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 24"

  I10_DX25                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 25"

  I10_DX26                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 26"

  I10_DX27                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 27"

  I10_DX28                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 28"

  I10_DX29                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 29"

  I10_DX30                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 30"

  I10_DX31                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 31"

  I10_DX32                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 32"

  I10_DX33                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 33"

  I10_DX34                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 34"

  I10_DX35                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 35"

  I10_DX36                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 36"

  I10_DX37                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 37"

  I10_DX38                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 38"

  I10_DX39                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 39"

  I10_DX40                   LENGTH=$7
  LABEL="ICD-10-CM Diagnosis 40"

  I10_INJURY                 LENGTH=3
  LABEL="Injury ICD-10-CM diagnosis reported on record (1: First-listed injury; 2: Other than first-listed injury; 0: No injury)"

  I10_MULTINJURY             LENGTH=3
  LABEL="Multiple ICD-10-CM injuries reported on record"

  I10_NDX                    LENGTH=3
  LABEL="ICD-10-CM Number of diagnoses on this record"

  I10_NPR                    LENGTH=3
  LABEL="ICD-10-PCS Number of procedures on this record"

  I10_PR1                    LENGTH=$7
  LABEL="ICD-10-PCS Procedure 1"

  I10_PR2                    LENGTH=$7
  LABEL="ICD-10-PCS Procedure 2"

  I10_PR3                    LENGTH=$7
  LABEL="ICD-10-PCS Procedure 3"

  I10_PR4                    LENGTH=$7
  LABEL="ICD-10-PCS Procedure 4"

  I10_PR5                    LENGTH=$7
  LABEL="ICD-10-PCS Procedure 5"

  I10_PR6                    LENGTH=$7
  LABEL="ICD-10-PCS Procedure 6"

  I10_PR7                    LENGTH=$7
  LABEL="ICD-10-PCS Procedure 7"

  I10_PR8                    LENGTH=$7
  LABEL="ICD-10-PCS Procedure 8"

  I10_PR9                    LENGTH=$7
  LABEL="ICD-10-PCS Procedure 9"

  I10_PR10                   LENGTH=$7
  LABEL="ICD-10-PCS Procedure 10"

  I10_PR11                   LENGTH=$7
  LABEL="ICD-10-PCS Procedure 11"

  I10_PR12                   LENGTH=$7
  LABEL="ICD-10-PCS Procedure 12"

  I10_PR13                   LENGTH=$7
  LABEL="ICD-10-PCS Procedure 13"

  I10_PR14                   LENGTH=$7
  LABEL="ICD-10-PCS Procedure 14"

  I10_PR15                   LENGTH=$7
  LABEL="ICD-10-PCS Procedure 15"

  I10_PR16                   LENGTH=$7
  LABEL="ICD-10-PCS Procedure 16"

  I10_PR17                   LENGTH=$7
  LABEL="ICD-10-PCS Procedure 17"

  I10_PR18                   LENGTH=$7
  LABEL="ICD-10-PCS Procedure 18"

  I10_PR19                   LENGTH=$7
  LABEL="ICD-10-PCS Procedure 19"

  I10_PR20                   LENGTH=$7
  LABEL="ICD-10-PCS Procedure 20"

  I10_PR21                   LENGTH=$7
  LABEL="ICD-10-PCS Procedure 21"

  I10_PR22                   LENGTH=$7
  LABEL="ICD-10-PCS Procedure 22"

  I10_PR23                   LENGTH=$7
  LABEL="ICD-10-PCS Procedure 23"

  I10_PR24                   LENGTH=$7
  LABEL="ICD-10-PCS Procedure 24"

  I10_PR25                   LENGTH=$7
  LABEL="ICD-10-PCS Procedure 25"

  I10_SERVICELINE            LENGTH=3
  LABEL="ICD-10-CM/PCS Hospital Service Line"

  LOS                        LENGTH=4
  LABEL="Length of stay (cleaned)"

  MDC                        LENGTH=3
  LABEL="MDC in effect on discharge date"

  MDC_NoPOA                  LENGTH=3
  LABEL="MDC in use on discharge date, calculated without POA"

  PAY1                       LENGTH=3
  LABEL="Primary expected payer (uniform)"

  PCLASS_ORPROC              LENGTH=3
  LABEL="Indicates operating room (major diagnostic or therapeutic) procedure on the record"

  PRDAY1                     LENGTH=4
  LABEL="Number of days from admission to I10_PR1"

  PRDAY2                     LENGTH=4
  LABEL="Number of days from admission to I10_PR2"

  PRDAY3                     LENGTH=4
  LABEL="Number of days from admission to I10_PR3"

  PRDAY4                     LENGTH=4
  LABEL="Number of days from admission to I10_PR4"

  PRDAY5                     LENGTH=4
  LABEL="Number of days from admission to I10_PR5"

  PRDAY6                     LENGTH=4
  LABEL="Number of days from admission to I10_PR6"

  PRDAY7                     LENGTH=4
  LABEL="Number of days from admission to I10_PR7"

  PRDAY8                     LENGTH=4
  LABEL="Number of days from admission to I10_PR8"

  PRDAY9                     LENGTH=4
  LABEL="Number of days from admission to I10_PR9"

  PRDAY10                    LENGTH=4
  LABEL="Number of days from admission to I10_PR10"

  PRDAY11                    LENGTH=4
  LABEL="Number of days from admission to I10_PR11"

  PRDAY12                    LENGTH=4
  LABEL="Number of days from admission to I10_PR12"

  PRDAY13                    LENGTH=4
  LABEL="Number of days from admission to I10_PR13"

  PRDAY14                    LENGTH=4
  LABEL="Number of days from admission to I10_PR14"

  PRDAY15                    LENGTH=4
  LABEL="Number of days from admission to I10_PR15"

  PRDAY16                    LENGTH=4
  LABEL="Number of days from admission to I10_PR16"

  PRDAY17                    LENGTH=4
  LABEL="Number of days from admission to I10_PR17"

  PRDAY18                    LENGTH=4
  LABEL="Number of days from admission to I10_PR18"

  PRDAY19                    LENGTH=4
  LABEL="Number of days from admission to I10_PR19"

  PRDAY20                    LENGTH=4
  LABEL="Number of days from admission to I10_PR20"

  PRDAY21                    LENGTH=4
  LABEL="Number of days from admission to I10_PR21"

  PRDAY22                    LENGTH=4
  LABEL="Number of days from admission to I10_PR22"

  PRDAY23                    LENGTH=4
  LABEL="Number of days from admission to I10_PR23"

  PRDAY24                    LENGTH=4
  LABEL="Number of days from admission to I10_PR24"

  PRDAY25                    LENGTH=4
  LABEL="Number of days from admission to I10_PR25"

  TRAN_IN                    LENGTH=3
  LABEL="Transfer in indicator"

  TRAN_OUT                   LENGTH=3
  LABEL="Transfer out indicator"

  YEAR                       LENGTH=3
  LABEL="Calendar year"

  ZIPINC_QRTL                LENGTH=3
  LABEL="Median household income national quartile for patient ZIP Code"

  TOTCHG_2023                LENGTH=8
  LABEL="Total charges adjusted for national estimates"

  PL_NCHS2                   LENGTH=3
  LABEL="Patient Location: NCHS Urban-Rural Code (collapsed)"
  ;

*** Read data elements from the ASCII file ***;
INPUT
      @1      HOSP_NIS                 N5PF.
      @6      KEY_NIS                  N10PF.
      @16     NIS_STRATUM              N4PF.
      @20     AGE                      N3PF.
      @23     AGE_NEONATE              N2PF.
      @25     AMONTH                   N2PF.
      @27     AWEEKEND                 N2PF.
      @29     DIED                     N2PF.
      @31     DISCWT                   N11P7F.
      @42     DISPUNIFORM              N2PF.
      @44     DQTR                     N2PF.
      @46     DRG                      N3PF.
      @49     DRGVER                   N2PF.
      @51     DRG_NoPOA                N3PF.
      @54     ELECTIVE                 N2PF.
      @56     FEMALE                   N2PF.
      @58     HCUP_ED                  N3PF.
      @61     I10_BIRTH                N3PF.
      @64     I10_DELIVERY             N3PF.
      @67     I10_DX1                  $CHAR7.
      @74     I10_DX2                  $CHAR7.
      @81     I10_DX3                  $CHAR7.
      @88     I10_DX4                  $CHAR7.
      @95     I10_DX5                  $CHAR7.
      @102    I10_DX6                  $CHAR7.
      @109    I10_DX7                  $CHAR7.
      @116    I10_DX8                  $CHAR7.
      @123    I10_DX9                  $CHAR7.
      @130    I10_DX10                 $CHAR7.
      @137    I10_DX11                 $CHAR7.
      @144    I10_DX12                 $CHAR7.
      @151    I10_DX13                 $CHAR7.
      @158    I10_DX14                 $CHAR7.
      @165    I10_DX15                 $CHAR7.
      @172    I10_DX16                 $CHAR7.
      @179    I10_DX17                 $CHAR7.
      @186    I10_DX18                 $CHAR7.
      @193    I10_DX19                 $CHAR7.
      @200    I10_DX20                 $CHAR7.
      @207    I10_DX21                 $CHAR7.
      @214    I10_DX22                 $CHAR7.
      @221    I10_DX23                 $CHAR7.
      @228    I10_DX24                 $CHAR7.
      @235    I10_DX25                 $CHAR7.
      @242    I10_DX26                 $CHAR7.
      @249    I10_DX27                 $CHAR7.
      @256    I10_DX28                 $CHAR7.
      @263    I10_DX29                 $CHAR7.
      @270    I10_DX30                 $CHAR7.
      @277    I10_DX31                 $CHAR7.
      @284    I10_DX32                 $CHAR7.
      @291    I10_DX33                 $CHAR7.
      @298    I10_DX34                 $CHAR7.
      @305    I10_DX35                 $CHAR7.
      @312    I10_DX36                 $CHAR7.
      @319    I10_DX37                 $CHAR7.
      @326    I10_DX38                 $CHAR7.
      @333    I10_DX39                 $CHAR7.
      @340    I10_DX40                 $CHAR7.
      @347    I10_INJURY               N2PF.
      @349    I10_MULTINJURY           N2PF.
      @351    I10_NDX                  N2PF.
      @353    I10_NPR                  N2PF.
      @355    I10_PR1                  $CHAR7.
      @362    I10_PR2                  $CHAR7.
      @369    I10_PR3                  $CHAR7.
      @376    I10_PR4                  $CHAR7.
      @383    I10_PR5                  $CHAR7.
      @390    I10_PR6                  $CHAR7.
      @397    I10_PR7                  $CHAR7.
      @404    I10_PR8                  $CHAR7.
      @411    I10_PR9                  $CHAR7.
      @418    I10_PR10                 $CHAR7.
      @425    I10_PR11                 $CHAR7.
      @432    I10_PR12                 $CHAR7.
      @439    I10_PR13                 $CHAR7.
      @446    I10_PR14                 $CHAR7.
      @453    I10_PR15                 $CHAR7.
      @460    I10_PR16                 $CHAR7.
      @467    I10_PR17                 $CHAR7.
      @474    I10_PR18                 $CHAR7.
      @481    I10_PR19                 $CHAR7.
      @488    I10_PR20                 $CHAR7.
      @495    I10_PR21                 $CHAR7.
      @502    I10_PR22                 $CHAR7.
      @509    I10_PR23                 $CHAR7.
      @516    I10_PR24                 $CHAR7.
      @523    I10_PR25                 $CHAR7.
      @530    I10_SERVICELINE          N3PF.
      @533    LOS                      N5PF.
      @538    MDC                      N2PF.
      @540    MDC_NoPOA                N2PF.
      @542    PAY1                     N2PF.
      @544    PCLASS_ORPROC            N2PF.
      @546    PRDAY1                   N3PF.
      @549    PRDAY2                   N3PF.
      @552    PRDAY3                   N3PF.
      @555    PRDAY4                   N3PF.
      @558    PRDAY5                   N3PF.
      @561    PRDAY6                   N3PF.
      @564    PRDAY7                   N3PF.
      @567    PRDAY8                   N3PF.
      @570    PRDAY9                   N3PF.
      @573    PRDAY10                  N3PF.
      @576    PRDAY11                  N3PF.
      @579    PRDAY12                  N3PF.
      @582    PRDAY13                  N3PF.
      @585    PRDAY14                  N3PF.
      @588    PRDAY15                  N3PF.
      @591    PRDAY16                  N3PF.
      @594    PRDAY17                  N3PF.
      @597    PRDAY18                  N3PF.
      @600    PRDAY19                  N3PF.
      @603    PRDAY20                  N3PF.
      @606    PRDAY21                  N3PF.
      @609    PRDAY22                  N3PF.
      @612    PRDAY23                  N3PF.
      @615    PRDAY24                  N3PF.
      @618    PRDAY25                  N3PF.
      @621    TRAN_IN                  N2PF.
      @623    TRAN_OUT                 N2PF.
      @625    YEAR                     N4PF.
      @629    ZIPINC_QRTL              N2PF.
      @631    TOTCHG_2023              N10PF.
      @641    PL_NCHS2                 N3PF.
      ;
RUN;
