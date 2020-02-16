*&---------------------------------------------------------------------*
*&  Include           Z6XX003R_TABMAIN_TOP
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*   INCLUDE ZPROG_SD_TABMAIN_TOP                                       *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*     TABLES                                                           *
*----------------------------------------------------------------------*
TABLES: dd03l.
TYPE-POOLS: slis.

*----------------------------------------------------------------------*
*     SELECTION SCREEN                                                 *
*----------------------------------------------------------------------*
PARAMETERS: tabname LIKE databrowse-tablename MEMORY ID tab.

PARAMETERS: feld0001(40).
PARAMETERS: feld0002(40).
PARAMETERS: feld0003(40).
PARAMETERS: feld0004(40).
PARAMETERS: feld0005(40).
PARAMETERS: feld0006(40).
PARAMETERS: feld0007(40).
PARAMETERS: feld0008(40).
PARAMETERS: feld0009(40).
PARAMETERS: feld0010(40).
PARAMETERS: feld0011(40).
PARAMETERS: feld0012(40).
PARAMETERS: feld0013(40).
PARAMETERS: feld0014(40).

*----------------------------------------------------------------------*
*     GLOBLE DATA                                                      *
*----------------------------------------------------------------------*
**-- FOR DYNAMIC TABLE
DATA: it_fcat TYPE slis_t_fieldcat_alv,
      is_fcat LIKE LINE OF it_fcat.
DATA: it_fieldcat TYPE lvc_t_fcat,
      is_fieldcat LIKE LINE OF it_fieldcat.
DATA: new_table TYPE REF TO data.
DATA: new_line  TYPE REF TO data.
FIELD-SYMBOLS: <l_table> TYPE ANY TABLE,
               <l_line>  TYPE ANY,
               <l_field> TYPE ANY.

**-- FOR DYNAMIC SCREEN
DATA: buffer LIKE char8000 OCCURS   0 WITH HEADER LINE.

CONSTANTS: on  VALUE '1'.
CONSTANTS: off VALUE '0'.

DATA: feldname LIKE screen-name.
DATA:  labname LIKE screen-name.
FIELD-SYMBOLS <label>.
FIELD-SYMBOLS <wert>.

DATA:       wtab(72) OCCURS 100 WITH HEADER LINE,
            and(3).

**-- GENERAL DATA
DATA : ok_code  LIKE  sy-ucomm .
DATA : field(10) .
DATA : not_found .
FIELD-SYMBOLS : <field> .

*----------------------------------------------------------------------*
*     GLOBLE DATA                                                      *
*----------------------------------------------------------------------*
DATA :   lbl001(20) ,
         lbl002(20) ,
         lbl003(20) ,
         lbl004(20) ,
         lbl005(20) ,
         lbl006(20) ,
         lbl007(20) ,
         lbl008(20) ,
         lbl009(20) ,
         lbl010(20) ,
         lbl011(20) ,
         lbl012(20) ,
         lbl013(20) ,
         lbl014(20) ,
         lbl015(20) ,
         lbl016(20) ,
         lbl017(20) ,
         lbl018(20) ,
         lbl019(20) ,
         lbl020(20) ,
         lbl021(20) ,
         lbl022(20) ,
         lbl023(20) ,
         lbl024(20) ,
         lbl025(20) ,
         lbl026(20) ,
         lbl027(20) ,
         lbl028(20) ,
         lbl029(20) ,
         lbl030(20) ,
         lbl031(20) ,
         lbl032(20) ,
         lbl033(20) ,
         lbl034(20) ,
         lbl035(20) ,
         lbl036(20) ,
         lbl037(20) ,
         lbl038(20) ,
         lbl039(20) ,
         lbl040(20) ,
         lbl041(20) ,
         lbl042(20) ,
         lbl043(20) ,
         lbl044(20) ,
         lbl045(20) ,
         lbl046(20) ,
         lbl047(20) ,
         lbl048(20) ,
         lbl049(20) ,
         lbl050(20) ,
         lbl051(20) ,
         lbl052(20) ,
         lbl053(20) ,
         lbl054(20) ,
         lbl055(20) ,
         lbl056(20) ,
         lbl057(20) ,
         lbl058(20) ,
         lbl059(20) ,
         lbl060(20) ,
         lbl061(20) ,
         lbl062(20) ,
         lbl063(20) ,
         lbl064(20) ,
         lbl065(20) ,
         lbl066(20) ,
         lbl067(20) ,
         lbl068(20) ,
         lbl069(20) ,
         lbl070(20) ,
         lbl071(20) ,
         lbl072(20) ,
         lbl073(20) ,
         lbl074(20) ,
         lbl075(20) ,
         lbl076(20) ,
         lbl077(20) ,
         lbl078(20) ,
         lbl079(20) ,
         lbl080(20) ,
         lbl081(20) ,
         lbl082(20) ,
         lbl083(20) ,
         lbl084(20) ,
         lbl085(20) ,
         lbl086(20) ,
         lbl087(20) ,
         lbl088(20) ,
         lbl089(20) ,
         lbl090(20) ,
         lbl091(20) ,
         lbl092(20) ,
         lbl093(20) ,
         lbl094(20) ,
         lbl095(20) ,
         lbl096(20) ,
         lbl097(20) ,
         lbl098(20) ,
         lbl099(20) ,
         lbl100(20) ,
         val001(50) ,
         val002(50) ,
         val003(50) ,
         val004(50) ,
         val005(50) ,
         val006(50) ,
         val007(50) ,
         val008(50) ,
         val009(50) ,
         val010(50) ,
         val011(50) ,
         val012(50) ,
         val013(50) ,
         val014(50) ,
         val015(50) ,
         val016(50) ,
         val017(50) ,
         val018(50) ,
         val019(50) ,
         val020(50) ,
         val021(50) ,
         val022(50) ,
         val023(50) ,
         val024(50) ,
         val025(50) ,
         val026(50) ,
         val027(50) ,
         val028(50) ,
         val029(50) ,
         val030(50) ,
         val031(50) ,
         val032(50) ,
         val033(50) ,
         val034(50) ,
         val035(50) ,
         val036(50) ,
         val037(50) ,
         val038(50) ,
         val039(50) ,
         val040(50) ,
         val041(50) ,
         val042(50) ,
         val043(50) ,
         val044(50) ,
         val045(50) ,
         val046(50) ,
         val047(50) ,
         val048(50) ,
         val049(50) ,
         val050(50) ,
         val051(50) ,
         val052(50) ,
         val053(50) ,
         val054(50) ,
         val055(50) ,
         val056(50) ,
         val057(50) ,
         val058(50) ,
         val059(50) ,
         val060(50) ,
         val061(50) ,
         val062(50) ,
         val063(50) ,
         val064(50) ,
         val065(50) ,
         val066(50) ,
         val067(50) ,
         val068(50) ,
         val069(50) ,
         val070(50) ,
         val071(50) ,
         val072(50) ,
         val073(50) ,
         val074(50) ,
         val075(50) ,
         val076(50) ,
         val077(50) ,
         val078(50) ,
         val079(50) ,
         val080(50) ,
         val081(50) ,
         val082(50) ,
         val083(50) ,
         val084(50) ,
         val085(50) ,
         val086(50) ,
         val087(50) ,
         val088(50) ,
         val089(50) ,
         val090(50) ,
         val091(50) ,
         val092(50) ,
         val093(50) ,
         val094(50) ,
         val095(50) ,
         val096(50) ,
         val097(50) ,
         val098(50) ,
         val099(50) ,
         val100(50) ,
         val001_t type d ,
         val002_t type d ,
         val003_t type d ,
         val004_t type d ,
         val005_t type d ,
         val006_t type d ,
         val007_t type d ,
         val008_t type d ,
         val009_t type d ,
         val010_t type d ,
         val011_t type d ,
         val012_t type d ,
         val013_t type d ,
         val014_t type d ,
         val015_t type d ,
         val016_t type d ,
         val017_t type d ,
         val018_t type d ,
         val019_t type d ,
         val020_t type d ,
         val021_t type d ,
         val022_t type d ,
         val023_t type d ,
         val024_t type d ,
         val025_t type d ,
         val026_t type d ,
         val027_t type d ,
         val028_t type d ,
         val029_t type d ,
         val030_t type d ,
         val031_t type d ,
         val032_t type d ,
         val033_t type d ,
         val034_t type d ,
         val035_t type d ,
         val036_t type d ,
         val037_t type d ,
         val038_t type d ,
         val039_t type d ,
         val040_t type d ,
         val041_t type d ,
         val042_t type d ,
         val043_t type d ,
         val044_t type d ,
         val045_t type d ,
         val046_t type d ,
         val047_t type d ,
         val048_t type d ,
         val049_t type d ,
         val050_t type t .

DATA : BEGIN OF itab OCCURS 0 ,
         field(20),
         keyflag ,
         type(4) ,
         leng TYPE i ,
         deci TYPE i ,
       END   OF itab .

DATA : itadir  LIKE  tadir  .
