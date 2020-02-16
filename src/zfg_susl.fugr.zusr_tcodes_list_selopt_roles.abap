FUNCTION ZUSR_TCODES_LIST_SELOPT_ROLES.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(ROLE) LIKE  AGR_DEFINE-AGR_NAME OPTIONAL
*"     VALUE(SELTYPE) LIKE  USSEL1-SELTYPE OPTIONAL
*"  TABLES
*"      ROLES_LIST STRUCTURE  AGR_DEFINE OPTIONAL
*"      E_I_TEMPTSTCT STRUCTURE  TSTCT OPTIONAL
*"----------------------------------------------------------------------
*  17.03.2006 R000731 ( Note 932726 )                              4.6b+
*  follow up correction to note 826908 (AGR_1016-PSTATE no longer used)
*-----------------------------------------------------------------------

  DATA roles_number TYPE I.
  DATA: BEGIN OF profiles_list OCCURS 0,
         prf_name LIKE agr_1016-profile,
        END OF profiles_list.
  DATA it_agr_1016 LIKE agr_1016 OCCURS 0 WITH HEADER LINE.
  DESCRIBE TABLE roles_list LINES roles_number.
  IF roles_number GT 1.
* Composite role itself has no profile(s)
   DELETE roles_list WHERE agr_name = role.
  ENDIF.
* Get all profiles
  SELECT * FROM agr_1016 INTO it_agr_1016
    FOR ALL ENTRIES IN roles_list
    WHERE agr_name = roles_list-agr_name
      AND generated = 'X' .                               "note 932726
    profiles_list-prf_name = it_agr_1016-profile.
    APPEND profiles_list.
  ENDSELECT.
* Create list of all authorizations
  LOOP AT profiles_list.
    CALL FUNCTION 'PROFIL_AUFLOESEN'
       EXPORTING
            AKTPAS            = 'A'
            PROFILE           = profiles_list-prf_name
       TABLES
            AUTHLIST          = AUTHLIST
            PROFLIST          = PROFLIST
       EXCEPTIONS
            PROFILE_NOT_FOUND = 1
            OTHERS            = 2.
    REFRESH AUTS.
    CLEAR AUTS.
    LOOP AT AUTHLIST.
      AUTS-OBJECT  = AUTHLIST-OBJECT.
      AUTS-AUTH    = AUTHLIST-AUTH.
      APPEND AUTS.
    ENDLOOP.
  ENDLOOP.
* Prepare executable transactions list and display it
  CALL FUNCTION 'ZUSR_GET_TCODES_WITH_AUTH_LIST'
       EXPORTING
            SELTYPE = SELTYPE
            SELTYPETEXT = 'RO'
            NAME    = ROLE
            AUTHS   = AUTS
       TABLES
            I_TEMPTSTCT = E_I_TEMPTSTCT
       EXCEPTIONS
            OTHERS  = 1.

ENDFUNCTION.
