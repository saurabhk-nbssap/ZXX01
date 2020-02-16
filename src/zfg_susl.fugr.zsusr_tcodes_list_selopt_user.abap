FUNCTION ZSUSR_TCODES_LIST_SELOPT_USER.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(USER) LIKE  USR04-BNAME
*"     VALUE(SELTYPE) LIKE  USSEL1-SELTYPE
*"--------------------------------------------------------------------
*  01.09.2005 C5035001 ( Note 873638 )                         4.6C+
* Relation to reference user is fixed
*&--------------------------------------------------------------------*
  DATA:
    BEGIN OF profiles OCCURS 0,
        profile LIKE ust04-profile,
    END OF profiles,
    reference_user TYPE usrefus-refuser,
    ustype         TYPE usr02-ustyp.

  SELECT profile FROM ust04 INTO TABLE profiles
         WHERE bname = user.
* If the user a reference one?
  SELECT SINGLE USTYP FROM USR02 INTO USTYPE
     WHERE BNAME = user.
  IF SY-SUBRC EQ 0 AND USTYPE NE 'L'.
*   Profile(s) of reference user
    SELECT SINGLE refuser FROM usrefus INTO reference_user
        WHERE bname = user.
    IF sy-subrc EQ 0 AND NOT reference_user IS INITIAL.
      SELECT profile FROM ust04 APPENDING TABLE profiles
         WHERE bname = reference_user.
    ENDIF.
  ENDIF.
  SORT profiles BY profile.
  DELETE ADJACENT DUPLICATES FROM profiles COMPARING profile.
  CLEAR: auts, auts[].
  LOOP AT profiles.
    CLEAR: authlist, authlist[], proflist, proflist[].
    CALL FUNCTION 'PROFIL_AUFLOESEN'
         EXPORTING
              AKTPAS            = AKTIVATED
              profile           = profiles-profile
         TABLES
              AUTHLIST          = AUTHLIST
              PROFLIST          = PROFLIST
         EXCEPTIONS
              PROFILE_NOT_FOUND = 1
              OTHERS            = 2.
    LOOP AT authlist.
      auts-object  = authlist-object.
      auts-auth    = authlist-auth.
      APPEND auts.
    ENDLOOP.    "at AUTHLIST
  ENDLOOP.    "at profiles
  SORT auts BY object auth.
  DELETE ADJACENT DUPLICATES FROM auts COMPARING object auth.
  CALL FUNCTION 'SUSR_GET_TCODES_WITH_AUTH_LIST'
     EXPORTING
          SELTYPE = SELTYPE
          SELTYPETEXT = 'US'
          NAME = USER
          AUTHS   = AUTS
     EXCEPTIONS
          OTHERS  = 1.
ENDFUNCTION.
