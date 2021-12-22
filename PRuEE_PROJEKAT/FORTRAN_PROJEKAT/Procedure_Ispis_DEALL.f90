!******************************************************************************************************
!Ispis dobijenih rezultata

SUBROUTINE Ispis(jed_Ispisa)

    USE EES_Elementi, ONLY: ID_File
    USE Topologija
    USE Topologija_Helper
    USE Bazne_Velicine, ONLY: Pg_SUM
    USE Tokovi_Snaga, ONLY: br_Iteracija


    IMPLICIT NONE


    INTEGER, INTENT(IN) :: jed_Ispisa
    INTEGER :: h


    !TIP IZLAZA
    IF (jed_Ispisa .EQ. ID_File) THEN
        !ISPIS U IZLAZNU DATOTEKU
        WRITE (*, *) '  Ispis u izlaznu datoteku...'
        CALL SLEEP(1)

        OPEN (UNIT = jed_Ispisa, FILE = 'IZLAZ___REZULTATI.txt', ACTION = 'WRITE', STATUS = 'UNKNOWN')

    ELSE
        !ISPIS NA EKRAN
        WRITE (*, *) '  Ispis na ekran...'
        CALL SLEEP(1)

        OPEN (UNIT = jed_Ispisa, FILE = 'CON')      !Ekranu dodeljen logicki broj: jed_Ispisa

    END IF               
    

        !..................................................................................................
        !Ispis unete konfiguracije mreze

        WRITE ( jed_Ispisa, '(/, 1x, 40("=") )' )
        WRITE ( jed_Ispisa, '(8x, a)' ) "UNETA KONFIGURACIJA MREZE"
        WRITE ( jed_Ispisa, '(1x, 40("=") )' )
        WRITE ( jed_Ispisa, '(/, 1x, 40("-") )' )
        WRITE ( jed_Ispisa, '(1x, a, 2(5x, a) )') "Br. grane", "GORNJI Cvor", "DONJI Cvor"
        WRITE ( jed_Ispisa, '(1x, 40("-") )' )

        DO h = 1, broj_Grana
            WRITE (jed_Ispisa, 201) h, grane(h)%Get_Gornji_Cvor(), grane(h)%Get_Donji_Cvor()
        ENDDO    
        
        201 FORMAT (2x, i3, 13x, i3, 12x, i3) 


        !..................................................................................................
        !Ispis rezultata tokova snaga

        WRITE (jed_Ispisa, '(//, 1x, 66("=") )' )
        WRITE (jed_Ispisa, '(17x, a)') "REZULTATI PRORACUNA TOKOVA SNAGA"
        WRITE (jed_Ispisa, '(1x, 66("=") )' ) 


        !..................................................................................................
        !Ispis kompleksnog napona cvorova i njegovog modula

        WRITE (jed_Ispisa, '(/, 1x, 66("-") )' )
        WRITE (jed_Ispisa, '(1x, a, 8x, a, 7x, a)') "Br. cvora", "Kompleksni napon [V]", "Moduo napona [kV]"
        WRITE (jed_Ispisa, '(1x, 66("-") )' )

        DO h = 1, broj_Cvorova
            WRITE (jed_Ispisa, 202) h, cvorovi(h)%V_Cvora, ABS(cvorovi(h)%V_Cvora)/1000
        ENDDO

        202 FORMAT ( 3x, i3, 8x,'(' f11.4 ',' f11.4 ' )', 6x, f9.4 )


        !..................................................................................................
        !Ispis kompleksne struje grana i njenog modula

        WRITE (jed_Ispisa, '(/, 1x, 66("-") )' )
        WRITE (jed_Ispisa, '(1x, a, 8x, a, 8x, a)') "Br. grane", "Kompleksna struja [A]", "Moduo struje [A]"
        WRITE (jed_Ispisa, '(1x, 66("-") )' )
    
        DO h = 1, broj_Grana
            WRITE (jed_Ispisa, 203) h, grane(h)%J_Grane, ABS(grane(h)%J_Grane)
        ENDDO

        203 FORMAT ( 3x, i3, 8x, '(' f9.4 ',' f13.4 ' )', 6x, f9.4 )

    
        !..................................................................................................
        !Ispis kompleksne snage i gubitaka aktivne snage svake grane

        WRITE (jed_Ispisa, '(/, 1x, 66("-") )' )
        WRITE (jed_Ispisa, '(1x, a, 7x, a, 3x, a)') "Br. grane", "Kompleksna snaga [kVA]", "Gubici aktivne snage [kW]"
        WRITE (jed_Ispisa, '(1x, 66("-") )' )
    
        DO h = 1, broj_Grana
            WRITE (jed_Ispisa, 204) h, grane(h)%S_12_Grane/1000, grane(h)%Pg_Grane/1000
        ENDDO

        204 FORMAT ( 3x, i3, 8x, '(' f11.5 ',' f11.6 ' )', 9x, f6.3  )


        !..................................................................................................
        !Ispis ukupnih gubitaka aktivne snage mreze

        WRITE (jed_Ispisa, '(/, 1x, 66("-") )' )
        WRITE (jed_Ispisa, '(/, 1x, a, f8.3, 1x, a)') "UKUPNI GUBICI AKTIVNE SNAGE MREZE:", Pg_SUM/1000, "[kW]"


        !..................................................................................................
        !Ispis ukupnog broja izvrsenih iteracija 

        WRITE (jed_Ispisa, '(/, 1x, 66("=") )' )
        WRITE (jed_Ispisa, '(/, 1x, a, i3, /)') "Ukupan broj izvrsenih iteracija:", br_Iteracija 
        WRITE (jed_Ispisa, '(1x, 66("="), /)' )


    IF (jed_Ispisa .EQ. ID_File) THEN
        CLOSE(jed_Ispisa)
    END IF


END SUBROUTINE Ispis


!******************************************************************************************************
!Oslobadjanje dinamicke memorije

SUBROUTINE Dealokacija()

    USE EES_Ucitavanje
    USE Topologija_Helper
    USE Tokovi_Snaga


    IMPLICIT NONE


    INTEGER :: Dealloc_Error


    WRITE (*, *) '  Oslobadjanje dinamicke memorije...'
    WRITE (*, *) ' '

    CALL SLEEP(1)


    IF( ALLOCATED(vodovi) ) DEALLOCATE( vodovi, STAT = Dealloc_Error )
        IF ( Dealloc_Error .NE. 0 ) THEN
            STOP ("[ERROR] Problem sa dealokacijom memorije za vodove!")
    END IF


    IF( ALLOCATED(potrosaci) ) DEALLOCATE( potrosaci, STAT = Dealloc_Error )
        IF ( Dealloc_Error .NE. 0 ) THEN
            STOP ("[ERROR] Problem sa dealokacijom memorije za potrosace!")
    END IF


    IF( ALLOCATED(cvorovi) ) DEALLOCATE( cvorovi, STAT = Dealloc_Error )
        IF ( Dealloc_Error .NE. 0 ) THEN
            STOP ("[ERROR] Problem sa dealokacijom memorije za cvorove!")
    END IF


    IF( ALLOCATED(grane) ) DEALLOCATE( grane, STAT = Dealloc_Error )
        IF ( Dealloc_Error .NE. 0 ) THEN
            STOP ("[ERROR] Problem sa dealokacijom memorije za grane!")
    END IF


    IF( ALLOCATED(dP) ) DEALLOCATE( dP, STAT = Dealloc_Error )
        IF ( Dealloc_Error .NE. 0 ) THEN
            STOP ("[ERROR] Problem sa dealokacijom memorije za debalans aktivnih snaga u cvorovima!")
    END IF


    IF( ALLOCATED(dQ) ) DEALLOCATE( dQ, STAT = Dealloc_Error )
        IF ( Dealloc_Error .NE. 0 ) THEN
            STOP ("[ERROR] Problem sa dealokacijom memorije za debalans reaktivnih snaga u cvorovima!")
    END IF


END SUBROUTINE Dealokacija