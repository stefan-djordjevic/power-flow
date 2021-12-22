MODULE Tokovi_Snaga


USE Topologija
USE Topologija_Helper
USE Bazne_Velicine
USE Interoperabilnost


IMPLICIT NONE


!********************************************************************************************

INTERFACE

    FUNCTION COMPLEX_Inicijalizacija(in_vrednost) 

        COMPLEX, INTENT(IN), OPTIONAL :: in_vrednost
        COMPLEX :: COMPLEX_Inicijalizacija

    END FUNCTION COMPLEX_Inicijalizacija


END INTERFACE

!********************************************************************************************


REAL, PARAMETER :: Eps = 1e-5

INTEGER :: br_Iteracija

REAL, ALLOCATABLE   :: dP(:)           !Debalans aktivnih snaga u cvorovima
REAL, ALLOCATABLE   :: dQ(:)           !Debalans reaktivnih snaga u cvorovima

INTEGER  :: vr_nula = 0                !Vrednost za inicijalizaciju 

!================================================================================================
!================================================================================================

CONTAINS


    !********************************************************************************************
    !Procedura za proracun tokova snaga

    SUBROUTINE Proracun_Tokova_Snaga()


    CLASS(CVOR), POINTER   :: p_Cvor
    CLASS(GRANA), POINTER  :: p_Grana

    !............................................................................................
    !Promenljive vezane za proracun tokova snaga

    INTEGER  :: i, j
    REAL     :: maxDP, maxDQ!          !Maksimalni debalans aktivne i reaktivne snage u cvorovima, respektivno

    WRITE (*, *) '  Pokrenut proracun tokova snaga...'

    CALL SLEEP(1)

    
    br_Iteracija = Inicijalizacija(vr_nula)                            

    !............................................................................................
    !Alokacija memorije za promenljive

    IF ( .NOT.ALLOCATED(dP) ) ALLOCATE ( dP(broj_Cvorova), STAT = Alloc_Error )
        IF (Alloc_Error .NE. 0) THEN
            STOP ("[ERROR] Problem sa alokacijom memorije za debalans aktivnih snaga u cvorovima!")
        END IF

    IF ( .NOT.ALLOCATED(dQ) ) ALLOCATE ( dQ(broj_Cvorova), STAT = Alloc_Error )
        IF (Alloc_Error .NE. 0) THEN
            STOP ("[ERROR] Problem sa alokacijom memorije za debalans reaktivnih snaga u cvorovima!")
        END IF


    !............................................................................................
    !Inicijalizacija velicina vezanih za cvorove

    DO i=0, broj_Cvorova

        p_Cvor => cvorovi(i)
        
        !Poziv f-je "COMPLEX_Inicijalizacija" bez argumenta => f-ja vraca vrednost 0
        !Poziv f-je "COMPLEX_Inicijalizacija" sa argumentom => f-ja vraca vrednost datog argumenta
        
        p_Cvor%Y0_sum = COMPLEX_Inicijalizacija()               ![r.j.]
        p_Cvor%V_Cvora = COMPLEX_Inicijalizacija((1, 0))        ![r.j.], inicijalizacija izjednacavanjem sa naponom korena mreze

    ENDDO


    !............................................................................................
    !Pretvaranje velicina cvorova i grana iz apsolutnih u relativne jedinice

    CALL Pretvarac_APS_REL(APS_u_REL)


    !............................................................................................
    !Proracun sume otocnih admitansi u cvorovima 

    CALL Proracun_Y0_sum()


    !............................................................................................
    !ITERATIVNI POSTUPAK

    WRITE (*, *) '    Pokrenut iterativni postupak...'

    CALL SLEEP(1)

    DO WHILE ( br_Iteracija .LE. 100 )

        maxDP = Inicijalizacija(REAL(vr_nula))
        maxDQ = Inicijalizacija(REAL(vr_nula))
        
        br_Iteracija = br_Iteracija + 1

        
        IF ( br_Iteracija == 11 ) THEN
            WRITE(*,*) "[WARNING] Funkcija tokova snaga ne konvergira nakon desete iteracije."
        END IF

        IF ( br_Iteracija == 101 ) THEN
            STOP ("[ERROR] Funkcija tokova snaga ne konvergira nakon stote iteracije.")
        END IF


        ![1. KORAK] Proracun injektiranih struja za svaki cvor prema relaciji 3.4
        DO i = 0, broj_Cvorova
            p_Cvor => cvorovi(i)
            CALL Proracun_I_inj_Cvora_C ( p_Cvor%Get_S_cons_Cvor(), p_Cvor%Y0_sum, p_Cvor%V_Cvora, p_Cvor%I_inj_Cvora )
        ENDDO


        ![2. KORAK] Proracun struja za svaku granu prema relaciji 3.7
                    !Zamena unazad - pocevsi od grana u poslednjem lejeru
        DO i = broj_Grana, 1, -1
            grane(i)%J_Grane = cvorovi( grane(i)%Get_Donji_Cvor() )%I_inj_Cvora
            DO j=1, broj_Grana
                IF ( grane(i)%Get_Donji_Cvor() == grane(j)%Get_Gornji_Cvor() ) THEN
                    CALL Proracun_J_Grane_C ( grane(i)%J_Grane, grane(j)%J_Grane )   
                END IF
            ENDDO
        ENDDO


        ![3. KORAK] Proracun napona za svaki cvor prema relaciji 3.8
                    !Zamena unapred - pocevsi od cvora u prvom lejeru
        DO i = 1, broj_Grana
            p_Grana => grane(i)
            CALL Proracun_V_Cvora_C ( cvorovi( p_Grana%Get_Gornji_Cvor() )%V_Cvora,  p_Grana%Get_Z_Grana(),  &
                                      p_Grana%J_Grane,  cvorovi(p_Grana%Get_Donji_Cvor())%V_Cvora )
        ENDDO
        

        !Proracun injektirane snage za svaki cvor i odstupanja od snage potrosnje
        DO i = 1, broj_Cvorova

            p_Cvor => cvorovi(i)
            CALL Proracun_S_Cvora_C (p_Cvor%V_Cvora, p_Cvor%I_inj_Cvora, p_Cvor%Y0_sum, p_Cvor%V_Cvora, p_Cvor%S_Cvora)

            dP(i) = REAL ( p_Cvor%S_Cvora - p_Cvor%Get_S_cons_Cvor() )
            dQ(i) = IMAG ( p_Cvor%S_Cvora - p_Cvor%Get_S_cons_Cvor() )

        ENDDO

        
        !Maksimalan debalans aktivne i reaktivne snage u tekucoj iteraciji
        DO i = 1, broj_Cvorova

            IF ( ABS(dP(i)) .GT. maxDP ) THEN
                maxDP = ABS(dP(i)) 
            END IF

            dQ(i) = ABS(dQ(i))

        ENDDO

        maxDQ = MAXVAL(dQ)


        !Ispitivanje uslova KONVERGENCIJE na kraju tekuce iteracije
        IF ( (maxDP .LT. Eps) .and. (maxDQ .LT. Eps) ) THEN
            EXIT
        ELSE 
            CYCLE
        END IF


    ENDDO
    !KRAJ ITERATIVNOG POSTUPKA
    

    !............................................................................................
    !Proracun kompleksne snage S12 za svaku granu
    
    CALL Proracun_Snage_Grana()


    !............................................................................................
    !Proracun gubitaka aktivne snage za svaku granu i ukupnih gubitaka mreze

    CALL Proracun_Gubitaka_Snage()


    !............................................................................................
    !Pretvaranje velicina cvorova i grana iz relativnih u apsolutne jedinice

    CALL Pretvarac_APS_REL(3)



    END SUBROUTINE Proracun_Tokova_Snaga



    !********************************************************************************************
    !Proracun sume otocnih admitansi u cvorovima

    SUBROUTINE Proracun_Y0_sum()
    
        
        INTEGER :: i, j
        
        WRITE (*, *) '    Proracun sume otocnih admitansi u cvorovima...'  

        CALL SLEEP(1)
              

        FORALL (i = 0:broj_Cvorova, j = 1:broj_Grana, ((grane(j)%Get_Donji_Cvor() .EQ. i) .or. (grane(j)%Get_Gornji_Cvor() == i)))
            
            cvorovi(i)%Y0_sum = cvorovi(i)%Y0_sum + 0.5 * ( grane(j)%Get_Y_Grana() )  
         
        END FORALL


    END SUBROUTINE Proracun_Y0_sum


    !********************************************************************************************
    !Proracun kompleksne snage po granama

    SUBROUTINE Proracun_Snage_Grana()
    
        
        INTEGER :: i
        CLASS(GRANA), POINTER  :: p_Grana


        WRITE (*, *) '    Proracun snage po granama...'

        CALL SLEEP(1)


        DO i = 1, broj_Grana
            p_Grana => grane(i)
            p_Grana%S_12_Grane = cvorovi( p_Grana%Get_Gornji_Cvor() )%V_Cvora * CONJG( p_Grana%J_Grane )
        ENDDO
        

    END SUBROUTINE Proracun_Snage_Grana
    
    
    !********************************************************************************************
    !Proracun gubitaka aktivne snage u svakoj grani i ukupnih gubitaka aktivne snage mreze

    SUBROUTINE Proracun_Gubitaka_Snage()
    
        
        INTEGER :: i
        CLASS(GRANA), POINTER  :: p_Grana
        REAL :: rezistansa
        REAL :: struja_Moduo


        WRITE (*, *) '    Proracun gubitaka snage i ukupnih gubitaka snage...'

        CALL SLEEP(1)


        rezistansa = Inicijalizacija(REAL(vr_nula))
        struja_Moduo = Inicijalizacija(REAL(vr_nula))

        Pg_SUM = Inicijalizacija(0.0)

        DO i = 1, broj_Grana

            p_Grana => grane(i)
            rezistansa = REAL( p_Grana%Get_Z_Grana() )
            struja_Moduo = ABS( p_Grana%J_Grane )

            p_Grana%Pg_Grane = rezistansa * struja_Moduo**2         !prema relaciji 3.16 iz dokumentacije

            Pg_SUM = Pg_SUM + p_Grana%Pg_Grane

        ENDDO
        

    END SUBROUTINE Proracun_Gubitaka_Snage

    !********************************************************************************************


END MODULE Tokovi_Snaga


!================================================================================================
!Inicijalizacija promenljivih tipa COMPLEX (sa OPTIONAL argumentom)

FUNCTION COMPLEX_Inicijalizacija(in_vrednost) 


    COMPLEX, INTENT(IN), OPTIONAL :: in_vrednost
    COMPLEX :: COMPLEX_Inicijalizacija


    IF ( PRESENT(in_vrednost) ) THEN
        COMPLEX_Inicijalizacija = in_vrednost
    ELSE
        COMPLEX_Inicijalizacija = 0
    END IF


END FUNCTION COMPLEX_Inicijalizacija

!================================================================================================


