MODULE Bazne_Velicine


USE, INTRINSIC :: ISO_C_BINDING
USE EES_Elementi, ONLY: ID_File
USE Topologija
USE Topologija_Helper

IMPLICIT NONE


ENUM, BIND(C)
    ENUMERATOR :: APS_u_REL = 2, REL_u_APS
ENDENUM

!********************************************************************************************
!Promenljive vezane za proracun baznih velicina

!Osnovne bazne velicine
REAL :: S_bazno        ![VA]
REAL :: V_bazno        ![V]

!Izvedene bazne velicine
REAL :: I_bazno        ![A]
REAL :: Z_bazno        ![Om]
REAL :: Y_bazno        ![S]


!********************************************************************************************

REAL :: Pg_SUM


!================================================================================================
!================================================================================================

CONTAINS


    !********************************************************************************************
    !Ucitavanje podataka za bazne velicine iz ulazne datoteke

    FUNCTION Ucitaj_Bazne_Velicine() RESULT(Error)


        LOGICAL :: Error

        Error = .false.


        WRITE (*, *) '  Ucitavanje podataka za bazne velicine...'

        CALL SLEEP(1)


        OPEN (UNIT = ID_File, FILE='ULAZ_Bazne_Vrednosti.txt', ACTION = 'READ', STATUS = 'UNKNOWN', ERR = 100)

        
        READ (ID_File, '(16x, e12.1)', ERR=101) S_bazno
        
        READ (ID_File, '(15x, e12.1)', ERR=101) V_bazno


        CLOSE(ID_File)


        RETURN

        100 WRITE (*,*)  "[ERROR] Ne postoji ulazna datoteka 'Bazne_Vrednosti' na odgovarajucoj lokaciji!"
        Error = .true.
        STOP

        101 WRITE (*,*) "[ERROR] Greska u formatu ulazne datoteke 'Bazne_Vrednosti'!"
        Error = .true.
        STOP


    END FUNCTION Ucitaj_Bazne_Velicine

    !********************************************************************************************
    !Proracun izvedenih baznih velicina

    SUBROUTINE Proracun_Izvedenih_Baznih_Velicina()
    
        
        WRITE (*, *) '  Proracun izvedenih baznih velicina...'          !prema relacijama 3.1 iz dokumentacije

        CALL SLEEP(1)

        I_bazno = S_bazno / (SQRT(3.) * V_bazno) 
        Z_bazno = (V_bazno * V_bazno) / S_bazno
        Y_bazno = S_bazno / (V_bazno**2) 
          

    END SUBROUTINE Proracun_Izvedenih_Baznih_Velicina

    !********************************************************************************************
    !Pretvaranje velicina vezanih za CVOROVE I GRANE iz apsolutnih u relativne jedinice i obrnuto

    SUBROUTINE Pretvarac_APS_REL(tip_Pretvaraca)


    INTEGER, INTENT(IN) :: tip_Pretvaraca
    INTEGER :: i

    CLASS(CVOR), POINTER  :: p_Cvor
    CLASS(GRANA), POINTER :: p_Grana


    SELECT CASE (tip_Pretvaraca)

        !.........................................................
        !Pretvaranje iz APSOLUTNIH u RELATIVNE jedninice, relacija 3.2 iz dokumentacije
        
        CASE (APS_u_REL)   
        
            WRITE (*, *) '    Pretvaranje velicina iz apsolutnih u relativne jedinice...'                                    
            
            CALL SLEEP(1)

            !velicine cvorova
            DO i = 0, broj_Cvorova
                
                p_Cvor => cvorovi(i)

                !p_Cvor%V_Cvora = p_Cvor%V_Cvora / V_bazno                   
                !p_Cvor%I_inj_Cvora = p_Cvor%I_inj_Cvora / I_bazno           
                !p_Cvor%S_Cvora = p_Cvor%S_Cvora / S_bazno                   

                !polja su private => pristup preko SET metoda i GET metoda
                CALL p_Cvor%Set_P_cons_Cvor( p_Cvor%Get_P_cons_Cvor() * 1000 / S_bazno ) 
                CALL p_Cvor%Set_Q_cons_Cvor( p_Cvor%Get_Q_cons_Cvor() * 1000 / S_bazno )
                CALL p_Cvor%Set_S_cons_Cvor( p_Cvor%Get_S_cons_Cvor() * 1000 / S_bazno )                      

            ENDDO

            !velicine grana
            DO i = 1, broj_Grana
                
                p_Grana => grane(i)

                !p_Grana%J_Grane = p_Grana%J_Grane / I_bazno         

                !polja su private => pristup preko SET metoda i GET metoda
                CALL p_Grana%Set_Z_Grana( p_Grana%Get_Z_Grana() / Z_bazno )  
                CALL p_Grana%Set_Y_Grana( p_Grana%Get_Y_Grana() / Y_bazno )                        

            ENDDO
        

        !.........................................................
        !Pretvaranje iz RELATIVNIH u APSOLUTNE jedinice, relacija 3.3 iz dokumentacije

        CASE (REL_u_APS)  
                                            
            WRITE (*, *) '    Pretvaranje velicina iz relativnih u apsolutne jedinice...' 

            CALL SLEEP(1)
            
            !velicine cvorova
            DO i = 0, broj_Cvorova
                
                p_Cvor => cvorovi(i)

                p_Cvor%V_Cvora = p_Cvor%V_Cvora * V_bazno               
                p_Cvor%I_inj_Cvora = p_Cvor%I_inj_Cvora * I_bazno       
                p_Cvor%S_Cvora = p_Cvor%S_Cvora * S_bazno               

                !polja su private => pristup preko SET metoda i GET metoda
                CALL p_Cvor%Set_P_cons_Cvor( p_Cvor%Get_P_cons_Cvor() / 1000 * S_bazno ) 
                CALL p_Cvor%Set_Q_cons_Cvor( p_Cvor%Get_Q_cons_Cvor() / 1000 * S_bazno )
                CALL p_Cvor%Set_S_cons_Cvor( p_Cvor%Get_S_cons_Cvor() / 1000 * S_bazno ) 

            ENDDO

            !velicine grana
            DO i = 1, broj_Grana
                
                p_Grana => grane(i)

                p_Grana%J_Grane = p_Grana%J_Grane * I_bazno
                p_Grana%S_12_Grane = p_Grana%S_12_Grane * S_bazno
                p_Grana%Pg_Grane = p_Grana%Pg_Grane * S_bazno         

                !polja su private => pristup preko SET metoda i GET metoda
                CALL p_Grana%Set_Z_Grana( p_Grana%Get_Z_Grana() * Z_bazno )  
                CALL p_Grana%Set_Y_Grana( p_Grana%Get_Y_Grana() * Y_bazno )  

            ENDDO

            Pg_SUM = Pg_SUM * S_bazno

        !.........................................................

        CASE DEFAULT
            STOP ("[ERROR] Pogresan podatak za tip pretvaraca!")

        END SELECT


    END SUBROUTINE Pretvarac_APS_REL

    !********************************************************************************************

END MODULE Bazne_Velicine