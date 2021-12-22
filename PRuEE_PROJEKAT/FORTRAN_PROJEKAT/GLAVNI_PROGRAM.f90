PROGRAM Glavni


USE EES_Elementi, ONLY: ID_File
USE EES_Ucitavanje
USE Topologija_Helper
USE Bazne_Velicine
USE Tokovi_Snaga


IMPLICIT NONE


INTERFACE


    SUBROUTINE Ispis(jed_Ispisa)

        INTEGER, INTENT(IN) :: jed_Ispisa

    END SUBROUTINE Ispis


END INTERFACE


WRITE (*, *) 'PROGRAM PRORACUN TOKOVA SNAGA ALGORITMOM SUMIRANJA STRUJA JE POKRENUT...'

CALL SLEEP(1)


!....................................................................................................
!Ucitavanje podataka o broju elemanata EES

Global_Error = Ucitaj_Broj_Elemenata_Mreze()
IF (Global_Error) STOP ("[ERROR] Problem prilikom ucitavanja podataka o broju elemenata mreze!")


!....................................................................................................
!Ucitavanje podataka za VODOVE iz ulazne datoteke

Global_Error = Ucitaj_Vodove()
IF (Global_Error) STOP ("[ERROR] Problem prilikom ucitavanja podataka za vodove!")


!....................................................................................................
!Ucitavanje podataka za POTROSACE iz ulazne datoteke

Global_Error = Ucitaj_Potrosace()
IF (Global_Error) STOP ("[ERROR] Problem prilikom ucitavanja podataka za potrosace!")


!....................................................................................................
!Ucitavanje podataka za topologiju iz ulazne datoteke 'Topol_Podaci'

CALL Ucitaj_Topologiju('ULAZ_Topol_Podaci.txt')


!....................................................................................................
!Proracun parametara cvorova

CALL Proracun_Parametara_CVOR()


!....................................................................................................
!Proracun parametara grana

CALL Proracun_Parametara_GRANA()


!....................................................................................................
!Ucitavanje podataka za bazne velicine iz ulazne datoteke

Global_Error = Ucitaj_Bazne_Velicine()
IF (Global_Error) STOP ("[ERROR] Problem prilikom ucitavanja podataka za bazne velicine!")


!....................................................................................................
!Proracun izvedenih baznih velicina

CALL Proracun_Izvedenih_Baznih_Velicina()


!....................................................................................................
!Proracun tokova snaga i promenljivih stanja mreze

CALL Proracun_Tokova_Snaga()


!....................................................................................................
!Ispis dobijenih rezultata u izlaznu datoteku

CALL Ispis(ID_File)


!....................................................................................................
!Ispis dobijenih rezultata na ekran
!Kao argument moze biti bilo koja celobrojna vrednost osim vrednosti ID_File

CALL Ispis(7)


!....................................................................................................
!Oslobadjanje dinamicke memorije

CALL Dealokacija()


WRITE (*, '(a, /)') 'PROGRAM USPESNO IZVRSEN.'


PAUSE
END PROGRAM Glavni