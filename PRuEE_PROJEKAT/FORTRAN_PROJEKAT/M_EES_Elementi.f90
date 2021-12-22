MODULE EES_Elementi


IMPLICIT NONE


INTEGER, PARAMETER :: ID_File = 50


!********************************************************************************************
!Broj elemenata mreze

TYPE, PUBLIC :: EES_Podaci

    PRIVATE
        INTEGER :: broj_Vodova
        INTEGER :: broj_Potrosaca


    CONTAINS

        PROCEDURE, PUBLIC :: Ucitaj_EES           => Ucitaj_podatke_za_EES
        PROCEDURE, PUBLIC :: Get_Broj_Vodova      => Uzmi_podatak_za_broj_vodova
        PROCEDURE, PUBLIC :: Get_Broj_Potrosaca   => Uzmi_podatak_za_broj_potrosaca


END TYPE EES_Podaci

PRIVATE :: Ucitaj_podatke_za_EES, Uzmi_podatak_za_broj_vodova
PRIVATE :: Uzmi_podatak_za_broj_potrosaca



!********************************************************************************************
!Korisnicki definisani tip koji modeluje ELEMENT mreze

TYPE, PUBLIC :: Element_EES
    
    PRIVATE
        INTEGER :: ID_Elementa        !sifra elementa


    CONTAINS

        PROCEDURE, PUBLIC :: Ucitaj   => Ucitaj_podatke_za_ELEMENT  !Definisana u okviru ove natklase, pa preklopljena u
                                                                    !potklasama -> princip polimorfizma "automatski radi"
        PROCEDURE, PUBLIC :: Get_ID   => Uzmi_ID_za_ELEMENT         !Definisana u natklasi   
                                                                    

END TYPE Element_EES

PRIVATE :: Ucitaj_podatke_za_ELEMENT, Uzmi_ID_za_ELEMENT


!********************************************************************************************
!Korisnicki definisani tip koji modeluje VODOVE u mrezi

TYPE, PUBLIC, EXTENDS(Element_EES) :: VOD

    PRIVATE
        REAL :: r_poduzno       !poduzna rezistansa [Om/km]
        REAL :: x_poduzno       !poduzna reaktansa [Om/km]
        REAL :: g_poduzno       !poduzna konduktansa [S/km]
        REAL :: b_poduzno       !poduzna susceptansa [S/km]
        REAL :: duzina          !duzina deonice [km]


    CONTAINS
        
        PROCEDURE, PUBLIC :: Ucitaj          => Ucitaj_podatke_za_VOD
        PROCEDURE, PUBLIC :: Get_r_poduzno   => Uzmi_r_poduzno_za_VOD
        PROCEDURE, PUBLIC :: Get_x_poduzno   => Uzmi_x_poduzno_za_VOD
        PROCEDURE, PUBLIC :: Get_g_poduzno   => Uzmi_g_poduzno_za_VOD
        PROCEDURE, PUBLIC :: Get_b_poduzno   => Uzmi_b_poduzno_za_VOD
        PROCEDURE, PUBLIC :: Get_duzina      => Uzmi_duzinu_za_VOD


END TYPE VOD

PRIVATE :: Ucitaj_podatke_za_VOD, Uzmi_r_poduzno_za_VOD, Uzmi_x_poduzno_za_VOD
PRIVATE :: Uzmi_g_poduzno_za_VOD, Uzmi_b_poduzno_za_VOD, Uzmi_duzinu_za_VOD


!********************************************************************************************
!Korisnicki definisani tip koji modeluje POTROSACE u mrezi

TYPE, PUBLIC, EXTENDS(Element_EES) :: POTROSAC

    PRIVATE
        REAL :: P_cons      !aktivna snaga potrosnje [kW]
        REAL :: Q_cons      !reaktivna snaga potrosnje [kVAr]


    CONTAINS
        
        PROCEDURE, PUBLIC :: Ucitaj       => Ucitaj_podatke_za_POTROSAC
        PROCEDURE, PUBLIC :: Get_P_cons   => Uzmi_P_cons_za_POTROSAC
        PROCEDURE, PUBLIC :: Get_Q_cons   => Uzmi_Q_cons_za_POTROSAC


END TYPE POTROSAC

PRIVATE :: Ucitaj_podatke_za_POTROSAC, Uzmi_P_cons_za_POTROSAC
PRIVATE :: Uzmi_Q_cons_za_POTROSAC

!================================================================================================
!================================================================================================

CONTAINS

    !********************************************************************************************
    !Ucitavanje podataka za mrezu iz ulazne datoteke

    FUNCTION Ucitaj_podatke_za_EES(this) RESULT (Error)


        CLASS (EES_Podaci) :: this
        LOGICAL            :: Error

        Error = .false.

        
        OPEN (UNIT = ID_File, FILE='ULAZ_Broj_Elemenata.txt', ACTION = 'READ', STATUS = 'UNKNOWN', ERR = 100)

        
        READ (ID_File, *, ERR=101) this%broj_Vodova

        READ (ID_File, '(I4)', ERR=101) this%broj_Potrosaca


        CLOSE(ID_File)


        RETURN

        100 WRITE (*,*)  "[ERROR] Ne postoji ulazna datoteka 'Broj_Elemenata'! na odgovarajucoj lokaciji"
        Error = .true.
        STOP

        101 WRITE (*,*) "[ERROR] Greska u formatu ulazne datoteke 'Broj_Elemenata'!"
        Error = .true.
        STOP

    END FUNCTION Ucitaj_podatke_za_EES

    !********************************************************************************************

    FUNCTION Uzmi_podatak_za_broj_vodova(this) RESULT(broj_Vod)


        CLASS (EES_Podaci) :: this
        INTEGER            :: broj_Vod

        Broj_Vod = this%broj_Vodova


    END FUNCTION Uzmi_podatak_za_broj_vodova

    !********************************************************************************************

    FUNCTION Uzmi_podatak_za_broj_potrosaca(this) RESULT(broj_Potrosac)


        CLASS (EES_Podaci) :: this
        INTEGER            :: broj_Potrosac

        broj_Potrosac = this%broj_Potrosaca


    END FUNCTION Uzmi_podatak_za_broj_potrosaca

    !********************************************************************************************

    FUNCTION Ucitaj_podatke_za_ELEMENT(this) RESULT(Error)


        CLASS (Element_EES)  :: this
        LOGICAL              :: Error

        Error = .false.

    END FUNCTION Ucitaj_podatke_za_ELEMENT

    !********************************************************************************************

    FUNCTION Uzmi_ID_za_ELEMENT(this) RESULT(ID_Element)


        CLASS (Element_EES) :: this
        INTEGER             :: ID_Element

        ID_Element = this%ID_Elementa


    END FUNCTION Uzmi_ID_za_ELEMENT

    !********************************************************************************************

    FUNCTION Ucitaj_podatke_za_VOD(this) RESULT(Error)

        CLASS (Vod)      :: this
        LOGICAL          :: Error

        Error = .false.


        READ (ID_File, 200, ERR = 101) this%ID_Elementa,                                          &
                     this%r_poduzno, this%x_poduzno, this%g_poduzno, this%b_poduzno, this%duzina

        200 FORMAT ( 5x, i9,                                          &
                      /, f10.4, 1x, f10.4, 1x, 2( e10.3, 1x ), f10.3)


        RETURN

        101 WRITE (*,*) "[ERROR] Greska u formatu ulazne datoteke 'Deonice_Podaci'!"
        Error = .true.
        STOP

    END FUNCTION Ucitaj_podatke_za_VOD

    !********************************************************************************************

    FUNCTION Uzmi_r_poduzno_za_VOD(this) RESULT(vod_r_poduzno)


        CLASS (Vod)    :: this
        REAL           :: vod_r_poduzno

        vod_r_poduzno = this%r_poduzno


    END FUNCTION Uzmi_r_poduzno_za_VOD

    !********************************************************************************************

    FUNCTION Uzmi_x_poduzno_za_VOD(this) RESULT(vod_x_poduzno)


        CLASS (Vod)    :: this
        REAL           :: vod_x_poduzno

        vod_x_poduzno = this%x_poduzno


    END FUNCTION Uzmi_x_poduzno_za_VOD

    !********************************************************************************************

    FUNCTION Uzmi_g_poduzno_za_VOD(this) RESULT(vod_g_poduzno)


        CLASS (Vod)    :: this
        REAL           :: vod_g_poduzno

        vod_g_poduzno = this%g_poduzno


    END FUNCTION Uzmi_g_poduzno_za_VOD

    !********************************************************************************************

    FUNCTION Uzmi_b_poduzno_za_VOD(this) RESULT(vod_b_poduzno)


        CLASS (Vod)    :: this
        REAL           :: vod_b_poduzno

        vod_b_poduzno = this%b_poduzno


    END FUNCTION Uzmi_b_poduzno_za_VOD

    !********************************************************************************************

    FUNCTION Uzmi_duzinu_za_VOD(this) RESULT(vod_duzina)


        CLASS (Vod)   :: this
        REAL          :: vod_duzina

        vod_duzina = this%duzina


    END FUNCTION Uzmi_duzinu_za_VOD

    !********************************************************************************************

    FUNCTION Ucitaj_podatke_za_POTROSAC(this) RESULT(Error)

        CLASS (Potrosac) :: this
        LOGICAL          :: Error

        Error = .false.


        READ (ID_File,'(5x, i9, 2( 1x, f10.5 ))', ERR = 101) this%ID_Elementa, this%P_cons, this%Q_cons


        RETURN

        101 WRITE (*,*) "[ERROR] Greska u formatu ulazne datoteke 'Potr_Podaci'!"
        Error = .true.
        STOP

    END FUNCTION Ucitaj_podatke_za_POTROSAC

    !********************************************************************************************

    FUNCTION Uzmi_P_cons_za_POTROSAC(this) RESULT(potr_P_cons)


        CLASS (Potrosac)  :: this
        REAL              :: potr_P_cons

        potr_P_cons = this%P_cons


    END FUNCTION Uzmi_P_cons_za_POTROSAC

    !********************************************************************************************

    FUNCTION Uzmi_Q_cons_za_POTROSAC(this) RESULT(potr_Q_cons)


        CLASS (Potrosac)  :: this
        REAL              :: potr_Q_cons

        potr_Q_cons = this%Q_cons


    END FUNCTION Uzmi_Q_cons_za_POTROSAC

    !********************************************************************************************

END MODULE EES_Elementi