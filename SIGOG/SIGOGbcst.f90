!   Simulated GPS Observations Generator (SiGOG)
!
!   (c) Copyright 2004 Elsa Mohino <emohino@fis.ucm.es>,
!   Universidad Complutense de Madrid, Facultad de Ciencias F�sicas
!
!   This program is free software; you can redistribute it and/or
!   modify it under the terms of the GNU General Public License as
!   published by the Free Software Foundation; either version 2 of
!   the License, or (at your option) any later version.
!
!   The author does NOT admit liability nor provide warranty for any
!   of this software. This material is provided "AS-IS" in the hope
!   that it may be useful for others.

!       *****************************************************************
!       *****************************************************************
!       *                       PROGRAM SiGOGbcst                       *
!       *****************************************************************
!       *****************************************************************
!       V2.2                     by: E. Mohino
!                                                Creation date : 11/XI/02
!                                            Last actualization: 15/IX/04
!       *****************************************************************
!       Purpose: This programme will generate a RINEX observation file
!               given the location of observer and period of observation
!               from a RINEX navigation file.
!       *****************************************************************
!       Family of files involved:
!       Input:
!               -input.txt (Input data file)
!               -file with navigation RINEX orbits 
!       Output:
!               -rinex file
!               -s//igs//weekday.sp3 (files with precise orbits "clean"
!                       defects
!       Intermediate:   several families(names depend on station)
!               -sat0#.aux :prn # satellite ephemeris
!               -sat#.aux family (satellite position for each prn# and
!                       observation epoch
!               -"esta"sat#.aux family (distance, elevation and azimuth of satellite
!                       prn # for each observation epoch. "esta" station's name)
!               -"esta"sat#.aux family (distance, elevation and azimuth of satellite
!                       prn # for each observation epoch for which elevation is above
!                       threshold)
!       *****************************************************************
!       List of SUBROUTINES used by this program:
!               lec21(filein,nsat,iden,niden)
!               intorb(filein,sat,nep,annio0,mes0,dia0,hora0,min0,
!                       sec0,nnep,inter,xo,yo,zo)
!               posorb(t,toe,m0,sqrta,dn,e,omp,om0,w0,cuc,cus,crc,crs,
!                       cic,cis,i0,ip,x,y,z,rel)
!               posic(filein,nsat,nnep,xo,yo,zo,estac,iden,lato,lono)
!               calculo(xo,yo,zo,lato,lono,xs,ys,zs,dist,caz,lats,lons,
!                       azim,alt)
!               umbral(iden,nsat,nnep,umb,estac)
!               rinex(fecha,annio0,mes0,dia0,hora0,min0,sec0,nep,nsat,iden,
!                       interv,xpos,ypos,zpos,estac,dh,de,dn)
!       *****************************************************************
!       List of FUNCTIONS used by this programme:
!               cdiaj(annio,mes,dia)
!               segGPS(annio,mes,dia,hora,min,sec)
!               diajul(annio,mes,dia)
!               seeber2(P,temp,hr,e,xo,yo,zo,lato)
!       *****************************************************************
!       *****************************************************************


PROGRAM observa

    ! Formats
    10 FORMAT(A)
    20 FORMAT(2X, 3(F14.7, 1X))
    30 FORMAT(2X, A4, 1X, 5A2)
    32 FORMAT(2X, A20)
    40 FORMAT(2X, F4.1, 1X, A1)
    50 FORMAT(2X, I4, 4(1X, I2), 1X, F11.8)
    60 FORMAT(2X, 2(I2, 1X), F11.8)
    70 FORMAT(2X, F8.2)

    IMPLICIT NONE
    INTEGER annioi, mesi, diai, horai, mini, horaf, minf, nsat, nnep, niden(85)&
            , i, nep
    !       annioi,mesi,diai,horai,mini: date & initial epoch of observation
    !       horaf,minf: final epoch of observation
    !       nsat:total number of satellites in order for observation day
    !       nnep:total number of epochs to observe
    !       niden:total number of satellites available
    !       i:counter
    !       nep:total number of epochs for which there are satellite positions
    DOUBLE PRECISION seci, secf, inter, xo, yo, zo, umb, dnnep, lato, lono&
            , xor, yor, zor, dh, de, dn
    !       seci:seconds of initial epoch
    !       secf:seconds of final epoch
    !       inter: inteval between consecutive observations
    !       xo,yo,zo:antenna position of receiver in km
    !       umb:elevation threshold (elevation mask)
    !       dnnep:auxiliar variable for calculus of integer number of epochs
    !       lato,lono: observation position latitude and longitude
    !       xor,yor,zor: observer position (NOT antenna)
    !       dh,de,dn:antenna position from observer
    CHARACTER aux2*2, filein*30, iden(85)*3, estac*4, fecha*20&
            , ssss*4, ddd*3, yy*2, f*1, t*1, cdiaj*3, sat*3, to(5)*2, trer*1
    !       aux2:auxiliar variable to detect relevant information in input file
    !       filein:broadcasted ephemeris file name
    !       iden:arrya with prn of satellites
    !       estac:observation station name
    !       fecha:date of creation of RINEX observation file
    !       ssss:receiver name
    !       ddd:julian date of observation
    !       yy:year of observation
    !       f:number of RINEX file
    !       t:type of RINEX file (navigation)
    !       cdiaj:julian date of observation
    !       sat:one satellite prn
    !       to:types of observable to generate: C1P1P2L1L2
    !       trer: calculate tropospheric error? S/N
    ! End declaration variables


    ! Reading input file:
    OPEN(12, FILE = 'input.txt')

    99 CONTINUE
    ! 1) Receiver coordinates (km)
    100 CONTINUE
    READ(12, 10, END = 500)aux2
    IF(aux2.NE.'**')GOTO 100
    BACKSPACE 12
    READ(12, 20) xor, yor, zor

    ! 2) Station name and observable to generate (C1,P1,P2,L1,L2)
    110 CONTINUE
    READ(12, 10)aux2
    IF(aux2.NE.'**')GOTO 110
    BACKSPACE 12
    READ(12, 30) estac, to

    ! 3) Elevation mask in degrees
    120 CONTINUE
    READ(12, 10)aux2
    IF(aux2.NE.'**')GOTO 120
    BACKSPACE 12
    READ(12, 40) umb, trer

    ! 4) Date and initial epoch of observation
    130 CONTINUE
    READ(12, 10)aux2
    IF(aux2.NE.'**')GOTO 130
    BACKSPACE 12
    READ(12, 50) annioi, mesi, diai, horai, mini, seci

    ! 5) Final epoch
    140 CONTINUE
    READ(12, 10)aux2
    IF(aux2.NE.'**')GOTO 140
    BACKSPACE 12
    READ(12, 60) horaf, minf, secf

    ! 6) Interval between observations
    150 CONTINUE
    READ(12, 10)aux2
    IF(aux2.NE.'**')GOTO 150
    BACKSPACE 12
    READ(12, 70)inter

    ! 7) Date of creation of RINEX observation file
    160 CONTINUE
    READ(12, 10)aux2
    IF(aux2.NE.'**')GOTO 160
    BACKSPACE 12
    READ(12, 32)fecha

    ! 8) Position of antenna from receiver
    170 CONTINUE
    READ(12, 10)aux2
    IF(aux2.NE.'**')GOTO 170
    BACKSPACE 12
    READ(12, '(8X,3F8.4)')dh, de, dn
    ! Finished reading input file

    ! Coordinates of antenna:
    CALL antena(xor * 1.0D3, yor * 1.0D3, zor * 1.0D3, dh, de, dn, xo, yo, zo)
    xo = xo / 1.0D3
    yo = yo / 1.0D3
    zo = zo / 1.0D3

    ! Name of RINEX navigation file:
    ssss = 'brdc'
    ddd = cdiaj(annioi, mesi, diai)
    f = '0'
    yy = CHAR(48 + MOD(annioi, 100) / 10) // CHAR(48 + MOD(annioi, 10))
    t = 'n'
    filein = ssss // ddd // f // '.' // yy // t


    ! Reading broadcasted ephemeris, calculating satellite positions in t-tau,
    ! satellite clock error. Record data in files sat//input file
    CALL lec21(filein, nsat, iden, niden)

    ! Reading orbit information. Interpolation of data. Satellite position
    ! calculation for epoch t-tau. Satellite clock error estimation.
    nnep = (horaf - horai) * 3600 + (minf - mini) * 60 + (secf - seci)
    dnnep = (horaf - horai) * 3600. + (minf - mini) * 60. + (secf - seci)
    nnep = nnep / inter

    DO i = 1, nsat
        sat = iden(i)
        nep = niden(i)
        CALL intorb(filein, sat, nep, annioi, mesi, diai, horai, mini, &
                seci, nnep, inter, xo, yo, zo)
    ENDDO

    ! Calculating azimuths and elevations, as well as tropospheric errors, once
    ! known elevation of each satellite

    CALL posic(filein, nsat, nnep, xo, yo, zo, estac, iden, trer, lato, lono)


    ! Filtering data with elevation mask given by umb
    CALL umbral(iden, nsat, nnep, umb, estac)

    ! Finally, generating RINEX observation file:
    CALL rinex(fecha, annioi, mesi, diai, horai, mini, seci&
            , nnep, nsat, iden, inter, xor * 1000.0d0&
            , yor * 1000.0d0, zor * 1000.0d0, estac&
            , to, trer, dh, de, dn)

    GOTO 99

    500 CONTINUE
    CLOSE(12)

END


!       *****************************************************************
!       *****************************************************************
!       *                       SUBROUTINE ANTENA                        *
!       *****************************************************************
!       *****************************************************************
!       V1.1                     By: E. Moh�no
!                                                Creation date:   25/X/04
!                                            Last actualization:  25/X/04
!       *****************************************************************
!       Purpose:
!               This subroutine will include antenna position referred to
!               observators position in order to calculate RINEX observation
!               files referred to phase center antenna. In a first aproach
!               it will only consider antenna height displacement.
!       *****************************************************************
!       Input Variables
!               -xo,yo,zo: observator position in METERS in cartesian
!                       ECEF frame
!               -dh,de,dn: antenna position referred to receiver position
!                       in METERS.
!       Output Variables
!               -x,y,z: phase center antenna position in cartesian ECEF reference
!                       system in METERS.
!       *****************************************************************
!       *****************************************************************

SUBROUTINE antena(xo, yo, zo, dh, de, dn, x, y, z)

    ! This subroutine is based in ecuations from Seeber (1993), page 19 and following.
    ! A similar procedure can be seen in Hofmann-Wellenhof, pp255 and following.

    ! Declaration of variables
    IMPLICIT NONE
    DOUBLE PRECISION xo, yo, zo, dh, de, dn, x, y, z, a, e2&
            , lat, p, N, h, hant, lon, PI, dlon, dlat
    !               xo,yo,zo:position of receiver
    !               dh,de,dn: position of antenna referred to receiver
    !               x,y,z:position of antenna in ECEF reference system
    !               a:Earth semimajor axis of ellipsoid
    !               e2:first numerical eccentricity
    !               lat:elipsoidical latitude of antenna phase center
    !               p:radius of ecuatorial proyection of antenna phase center
    !               N:radius of curvature in prime vertical
    !               h:height of antena phase center
    !               hant:height of antena phase center in previous step
    !               lon:elipsoidical longitude of antena phase center
    !               PI:pi number
    !               dlat:elipsoidical latide difference between receiver and antenna
    !               dlon:elipsoidical longitude difference between receiver and antenna
    PARAMETER(a = 6378137.0D0, e2 = 6.69437999013D-3)
    INTEGER cont
    !               cont: counter
    ! End declaration variables

    PI = 4.0D0 * DATAN(1.0D0)

    p = DSQRT(xo**2.0D0 + yo**2.0D0)
    h = 0.0D0
    N = 1.0D0
    cont = 0
    100 CONTINUE
    cont = cont + 1
    hant = h
    lat = DATAN(zo / (p * (1 - e2 * N / (N + h))))
    N = a / DSQRT(1 - e2 * (DSIN(lat))**2.0D0)
    h = p / DCOS(lat) - N
    IF(ABS(hant - h).GT.1.0D-6) GOTO 100

    lon = DATAN(yo / xo)
    if(xo.LT.0.0D0)lon = lon + PI

    dlat = dn / (N + h)
    dlon = de / (N * DCOS(lat))

    x = (N + h + dh) * DCOS(lat + dlat) * DCOS(lon + dlon)
    y = (N + h + dh) * DCOS(lat + dlat) * DSIN(lon + dlon)
    z = ((1 - e2) * N + h + dh) * DSIN(lat + dlat)
    !      x=(N+h+dh)*DCOS(lat)*DCOS(lon)
    !      y=(N+h+dh)*DCOS(lat)*DSIN(lon)
    !      z=((1-e2)*N+h+dh)*DSIN(lat)

    RETURN
END


!       *****************************************************************
!       *****************************************************************
!       *                       FUNCTIONN CDIAJ                         *
!       *****************************************************************
!       *****************************************************************
!       V1.1            By: E. Moh�no
!                                                  Creation date:03/XI/02
!                                            Last actualization:02/XII/02
!       *****************************************************************
!       Purpose: calculate julian date for a given day of month of year
!       *****************************************************************
!       Input Variables
!               -annio:year
!               -mes: month
!               -dia: day
!       *****************************************************************
!       Function output: cdiaj: character with julian day
!       *****************************************************************
!       *****************************************************************

CHARACTER*3 FUNCTION cdiaj(annio, mes, dia)

    ! Declaration of variables
    IMPLICIT NONE
    INTEGER annio, mes, dia, diames(12), jdia, j, cent, dec, num
    !       annio= year
    !       mes= month
    !       dia= day
    !       diames= vector with total number of days per month
    !       jdia= elapsed days from begining of year
    !       j= couter
    !       cent=hundres in jdia
    !       dec=tens in jdia
    !       num=units in jdia
    ! End declaration variables
    ! Definition of diames
    diames(1) = 31
    diames(2) = 28
    diames(3) = 31
    diames(4) = 30
    diames(5) = 31
    diames(6) = 30
    diames(7) = 31
    diames(8) = 31
    diames(9) = 30
    diames(10) = 31
    diames(11) = 30
    diames(12) = 31

    ! Modify if annio is leap year
    IF (MOD(annio, 4).EQ.0.AND.MOD(annio, 100).NE.0) diames(2) = 29
    IF (MOD(annio, 400).EQ.0) diames(2) = 29

    ! Estimate days elapsed form begining of year
    jdia = dia
    IF (mes.LT.2) GOTO 100
    DO j = 1, mes - 1
        jdia = jdia + diames(j)
    ENDDO
    100 CONTINUE

    ! Calculate each number in jdia
    cent = jdia / 100
    dec = (jdia - cent * 100) / 10
    num = (jdia - cent * 100 - dec * 10)
    cdiaj = CHAR(48 + cent) // CHAR(48 + dec) // CHAR(48 + num)
    RETURN
END


!       *****************************************************************
!       *****************************************************************
!       *                       SUBROUTINE LEC21                        *
!       *****************************************************************
!       *****************************************************************
!       V1.0                     By: E. Moh�no
!                                                Creation date :10/XII/02
!                                         Last actualization :  19/XII/02
!       *****************************************************************
!       Purpose: separate ephemeris for each satellite recorded in a
!               RINEX navigation file
!       *****************************************************************
!       Files involved
!       Input:
!               -filein:RINEX navigation file
!       Output:
!               -sat0#filein: satellite prn # ephemeris
!       *****************************************************************
!       Input Variables
!               -filein:name of RINEX navigation file
!       Output Variables
!               -nsat:total number of available satellites in filein
!               -iden:array with prn of those satellites
!               -niden:array with number of epochs recorded for each sat.
!       *****************************************************************
!       *****************************************************************



SUBROUTINE lec21(filein, nsat, iden, niden)


    ! Formats
    10 FORMAT(A2)
    20 FORMAT(60X, A20)
    30 FORMAT(//////A3)
    40 FORMAT(A80)

    ! Definition  variables
    IMPLICIT NONE
    CHARACTER filein*30, iden(85)*3, endh*20, aux2*2, aux3*3&
            , aux1*1, fileout*30, aux80*80
    !       filein=name of RINEX navigation file
    !       iden=array with prn of available satellites
    !       endh=to locate END OF HEADER line
    !       aux2,aux3,aux1,aux80=auxiliar variables to locate different parts
    !       fileout=name of output files
    INTEGER nsat, niden(85), i, j
    !       nsat=total number of available satellites
    !       niden=array with number of epochs recorded for each satellite
    !       i,j=counters

    DO i = 1, 85
        iden(i) = '  '
        niden(i) = 0
    ENDDO

    OPEN(15, FILE = filein, STATUS = 'OLD')

    ! GO to END OF HEADER
    200 CONTINUE
    READ(15, 20)endh
    IF(endh.NE.'END OF HEADER') GOTO 200

    ! How many satellites are they in filein? How many epochs recorded?
    210 CONTINUE
    READ(15, 10, END = 220)aux2
    aux3 = ' ' // aux2
    aux1 = 'y'
    DO i = 1, 85
        IF(aux3.EQ.iden(i)) aux1 = 'n'
    ENDDO
    IF(aux1.EQ.'y') THEN
        DO i = 85, 1, -1
            IF(iden(i).EQ.'   ') j = i
        ENDDO
        iden(j) = aux3
    ENDIF
    READ(15, 30)aux2
    GOTO 210
    220 CONTINUE
    nsat = j

    REWIND 15

    ! GO to END OF HEADER
    230 CONTINUE
    READ(15, 20)endh
    IF(endh.NE.'END OF HEADER') GOTO 230

    ! Open all output files
    DO i = 1, nsat
        fileout = 'sat0' // iden(i) // filein
        OPEN(15 + i, FILE = fileout)
    ENDDO

    ! Separate each satellite
    240 CONTINUE
    READ(15, 10, END = 250)aux2
    aux3 = ' ' // aux2
    j = -1
    DO i = 1, nsat
        IF(iden(i).EQ.aux3) j = i
    ENDDO

    BACKSPACE 15
    niden(j) = niden(j) + 1
    DO i = 1, 8
        READ(15, 40)aux80
        WRITE(15 + j, 40)aux80
    ENDDO
    GOTO 240

    250 CONTINUE

    DO i = 1, nsat
        CLOSE(15 + i)
    ENDDO
    CLOSE(15)

    RETURN
END


!       *****************************************************************
!       *****************************************************************
!       *                       SUBROUTINE POSORB                       *
!       *****************************************************************
!       *****************************************************************
!       V1.0                     By: E. Moh�no
!                                                Creation date :10/XII/02
!                                     Last actualization       :18/XII/02
!       *****************************************************************
!       Purpose: calculate satellite coordinates (x,y,x) and relativity
!               effect for each epoch given satellite ephemeris terms.
!       *****************************************************************
!       Input Variables
!               -t: elapsed seconds from begining GPS week
!               -toe: time of ephemeris
!               -m0:mean keplerian orbit anomaly
!               -sqrta: sqrt of semimajor orbit axis
!               -dn: rate of angular velocity of satellite (n)
!               -e: keplerian orbit eccentricity
!               -omp: rate of chang of right ascention
!               -om0: right ascention at toe
!               -w0: argument of perigee at reference time
!               -cuc: amplitude of cosine armonic correction term to the
!                       argument of perigee
!               -cus: amplitude of sine armonic correction term to the
!                       argument of perigee
!               -crc: amplitude of cosine armonic correction term to the
!                       orbit radius
!               -crs: amplitude of sine armonic correction term to the
!                       orbit radius
!               -cic: amplitude of cosine armonic correction term to the
!                       angle of inclination
!               -cis: amplitude of sine armonic correction term to the
!                       angle of inclination
!               -i0: inclination angle at reference time
!               -ip: rate of inclination angle
!       Output Variables
!               -x,y,z:satellite coordinates
!               -rel: relativistic effect
!       *****************************************************************
!       *****************************************************************


SUBROUTINE posorb(t, toe, m0, sqrta, dn, e, omp, om0&
        , w0, cuc, cus, crc, crs, cic, cis, i0, ip, x, y, z, rel)

    ! Definition variables
    IMPLICIT NONE
    DOUBLE PRECISION t, toe, m0, sqrta, dn, mu, m, tt, e&
            , emay, emayan, cv, sv, v, r0, r, a, PI, WE, omp, t2, om0, l, w0, w, u&
            , want, cuc, cus, crc, crs, i0, i, ip, cic, cis, rx, ry, x, y, z&
            , rel, fmay, c
    !       t= time for which we want satellite coordinates
    !       toe= time of reference
    !       m0= mean keplerian orbit anomaly
    !       sqrta= sqrt of semimajor orbit axis
    !       dn= rate of angular velocity of satellite (n)
    !       mu= WGS 84 value of Earth's universial gravitational parameter
    !       m=mean anomaly at time t
    !       tt=elapsed time since toe
    !       e= orbit eccentricity
    !       emay= Eccentric anomaly
    !       emayan=previous eccentric anomaly
    !       cv=cosine of v
    !       sv=sine of v
    !       v=true anomaly
    !       r0=satellite geocentric distance at toe
    !       r=satellite geocentric distance at t
    !       a=semimajor orbit axis
    !       PI= pi number
    !       WE=rotational velocity of Earth
    !       omp= rate of chang of right ascention
    !       t2=elapsed time since toe taking into account crossovers
    !       om0= right ascention at toe
    !       l= angle between Greenwich and ascending node
    !       w0= argument of perigee at toe
    !       w=argument of perigee at t
    !       u=angle between satellite and ascending node (w+v)
    !       want=w in previous step
    !       cuc: amplitude of cosine armonic correction term to the
    !                       argument of perigee
    !       cus: amplitude of sine armonic correction term to the
    !             argument of perigee
    !       crc: amplitude of cosine armonic correction term to the
    !                       orbit radius
    !       crs: amplitude of sine armonic correction term to the
    !                 orbit radius
    !       i0: inclination angle at reference time
    !       i=inclination at time t
    !       ip= rate of inclination angle
    !       cic: amplitude of cosine armonic correction term to the
    !                       angle of inclination
    !       cis: amplitude of sine armonic correction term to the
    !                       angle of inclination
    !       rx=position x component at orbit plane
    !       ry=position y component at orbit plane
    !       x,y,z=satellite coordinates
    !       rel=relativity effect
    !       fmay=constant to calculate re. (2*SQRT(mu)/c^2)
    !       c=vacuum light speed
    PARAMETER(mu = 3986005.0D8, WE = 7292115.1467D-11&
            , fmay = -4.442807633D-10, c = 299792458.0D0)
    INTEGER j
    !       j=counter

    PI = 4.0D0 * DATAN(1.0D0)
    tt = t - toe
    a = sqrta * sqrta

    m = m0 + (DSQRT(mu) / sqrta**3.0D0 + dn) * tt

    ! Calculate l taking into account crossovers
    t2 = t
    IF(t.GT.604799.0D0) t2 = t - IDINT(toe / 604799.0D0) * 604800.0D0
    l = om0 + omp * tt - WE * t2

    ! Calculate eccentric anomaly
    emayan = m
    DO j = 1, 10
        emay = m + e * DSIN(emayan)
        IF(abs((emay - emayan) / emay).LT.1.0D-14) GOTO 100
        emayan = emay
    ENDDO
    100 CONTINUE

    ! Calculate true anomaly
    r0 = a * (1 - e * DCOS(emay))
    cv = a * (DCOS(emay) - e) / r0
    sv = (a * DSQRT(1 - e * e) * DSIN(emay)) / r0
    v = DATAN(sv / cv)
    IF(cv.LT.0.0D0) v = v + PI
    IF(v.LT.0.0D0)v = v + 2.0D0 * PI


    ! Calculate argument of perigee
    want = w0 + cuc * DCOS(2.0D0 * (w0 + v)) + cus * DSIN(2.0D0 * (w0 + v))
    DO j = 1, 10
        w = w0 + cuc * DCOS(2.0D0 * (want + v)) + cus * DSIN(2.0D0 * (want + v))
        IF(abs((w - want) / w).LT.1.0D-14)GOTO 110
        want = w
    ENDDO
    110 CONTINUE

    u = w + v

    r = r0 + crc * DCOS(2.0D0 * u) + crs * DSIN(2.0D0 * u)

    i = i0 + cic * DCOS(2.0D0 * u) + cis * DSIN(2.0D0 * u) + ip * tt

    ! Calculate rx,ry coordinates of position at orbit plane
    rx = r * DCOS(u)
    ry = r * DSIN(u)

    ! Rotate to a geocentric reference sistem
    ! R=R{-l}R{-i}
    x = rx * DCOS(l) - ry * DSIN(l) * DCOS(i)
    y = rx * DSIN(l) + ry * DCOS(l) * DCOS(i)
    z = ry * DSIN(i)

    ! Relativity effect in meters
    rel = -fmay * e * sqrta * DSIN(emay) * c

    RETURN
END


!       *****************************************************************
!       *****************************************************************
!       *                       FUNCTION SEGGPS                         *
!       *****************************************************************
!       *****************************************************************
!       V1.0                       By: E. Moh�no
!                                                Creation date :10/XII/02
!                                       Last actualization :    10/XII/02
!       *****************************************************************
!       Purpose: calculate seconds of GPS week that define a certain epoch
!               given year,month,hour,minute and second of that epoch
!       *****************************************************************
!       Input Variables
!               -annio:year
!               -mes: month
!               -dia: day
!               -hora:hour
!               -min: minute
!               -sec: second
!       *****************************************************************
!       Output of function: segGPS: seconds elapsed since begining of GPS week
!       *****************************************************************
!       List of FUNCTIONS used
!               diajul(annio,mes,dia)
!       *****************************************************************
!       *****************************************************************


DOUBLE PRECISION FUNCTION segGPS(annio, mes, dia, hora, min, sec)

    ! Definition variables
    IMPLICIT NONE
    INTEGER annio, mes, dia, hora, min, dias, smn, i, diajul, annio4
    !       annio=year
    !       mes=month
    !       dia=day
    !       hora=hour
    !       min=minute
    !       dias=days elapsed from begining GPS week
    !       smn,i=counter
    !       diajul=function to calculate julian day
    !       annio4=year with 4 digits
    DOUBLE PRECISION sec
    !       sec=seconds

    ! Year in 4 digits, in case it only comes with 2
    annio4 = annio
    IF(annio.LT.100) annio4 = annio + 1900
    IF(annio.LT.80) annio4 = annio + 2000

    ! Calculate GPS week and day inside that week
    dias = -6
    IF(annio4.LE.1980) THEN
        annio4 = 1980
        GOTO 200
    ENDIF
    DO i = 1980, annio4 - 1

        IF(MOD(i, 100).NE.0.AND.MOD(i, 4).EQ.0) THEN
            dias = dias + 366
        ELSE
            IF(MOD(i, 400).EQ.0) THEN
                dias = dias + 366
            ELSE
                dias = dias + 365
            ENDIF
        ENDIF

    ENDDO
    200 CONTINUE

    dias = dias + diajul(annio4, mes, dia)
    smn = dias / 7
    dias = MOD(dias, 7)

    ! Calculate seconds elapsed from begining of GPS week
    segGPS = sec + min * 60.0D0 + hora * 3600.0D0 + dias * 3600.0D0 * 24.0D0

    RETURN
END


!       *****************************************************************
!       *****************************************************************
!       *                       FUNCTION DIAJUL                         *
!       *****************************************************************
!       *****************************************************************
!       V1.2                     By: E. Moh�no
!                                                Creation date : 03/XI/02
!                                            Last actualization:02/XII/02
!       *****************************************************************
!       Purpose: calculate julian date corresponding to a particular day
!               of month of year.
!       *****************************************************************
!       Input variables
!               -annio:year (important if it is a leap year)
!               -mes: month
!               -dia: day
!       *****************************************************************
!       Output of function: diajul: integer variable containing julian day
!       *****************************************************************
!       *****************************************************************
INTEGER FUNCTION diajul(annio, mes, dia)

    ! Declaration of variables
    IMPLICIT NONE
    INTEGER annio, mes, dia, diames(12), jdia, j
    !       annio= year
    !       mes= month
    !       dia= day
    !       diames= array with number of days for each month
    !       jdia= variable with days elapsed from begining of year
    !       j= counter
    ! End of declaration of variables

    ! Values for diames vector:
    diames(1) = 31
    diames(2) = 28
    diames(3) = 31
    diames(4) = 30
    diames(5) = 31
    diames(6) = 30
    diames(7) = 31
    diames(8) = 31
    diames(9) = 30
    diames(10) = 31
    diames(11) = 30
    diames(12) = 31

    ! Be careful if annio is a leap year
    IF (MOD(annio, 4).EQ.0.AND.MOD(annio, 100).NE.0) diames(2) = 29
    IF (MOD(annio, 400).EQ.0) diames(2) = 29

    jdia = dia
    IF (mes.LT.2) GOTO 100

    DO j = 1, mes - 1
        jdia = jdia + diames(j)
    ENDDO

    100 CONTINUE
    diajul = jdia
    RETURN
END


!       *****************************************************************
!       *****************************************************************
!       *                       SUBROUTINE POSIC                        *
!       *****************************************************************
!       *****************************************************************
!       V1.3                     By: E. Moh�no
!                                                Creation date :  28/X/02
!                                       Last modified          :03/XII/02
!       *****************************************************************
!       Purpose: calculate distance, elevation and azimuth of each satellite
!               Spherical approximation of Earth used.
!               Also calculated relativity effect, satellite clock error,
!               and tropospheric errors (if so stated in input file input.txt)
!       *****************************************************************
!       Files involved
!       Input:
!               -sat#filein: with satellite coordinates, clock errors for
!                       satelite #. Once used, these files will be
!                       deleted
!       Output:
!               -'estac'sat#.aux: 'estac' is the station name and # prn
!                       number of satellite. These files will record distance,
!                       elevation and azimuth of each satellite for each epoch.
!                       These files will be deleted in subroutine umbral.
!       *****************************************************************
!       Input Variables
!               -filein:name of precise orbits files
!               -nsat: total number of satellites available
!               -nnep: total number of observation epochs to generate
!               -xo,yo,zo: receiver coordinates (phase center antenna)
!               -estac: name of receiver
!               -iden: array of prn of satellites
!               -trer: should we generate tropospheric error? S/N
!       Output Variables
!               -lato,lono:satellite latitude and longitude
!       *****************************************************************
!       List of SUBROUTINES used:
!               calculo(xo,yo,zo,lato,lono,xs,ys,zs,dist,caz,lats,lons,
!                       azim,alt)
!       *****************************************************************
!       List of FUNCTIONS used
!               seeber2(P,temp,hr,e,xo,yo,zo,lato)
!       *****************************************************************
!       *****************************************************************


SUBROUTINE posic(filein, nsat, nnep, xo, yo, zo, estac, iden, trer&
        , lato, lono)

    ! Formats
    10 FORMAT(A)
    20 FORMAT(2A)
    30 FORMAT(I4, 4(1X, I2), 1X, F11.8/3(2X, F13.6), 3(2X, F17.8))
    40 FORMAT(A, F10.6)
    50 FORMAT(I4, 4(1X, I2), 1X, F11.8)
    60 FORMAT(3(F13.6, 1X), 9(2X, F17.8))

    ! Variable definition
    IMPLICIT NONE
    CHARACTER filein*30, iden(85)*3, filout*30, filsav*30, caz, estac*4&
            , trer*1
    !       filein=name of precise orbit file
    !       iden=array of prn of satellites
    !       filout=file whith interpolated positions of satellites
    !       filsav=output files with elevation and azimuth of satellites
    !       caz=auxiliar variable: can we calculate azimuth?
    !       estac=receiver name
    !       trer=generate trop error? S/N
    INTEGER nsat, annio, mes, dia, hora, min, i, j, nnep
    !       nsat=number of satellites
    !       annio,mes,dia,hora,min=observation epoch
    !       i,j=couters
    !       nnep=total number of observation epocs to generate
    DOUBLE PRECISION sec, xo, yo, zo, xs, ys, zs, dist, alt, azim&
            , lato, lono, lats, lons, PI, clerr, troperr, Pres, Temp, hr, rel&
            , seeber2, rot
    !       sec=seconds of observation epoch
    !       xo,yo,zo=receiver coordinates
    !       xs,ys,zs=satellite coordinates
    !       dist=satellite-receiver geometric distance
    !       alt=satellite elevation
    !       azim=satellite azimuth
    !       lato,lono=latitude and longitude of receiver
    !       lats,lons=satellite latitude and longitude
    !       PI=pi number
    !       clerr=satellite clock error
    !       trerr=tropospheric error
    !       Pres=surface pressure in mb for tropospheric error estimation
    !       Temp=surface temperature in �C for tropospheric error estimation
    !       hr= relative humidity in % for tropospheric error estimation
    !       rel=relativistic effect
    !       seeber2=function to calculate tropopheric error
    !       rot=Sagnac effect
    PARAMETER(PI = 3.141592d0)

    ! Pressure, temperature and relative humidity
    Pres = 1013.25d0
    Temp = 20.0d0
    hr = 50.0d0

    ! Verification of coordinates to avoid receiver latitude and longitude
    ! non sense (center Earth, or poles: if longitude of receiver is undefined,
    ! azimuth of satellites cannot be estimated-->caz='n').
    IF(xo.EQ.0.0.AND.yo.EQ.0.) THEN
        caz = 'n'
        lono = -999.9 * PI / 180.
        IF (zo.EQ.0.) THEN
            WRITE(*, 10) 'El punto no tiene sentido'
            GOTO 500
        ELSE
            lato = PI / 2
            IF (zo.LT.0.) lato = -lato
            WRITE(*, 40) 'Receiver latitude is ', lato * 180. / PI
            WRITE(*, 10) 'Receiver longitude is not defined'
            WRITE(*, 10) 'Satellite azimuth will not be calculated'
            DO i = 1, nsat
                filout = 'sat' // iden(i) // filein
                OPEN(16 + i, FILE = filout, STATUS = 'OLD')
                filsav = estac // 'sat' // iden(i) // '.aux'
                OPEN(16 + nsat + i, FILE = filsav)
                DO j = 0, nnep
                    READ(16 + i, 30, END = 110) annio, mes, dia, hora, min&
                            , sec, xs, ys, zs, clerr, rel, rot
                    dist = SQRT((xs - xo)**2. + (ys - yo)**2. + (zs - zo)**2.)
                    CALL calculo(xo, yo, zo, lato, lono, xs, ys, zs, dist, caz&
                            , lats, lons, azim, alt)
                    troperr = seeber2(Pres, Temp, hr, alt * 180. / PI, xo, yo, zo, lato)
                    IF(trer.EQ.'N')troperr = 0.0D0
                    WRITE(16 + i + nsat, 50) annio, mes, dia, hora, min, sec
                    dist = dist + (-clerr + troperr + rel) / 1000.
                    WRITE(16 + i + nsat, 60) dist, alt * 180. / PI, azim * 180. / PI&
                            , clerr, rel, rot, troperr, xs, ys, zs&
                            , lats * 180.0D0 / PI, lons * 180.0D0 / PI
                ENDDO

                110        CONTINUE

                !           CLOSE(16+i)
                CLOSE(16 + i, STATUS = 'DELETE')
                CLOSE(16 + nsat + i)
            ENDDO
        ENDIF
    ELSE
        ! Satellite azimuth can be estimated: we are not in center of Earth nor
        ! poles:
        caz = 'y'
        lato = atan(zo / SQRT(xo**2. + yo**2.))

        IF(xo.EQ.0.) THEN
            lono = PI / 2.
            IF(yo.LT.0.) lono = -lono
        ELSE
            lono = atan(yo / xo)
            IF(xo.LT.0.) lono = lono + PI
        ENDIF

        DO i = 1, nsat
            filout = 'sat' // iden(i) // filein
            OPEN(16 + i, FILE = filout, STATUS = 'OLD')
            filsav = estac // 'sat' // iden(i) // '.aux'
            OPEN(16 + nsat + i, FILE = filsav)
            DO j = 0, nnep
                READ(16 + i, 30, END = 210) annio, mes, dia, hora, min&
                        , sec, xs, ys, zs, clerr, rel, rot
                dist = SQRT((xs - xo)**2. + (ys - yo)**2. + (zs - zo)**2.)
                CALL calculo(xo, yo, zo, lato, lono, xs, ys, zs, dist, caz&
                        , lats, lons, azim, alt)
                WRITE(16 + i + nsat, 50) annio, mes, dia, hora, min, sec
                troperr = seeber2(Pres, Temp, hr, alt * 180. / PI, xo, yo, zo, lato)
                IF(trer.EQ.'N')troperr = 0.0D0
                dist = dist + (-clerr + troperr + rel) / 1000.
                WRITE(16 + i + nsat, 60) dist, alt * 180. / PI, azim * 180. / PI&
                        , clerr, rel, rot, troperr, xs, ys, zs&
                        , lats * 180.0D0 / PI, lons * 180.0D0 / PI
            ENDDO

            210     CONTINUE

            CLOSE(16 + i, STATUS = 'DELETE')
            !       CLOSE(16+i)
            CLOSE(16 + nsat + i)
        ENDDO
    ENDIF
    500 CONTINUE

    RETURN
END


!       *****************************************************************
!       *****************************************************************
!       *                       SUBROUTINE CALCULO                      *
!       *****************************************************************
!       *****************************************************************
!       V1                       By: E. Moh�no
!                                                Creation date:   28/X/02
!                                            Last actualization:03/XII/02
!       *****************************************************************
!       Purpose: calculate satellite latitude (lats) and longitude (lons)
!               as well as elevation and azimuth with respect to receiver
!       *****************************************************************
!       Input Variables
!               -xo,yo,zo: receiver coordinates
!               -lato: receiver latitud
!               -lono: receiver longitud
!               -xs,ys,zs: satellite coordinates
!               -dist: satellite-receiver distance
!               -caz: can we calculate azimuth?
!       Output Variables
!               -lats: satellite latitude
!               -lons: satellite longitude
!               -azim: satellite azimuth
!               -alt: satellite elevation
!       *****************************************************************
!       *****************************************************************


SUBROUTINE calculo(xo, yo, zo, lato, lono, xs, ys&
        , zs, dist, caz, lats, lons, azim, alt)

    ! Formats
    10 FORMAT(A)
    ! End formats

    ! Declaration of variables
    IMPLICIT NONE
    DOUBLE PRECISION xo, yo, zo, lato, lono, xs, ys, zs, lats, lons&
            , dist, azim, alt, PI, sinaz, cosaz, aux1
    !       xo,yo,zo=receiver coordinates
    !       lato,lono=receiver latitude and longitude
    !       xs,ys,zs=satellite coordinates
    !       lats,lons=satellite latitude and longitude
    !       dist=satellite-receiver distance
    !       azim=satellite azimuth
    !       alt=satellite elevation
    !       PI=pi number
    !       sinaz=sin of azimuth
    !       cosaz=cosin of azimuth
    !       aux1=auxiliar variable to avoid /0.
    PARAMETER(PI = 3.141592d0)
    !       PI=pi number
    CHARACTER caz
    !       caz=can we calculate azimuth?
    ! End of declaration of variables

    ! Calculate latitude of satellite: verify latitude and longitud
    ! can be estimated (not center of Earth, not in poles)
    lats = -999.9d0 * PI / 180.
    lons = -999.9d0 * PI / 180.
    alt = -999.9d0 * PI / 180.
    azim = -999.9d0 * PI / 180.

    ! Verify position of satellite not equal to receiver
    IF(dist.EQ.0.) GOTO 110

    IF (xs.EQ.0.0.AND.ys.EQ.0.) THEN
        IF(zs.EQ.0.) THEN
            WRITE(*, 10) 'Posic erronea de 1 sat'
            WRITE(*, 10) 'alt=-999.9; azim=-999.9'
            GOTO 110
        ELSE
            lats = PI / 2
            IF (zs.LT.0.) lats = -lats
            WRITE(*, 10) 'Un satelite en el polo'
            alt = asin(((xs - xo) * xo + (ys - yo) * yo + (zs - zo) * zo) / &
                    ((dist) * SQRT(xo**2. + yo**2. + zo**2.)))
            IF (caz.EQ.'y'.AND.lats.GT.0.) azim = 0.0
            IF (caz.EQ.'y'.AND.lats.LT.0.) azim = PI
            GOTO 110
        ENDIF
    ELSE
        lats = atan(zs / SQRT(xs**2. + ys**2.))
    ENDIF


    ! Calculate longitud of satellite.
    IF (xs.EQ.0.) THEN
        lons = PI / 2
        IF(ys.LT.0.) lons = -lons
    ELSE
        lons = atan(ys / xs)
        IF (xs.LT.0.) lons = lons + PI
    ENDIF

    ! Calculate elevation and azimuth, careful if it is not possible
    ! to estimate azimuth (caz='n')
    alt = asin(((xs - xo) * xo + (ys - yo) * yo + (zs - zo) * zo) / &
            ((dist) * SQRT(xo**2. + yo**2. + zo**2.)))
    aux1 = acos(sin(lats) * sin(lato) + &
            cos(lats) * cos(lato) * cos(lons - lono))
    IF (caz.EQ.'y'.AND.sin(aux1).NE.0.) THEN
        cosaz = (sin(lats) - cos(aux1) * sin(lato))&
                / (sin(aux1) * cos(lato))
        sinaz = (cos(lats)) * sin(lons - lono) / (sin(aux1))
        IF (abs(cosaz).LE.1.0) THEN
            azim = acos(cosaz)
            IF (sinaz.LT.0.) azim = 2. * PI - azim
        ELSE
            !               It could happen in extreme cases in which azimuth
            !               is close to zero and sin(aux) is very small
            azim = asin(sinaz)
            IF (cosaz.LT.0.) azim = azim + PI
        ENDIF

    ENDIF

    110 CONTINUE
    RETURN
END


!       *****************************************************************
!       *****************************************************************
!       *                       SUBROUTINE UMBRAL                       *
!       *****************************************************************
!       *****************************************************************
!       V1.3                     By: E. Moh�no
!                                                Creation date :  28/X/02
!                                           Last actualization :03/XII/02
!       *****************************************************************
!       Purpose: select those epochs in which elevation of satellite is
!               above some threshold (umb) given in degrees.
!       *****************************************************************
!       Files involved
!       Input
!               -'estac'sat#.aux: 'estac' is the station name and # prn
!                       number of satellite. These files will record distance,
!                       elevation and azimuth of each satellite for each epoch.
!                       These files will be deleted in subroutine umbral.
!       Output:
!               -'estac'satumb#.aux: (variable filsav). Same information but
!                       just for those epochs above certain threshold
!       *****************************************************************
!       Input Variables
!               -iden: array with prn satellites
!               -nsat: total number of satellites
!               -nnep: total number of epochs to generate in RINEX output file
!               -umb: elevation mask
!               -estac: name of receiver
!       Output Variables: none
!       *****************************************************************
!       *****************************************************************

SUBROUTINE umbral(iden, nsat, nnep, umb, estac)


    ! Formats
    10 FORMAT(3A, I4)
    20 FORMAT(//9X, 17A3/9X, 17A3/9X, 17A3/9X, 17A3/9X, 17A3)
    30 FORMAT(I4, 4(1X, I2), 1X, F11.8/3(F13.6, 1X), 9(2X, F17.8))
    40 FORMAT(I2, 85(A3))
    50 FORMAT(85(I2, 1X))
    ! End formats

    ! Declaracion de variables
    IMPLICIT NONE
    CHARACTER iden(85)*3, filsav*30, filout*30, estac*4
    !               iden=array with satellite prn
    !               filsav=output file with elevation above elevation mask
    !               filout=input files with data for all elevations
    !               estac=receiver name
    INTEGER annio, mes, dia, hora, min, nsat, nnep, i, j
    !               5 first variables: epoch of observation
    !               nsat=total number of available satellites
    !               nnep=total number of observation epochs
    !               i,j= counters
    DOUBLE PRECISION sec, umb, dist, alt, azim, lats, lons, xs, ys, zs&
            , troperr, rel, rot, clerr
    !               sec=seconds of observation epoch
    !               umb=elevation mask
    !               dist=satellite-receiver distance
    !               alt=satellite elevation
    !               azim=satellite azimuth
    !               lats=satellite latitude
    !               lons=satellite longitude
    !               xs,ys,zs=satellite coordinates
    !               troperr=tropospheric error
    !               rel=relativity effect
    !               rot=Sagnac effect
    !               clerr=satellite clock error
    ! End declaration variables


    ! Open each file containing information of satellite, copy epochs in
    ! which elevation above elevation mask
    DO i = 1, nsat
        filout = estac // 'sat' // iden(i) // '.aux'
        OPEN(16 + i, FILE = filout, STATUS = 'OLD')
        filsav = estac // 'satumb' // iden(i) // '.aux'
        OPEN(16 + nsat + i, FILE = filsav)

        DO j = 0, nnep
            READ(16 + i, 30, ERR = 100, END = 110) annio, mes, dia, hora, min&
                    , sec, dist, alt, azim&
                    , clerr, rel, rot, troperr, xs, ys, zs, lats, lons
            IF (alt.GT.umb) THEN
                WRITE(16 + nsat + i, 30) annio, mes&
                        , dia, hora, min, sec, dist, alt, azim&
                        , clerr, rel, rot, troperr, xs, ys, zs, lats, lons
            ENDIF

            GOTO 110
            100     WRITE(*, 10) 'Incorrect data in file:  ', filout, ' epoca #', j
            110     CONTINUE
        ENDDO

        !       CLOSE(16+i)
        CLOSE(16 + i, STATUS = 'DELETE')
        CLOSE(16 + nsat + i)
    ENDDO

    RETURN
END


!       *****************************************************************
!       *****************************************************************
!       *                       SUBROUTINE RINEX                        *
!       *****************************************************************
!       *****************************************************************
!       V2.2                     By: E. Moh�no
!                                                Creation date :  14/X/02
!                                            Last actualization:03/XII/02
!       *****************************************************************
!       Purpose: create RINEX observation file from data of satellite
!               position and different effects recorded in files.
!               This RINEX file is created according to RINEX version 2.1
!       *****************************************************************
!       Involved files
!       Input
!               -'estac'satumb#.aux: files with satellite information for
!                       all epochs in which elevation above elevation mask
!                       These files will be deleted once used.


!       Output
!               -'estac'dia_#.a�oO: RINEX observation file for receiver
!                       'esta', julian date dia, year a�o.
!       *****************************************************************
!       Input Variables
!               -fecha: date of RINEX observation file creation given in
!                       input.txt
!               -annio0,mes0,dia0,hora0,min0,sec0:initial epoch of observation
!               -nep: total number of epochs to generate
!               -nsat: total number of available satellites.
!               -iden: array with satellite prn
!               -interv:time span between consecutive observations
!               -xpos,ypos,zpos: receiver coordinates (not antenna phase center)
!               -estac: receiver name
!               -to: types of observable to generate. Maximum 5 possibilities
!               -trer: generate tropospheric error? S/N
!               -dh,de,dn: antenna phase center with respect to receiver
!       Output Variables: none
!       *****************************************************************
!       List of FUNCTIONS used
!               cdiaj(annio,mes,dia)
!       *****************************************************************
!       *****************************************************************


SUBROUTINE rinex(fecha, annio0, mes0, dia0, hora0, min0&
        , sec0, nep, nsat, iden, interv, xpos, ypos, zpos, estac&
        , to, trer, dh, de, dn)



    ! Formats
    10 FORMAT(F9.2, 11X, A20, A20, A20)
    20 FORMAT(4A20)
    30 FORMAT(A60, A20)
    40 FORMAT(A20, A40, A20)
    50 FORMAT(3F14.4, 18X, A20)
    60 FORMAT(2I6, 48X, A20)
    70 FORMAT(I6, 5(4X, A2), 24X, A20)
    80 FORMAT(F10.3, 50X, A20)
    90 FORMAT(5I6, F13.7, 5X, A3, 9X, A20)
    100 FORMAT(60X, A20)
    110 FORMAT(I4, 4(1X, I2), 1X, F11.8)
    120 FORMAT(F14.6)
    !  120 FORMAT(12(F14.6,1X))
    130 FORMAT(1X, I2.2, 4(1X, I2), F11.7, 2X, I1, I3, 12A3)
    140 FORMAT(F14.3, 1X, I1)
    145 FORMAT(5(F14.3, 2X))
    150 FORMAT(32X, 12A3)
    160 FORMAT(12(F15.8, 1X))
    ! End formats

    ! Definition of variables
    IMPLICIT NONE
    INTEGER wvfaL1, wvfaL2, nobs, annio, mes, dia, hora, min&
            , annio0, mes0, dia0, hora0, min0, nep, nsat, i, j, k&
            , satobs, annio2, flag, nok, nto, cto
    !       wvfaL1=wavelength factor for L1
    !       wvfaL2=wavelength factor for L2
    !       nobs=number of observables to generate
    !       annio,mes,dia,hora,min=observation epoch
    !       annio0,mes0,dia0,hora0,min0=initial observation epoch
    !       nep=total number of observation epochs
    !       nsat=total number of available satellites
    !       i,j,k=counters
    !       satobs=number of available satellites for one observation epoch
    !       annio2=two last digits of year
    !       flag=epoch flag. 0 means OK.
    !       nok=counter of satellite number (avoid more than 12)
    !       nto=total number of observables to generate
    !       cto=counter for generating observables
    REAL vers
    !       vers=RINEX version (2.1)
    DOUBLE PRECISION xpos, ypos, zpos, interv, sec, sec0, dist(85), tt, PI&
            , vto(5), alp(5), f1, f2, c&
            , dh, de, dn
    !       xpos,ypos,zpos=receiver approx. position
    !       interv=interval between 2 epochs
    !       sec=second of observation epoch
    !       sec0=second of initial observation epoch
    !       dist=vector with satellite-receiver distances for obs. epoch
    !       tt=elapsed seconds from initial epoch
    !       PI=pi number
    !       vto=vector with 5 observables
    !       f1,f2=carrier frequencies
    !       alp= coefficient to estimate observables
    !       c=vacuum speed of light
    !       dh,de,dn=position of antenna with respect to receiver
    CHARACTER ssss*4, ddd*3, f, yy*2, t, filenm*12, tsys*3, cdiaj*3
    !       ssss=4-character station name
    !       ddd=julian date of first observation
    !       f=number of file sequence
    !       yy=year
    !       t=type of RINEX�file (O= observation)
    !       filenm=obs RINEX file name (ssssdddf.yyt)
    !       tsys=positioning system used (GPS)
    !       cdiaj=function that calculates julian date
    CHARACTER to(5)*2, sto*2
    !       to=types of observables
    !       sto=each type of observable
    CHARACTER*20 caux1, caux2, caux3, caux4, aux
    CHARACTER*40 caux40
    CHARACTER*60 coment
    CHARACTER iden(85)*3, fecha*20, fileop*30
    CHARACTER idnval(85)*3, estac*4, trer*1
    !       caux1,2,3 and 4,aux,caux40=auxiliar variables to write RINEX file
    !       coment=comments to write in RINEX file
    !       filein=file with relevant data
    !       iden=array with prn of available satellites
    !       fecha=creation date of RINEX files
    !       fileop=file with satellite-receiver distances for sat prn #
    !       idnval=array with available satellite at one observation epoch
    !       estac=receiver name
    !       trer=generate tropospheric error? S/N
    ! End definition variables

    c = 299792458.0d0
    PI = 4.0D0 * DATAN(1.0D0)
    f1 = 1575.42D6
    f2 = 1227.60D6

    ! To generate different types of observables:
    DO cto = 1, 5
        vto(cto) = 0.0D0
        alp(cto) = 1.0D0
    ENDDO
    cto = 1
    nto = 0
    DO WHILE (cto.LE.5)
        sto = to(cto)
        IF(sto.EQ.'C1'.OR.sto.EQ.'P1'.OR.sto.EQ.'P2'
            .&
                    OR.sto.EQ.'L1'.OR.sto.EQ.'L2')THEN
            IF(sto.EQ.'L1')THEN
                alp(cto) = f1 / c
            ENDIF
            IF(sto.EQ.'L2')THEN
                alp(cto) = f2 / c
            ENDIF
            nto = cto
            cto = cto + 1
        ELSE
            cto = 6
        ENDIF
    ENDDO
    IF(nto.EQ.0)THEN
        nto = 1
        to(1) = 'C1'
    ENDIF
    IF(nto.LT.5)THEN
        DO cto = nto + 1, 5
            to(cto) = '  '
        ENDDO
    ENDIF

    IF(nto.EQ.0)THEN
        nto = 1
        to(1) = 'C1'
    ENDIF
    IF(nto.LT.5)THEN
        DO cto = nto + 1, 5
            to(cto) = '  '
        ENDDO
    ENDIF

    ! Name of observation RINEX file to create
    ssss = estac
    ddd = cdiaj(annio0, mes0, dia0)
    f = '0'
    yy = CHAR(48 + MOD(annio0, 100) / 10) // CHAR(48 + MOD(annio0, 10))
    t = 'O'
    filenm = ssss // ddd // f // '.' // yy // t
    OPEN(13, FILE = filenm)


    ! Begin writing of data into RINEX file: header section
    vers = 2.1
    caux1 = 'OBSERVATION DATA'
    caux2 = 'G (GPS)'
    caux3 = 'RINEX VERSION / TYPE'
    WRITE(13, 10) vers, caux1, caux2, caux3

    caux1 = 'SiGOGbcst v.1       '
    caux2 = '                    '
    caux3 = 'PGM / RUN BY / DATE '
    WRITE(13, 20) caux1, caux2, fecha, caux3

    coment = 'SIMULATED RINEX from BROADCASTED ORBITS'
    caux1 = 'COMMENT'
    WRITE(13, 30) coment, caux1

    IF(trer.EQ.'N')THEN
        coment = 'No tropospheric error being generated'
        caux1 = 'COMMENT'
        WRITE(13, 30) coment, caux1
    ENDIF

    coment = ssss
    caux1 = 'MARKER NAME'
    WRITE(13, 30) coment, caux1

    caux1 = 'elsa mohino'
    caux40 = 'UCM'
    caux2 = 'OBSERVER / AGENCY'
    WRITE(13, 40) caux1, caux40, caux2

    caux1 = 'X124A123'
    caux2 = 'XX'
    caux3 = 'ZZZ'
    caux4 = 'REC # / TYPE / VERS'
    WRITE(13, 20) caux1, caux2, caux3, caux4

    caux1 = '234'
    caux2 = 'YY'
    caux3 = ' '
    caux4 = 'ANT # / TYPE'
    WRITE(13, 20) caux1, caux2, caux3, caux4

    caux1 = 'APPROX POSITION XYZ'
    WRITE(13, 50) xpos, ypos, zpos, caux1

    caux1 = 'ANTENNA: DELTA H/E/N'
    WRITE(13, 50) dh, de, dn, caux1

    wvfaL1 = 1
    wvfaL2 = 1
    caux1 = 'WAVELENGTH FACT L1/2'
    WRITE(13, 60) wvfaL1, wvfaL2, caux1

    nobs = nto
    caux1 = '# / TYPES OF OBSERV'
    WRITE(13, 70) nobs, to, caux1

    caux1 = 'INTERVAL'
    WRITE(13, 80) interv, caux1

    tsys = 'GPS'
    caux1 = 'TIME OF FIRST OBS'
    WRITE(13, 90) annio0, mes0, dia0, hora0, min0, sec0, tsys, caux1

    caux1 = 'END OF HEADER'
    WRITE(13, 100) caux1



    ! Begin data section:
    flag = 0

    ! Open files with satellite-receiver distances
    DO i = 1, nsat
        fileop = estac // 'satumb' // iden(i) // '.aux'
        OPEN(16 + i, FILE = fileop, STATUS = 'OLD')
    ENDDO


    ! Which satellites were available at that precise epoch?
    DO j = 0, nep

        satobs = 0
        DO i = 1, 85
            idnval(i) = '   '
        ENDDO

        DO i = 1, nsat
            READ(16 + i, 110, END = 450, ERR = 450) annio, mes, dia, hora, min, sec
            tt = sec - sec0 + (min - min0) * 60.0 + (hora - hora0) * 3600.&
                    + (dia - dia0) * 24. * 3600.

            IF(tt.GT.j * interv) THEN
                BACKSPACE 16 + i
                dist(i) = -9.
                GOTO 500
            ELSE
                IF(tt.LT.j * interv) GOTO 400
                READ(16 + i, 120) dist(i)
                satobs = satobs + 1
                idnval(satobs) = iden(i)
                GOTO 500
            ENDIF

            400   WRITE(*, *) 'Error, could not read epoch before!:'
            WRITE(*, *) 'Satellite:', iden(i)
            READ(16 + i, *) aux
            WRITE(*, *) aux
            READ(16 + i, 110) annio, mes, dia, hora, min, sec
            tt = sec - sec0 + (min - min0) * 60.0 + (hora - hora0) * 3600.&
                    + (dia - dia0) * 24. * 3600.

            450   CONTINUE
            dist(i) = -9.

            500   CONTINUE
        ENDDO

        ! Prepare variables to write in RINEX file
        annio = annio0
        mes = mes0
        dia = dia0 + interv * j / (24. * 3600.)
        hora = hora0 + (interv * j - 24. * 3600. * (dia - dia0)) / 3600.
        min = min0 + (interv * j - 24. * 3600. * (dia - dia0) - &
                3600. * (hora - hora0)) / 60.
        sec = sec0 + (interv * j - 24. * 3600. * (dia - dia0) - &
                3600. * (hora - hora0) - (min - min0) * 60.)
        annio2 = MOD(annio, 100)


        ! Maximum number of satellites for one epoch=12
        IF(satobs.GT.12) satobs = 12

        WRITE(13, 130) annio2, mes, dia, hora, min, sec, flag, &
                satobs, (idnval(k), k = 1, 12)

        nok = 0

        DO i = 1, nsat
            IF (dist(i).NE.-9.) THEN
                nok = nok + 1
                dist(i) = dist(i) * 1000.0D0
                ! Build vto with observables:
                DO cto = 1, nto
                    vto(cto) = dist(i) * alp(cto)
                ENDDO
                IF(nok.LE.12) WRITE(13, 145) (vto(cto), cto = 1, nto)
            ENDIF
        ENDDO

    ENDDO


    ! Close files
    DO i = 1, nsat
        !      CLOSE(16+i)
        CLOSE(16 + i, STATUS = 'DELETE')
    ENDDO
    !      CLOSE(12)
    CLOSE(13)
    CLOSE(14)
    WRITE(*, *)' '
    WRITE(*, *)'********************************************'
    WRITE(*, *)'       OUTPUT RINEX OBSERVATION FILE:       '
    WRITE(*, *)filenm
    WRITE(*, *)'********************************************'
    WRITE(*, *)' '
END


!       *****************************************************************
!       *****************************************************************
!       *                       FUNCTION SEEBER2                        *
!       *****************************************************************
!       *****************************************************************
!       V1.2                      By: E. Moh�no
!                                                Creation date : 20/XI/02
!                                           Last actualization :03/XII/02
!       *****************************************************************
!       Purpose: estimate tropospheric error using modified hopfield model
!               See Seeber for a referece.
!       *****************************************************************
!       Input Variables
!               -P: surface pressure in mb
!               -temp: surface temperature in �C
!               -hr: relative humidity (%)
!               -e: elevation of satellite
!               -xo,yo,zo: position of receiver (in km)
!               -lato:receiver latitude
!       *****************************************************************
!       Function output: seeber2: tropospheric error
!       *****************************************************************
!       *****************************************************************


DOUBLE PRECISION FUNCTION seeber2(P, temp, hr, e, xo, yo, zo, lato)

    ! Declaraction variables
    IMPLICIT NONE
    DOUBLE PRECISION P, temp, T, hr, e, pv, Nd0, Nw0, hd, hw, PI, facd, facw&
            , xo, yo, zo, Re, r, h, a, ec, reli, lato, b, Ppr
    !       P=surface pressure in mb
    !       Ppr=surface pressure in mb
    !       temp=surface temperature in �C
    !       T=surface temperature in K
    !       hr=relative humidity (%)
    !       e=elevation of satellite
    !       pv=partial water vapour pressure
    !       Nd0=dry air factor
    !       Nw0=water factor
    !       hd=dry layer height
    !       hw=water layer height
    !       PI=pi number
    !       facd,facw=dry and water factors
    !       xo,yo,zo=receiver coordinates (km)
    !       Re=Earth radius
    !       r=receiver distance to Earth center
    !       h=height of receiver above Earth surface
    !       a= Earth semimajor axis
    !       ec= eccentricity of ellipsoid
    !       reli=Earth radius at receiver latitude
    !       lato=receiver latitude
    !       b=semiminor Earth axis
    ! End declaration variables

    Re = 6371.0D3
    PI = (DATAN(1.d0)) * 4.d0

    ! Earth ellipsoid
    a = 6378137.0D0
    b = 6356752.314D0
    ec = DSQRT(1 - (b / a)**2.0D0)
    reli = a * DSQRT((1 - ec**2.0D0) / (1 - (ec * DCOS(lato))**2.0D0))

    r = DSQRT(xo**2.0D0 + yo**2.0D0 + zo**2.0D0) * 1.0D3
    ! receiver heihgt (m)
    h = r - reli

    ! Temperature in K
    T = temp + 273.15d0

    ! New hidrostatic pressure
    Ppr = P * DEXP(-9.81D0 * h / (T * 287.0D0))

    pv = hr / 100. * DEXP(-37.2465 + .213166 * T - .256908d-3 * T**2)
    IF(pv.GT.1.0d0) pv = pv / 100.d0

    ! Hopfield modified model:
    hd = 40136.0d0 + 148.72 * (T - 273.15)
    hw = 11.0d3

    Nd0 = 155.2d-7 * hd * P / T
    Nw0 = 1.0D-6 * hw / 5 * (-12.96D0 * T + 3.718D5) * pv / T**2.0D0

    ! Modification to take into account receiver height
    Nd0 = Nd0 * ((hd - h) / hd)**5.0D0
    Nw0 = Nw0 * ((hw - h) / hw)**5.0D0

    IF(h.GT.hd) Nd0 = 0.0D0
    IF(h.GT.hw) Nw0 = 0.0D0

    ! Geometric factors
    facd = 1.0 / sin(dsqrt((e * e + 6.25)) * PI / 180.)
    facw = 1.0 / sin(dsqrt((e * e + 2.25)) * PI / 180.)
    seeber2 = Nd0 * facd + Nw0 * facw
    RETURN
END


!       *****************************************************************
!       *****************************************************************
!       *                       SUBRoUTINe INTORB                       *
!       *****************************************************************
!       *****************************************************************
!       V1.0                     By: E. Moh�no
!                                                Creation date:10/XII/02
!                                               Last modified :18/XII/02
!       *****************************************************************
!       Purpose: generate coordinates of satellite #, clock error and
!               relativistic effect from ephemeris
!       *****************************************************************
!       Files involved
!       Input
!               -sat0#filein: ephemeris of satellite prn #. This file
!                       will be deleted after use.
!       Salida:
!               -sat#filein: record satellite coordinates, clock error
!                       and relativistic effect for each epoch of observation
!       *****************************************************************
!       Input Variables
!               -filein:name of Rinex navigation file
!               -sat:prn of satellite
!               -nep:total number of epochs registered in RINEX navigation
!                       file
!               -annio0:year of initial epoch
!               -mes0:month of initial epoch
!               -dia0:day of initial epoch
!               -hora0:hour of initial epoch
!               -min0:minute of initial epoch
!               -sec0:second of initial epoch
!               -nnep:total number of observation epochs to generate
!               -inter:time interval between observations
!               -xo,yo,zo: receiver (center phase antenna) coordinates
!       Output Variables: none
!       *****************************************************************
!       List of SUBROUTINES used
!                posorb(t,toe,m0,sqrta,dn,e,omp,om0,w0,cuc,cus,crc,crs,
!                       ,cic,cis,i0,ip,x,y,z,rel)
!       *****************************************************************
!       *****************************************************************


SUBROUTINE intorb(filein, sat, nep, annio0, mes0, dia0, hora0, min0&
        , sec0, nnep, inter, xo, yo, zo)

    ! Formats
    10 FORMAT(3X, I2.2, 4(1X, I2), F5.1, 3(D19.12))
    20 FORMAT(22X, 3(D19.12))
    30 FORMAT(3X, 4(D19.12))
    40 FORMAT(3X, D19.12)
    50 FORMAT(41X, D19.12)
    60 FORMAT(A)
    70 FORMAT(I4, 1X, 4(I2, 1X), F11.8)
    80 FORMAT(3(2X, F13.6), 3(2X, F17.8))


    ! Definition of variables
    IMPLICIT NONE
    CHARACTER filein*30, file1*30, sat*3, salto*1
    !       filein=name of Rinex navigation file
    !       file1=name of file with ephemeris of satellite # sat
    !       sat=satellite prn
    !       salto=auxiliar variable to skip lines
    INTEGER nep, annio0, mes0, dia0, hora0, min0, nnep, j, k, annios, mess&
            , dias, horas, mins, annio, mes, dia, hora, min, ntoc
    !       nep=total number of epochs registered in RINEX navigation
    !                       file
    !       annio0=year of initial epoch
    !       mes0=month of initial epoch
    !       dia0=day of initial epoch
    !       hora0=hour of initial epoch
    !       min0=minute of initial epoch
    !       nnep=total number of observation epochs to generate
    !       j,k=counter
    !       annios=year of epoch recorded in file1
    !       mess=month of epoch recorded in file1
    !       dias=day of epoch recorded in file1
    !       horas=hour of epoch recorded in file1
    !       mins=minute of epoch recorded in file1
    !       annio,mes,dia,hora,min=observation epoch
    !       ntoc=subindex of the nearest ephemeris epoch to observation epoch
    DOUBLE PRECISION sec0, inter, t, t0, x, y, z, clerr, toc(48), secs, sec&
            , a0(48), a1(48), a2(48)&
            , crs(48), dn(48), m0(48)&
            , cuc(48), e(48), cus(48), sqrta(48)&
            , toe(48), cic(48), om0(48), cis(48)&
            , i0(48), crc(48), w0(48), omp(48), ip(48)&
            , tgd(48), aux1, aux2, aux3, aux4, segGPS&
            , rel, dif, c&
            , tau, umbral, xpr, ypr&
            , dist, disant, WE, xo, yo, zo
    !       sec0=seconds of initial epoch
    !       inter=time interval between epochs
    !       t=time elapsed in seconds since begining of GPS week
    !       t0=time of first epoch, elapsed in seconds since begining of GPS week
    !       x,y,z=satellite coordinates at time t
    !       clerr=satellite clock error at time t
    !       toc=array of times of clock in seconds since begining of GPS week
    !       secs=seconds of epoch of ephemeris in file1
    !       sec=seconds of observation epoch
    !       a0=array of coeficients a0 for each ephemeris epoch (clock error)
    !       a1=array of coeficients a1 for each ephemeris epoch (clock error)
    !       a2=array of coeficients a2 for each ephemeris epoch (clock error)
    !       crs=array of amplitude of sine armonic correction term to the
    !                 orbit radius
    !       dn=array of rate of angular velocity of satellite (n)
    !       m0=array of mean keplerian orbit anomaly
    !       cuc=array of amplitude of cosine armonic correction term to the
    !                       argument of perigee
    !       e=array of orbit eccentricity
    !       cus=array of amplitude of sine armonic correction term to the
    !             argument of perigee
    !       sqrta=array of sqrt of semimajor orbit axis
    !       toe=array of times of reference
    !       cic=array of amplitude of cosine armonic correction term to the
    !                       angle of inclination
    !       om0=array of right ascention at toe
    !       cis=array of amplitude of sine armonic correction term to the
    !                       angle of inclination
    !       i0=array of inclination angle at reference time
    !       crc=array of amplitude of cosine armonic correction term to the
    !                       orbit radius
    !       w0=array of argument of perigee at toe
    !       omp=array of rate of chang of right ascention
    !       ip=array  of rate of inclination angle
    !       tgd=array of satellite clock error at toe
    !       aux1,aux2,aux3,aux4=auxiliar variables to read file1
    !       segGPS=function that calculates time in seconds elapsed since
    !               begining of GPS week
    !       rel=relativity correction
    !       dif=to calculate closest ephemeris epoch to observation epoch
    !       c=vacuum light speed
    !       tau=time elapsed from satellite emision and receiver reception
    !       umbral=threshold for convergence in tau calculus
    !       xpr,ypr=coordinates including Sagnac effect
    !       dist=satellite-receiver distance
    !       distant=satellite-receiver distance in previous step
    !       WE=rotational angular velocity of Earth
    !       xo,yo,zo=receiver coordinates
    PARAMETER(c = 299792458.0D0, WE = 7.2921151467D-5)


    ! Fix convergence threshold in meters
    umbral = 1.0D-6

    ! Open file1 and read relevant information
    file1 = 'sat0' // sat // filein
    OPEN(13, FILE = file1)

    DO j = 1, nep
        READ(13, 10)annios, mess, dias, horas, mins, secs, aux2, aux3, aux4
        toc(j) = segGPS(annios, mess, dias, horas, mins, secs)
        a0(j) = aux2
        a1(j) = aux3
        a2(j) = aux4
        READ(13, 20)aux2, aux3, aux4
        crs(j) = aux2
        dn(j) = aux3
        m0(j) = aux4
        READ(13, 30)aux1, aux2, aux3, aux4
        cuc(j) = aux1
        e(j) = aux2
        cus(j) = aux3
        sqrta(j) = aux4
        READ(13, 30)aux1, aux2, aux3, aux4
        toe(j) = aux1
        cic(j) = aux2
        om0(j) = aux3
        cis(j) = aux4
        READ(13, 30)aux1, aux2, aux3, aux4
        i0(j) = aux1
        crc(j) = aux2
        w0(j) = aux3
        omp(j) = aux4
        READ(13, 40)aux1
        ip(j) = aux1
        READ(13, 50)aux3
        tgd(j) = aux3
        READ(13, 60)salto

    ENDDO
    CLOSE(13, STATUS = 'DELETE')

    ! Calculate observation epochs, for each choose data used to interpolate,
    ! goto posorb and obtain x,y,z and clock error and group delay
    ! Not taking into account crossovers for clock correction because we
    ! will not accept data so far away for the calculations (if  t-toc>302400)
    file1 = 'sat' // sat // filein
    OPEN(13, FILE = file1)

    ! Observation epochs:
    DO j = 0, nnep
        t0 = segGPS(annio0, mes0, dia0, hora0, min0, sec0)
        t = j * inter
        annio = annio0
        mes = mes0
        dia = dia0
        hora = hora0 + t / 3600
        min = min0 + (t - (hora - hora0) * 3600) / 60
        sec = sec0 + t - (hora - hora0) * 3600 - (min - min0) * 60
        t = t + t0

        ! Nearest toc to t?:
        ntoc = 1
        dif = abs(t - toc(1))
        DO k = 2, nep
            IF(abs(t - toc(k)).LT.dif) THEN
                dif = abs(t - toc(k))
                ntoc = k
            ENDIF
        ENDDO
        IF(abs(t - toc(ntoc)).GT.7200.0D0) GOTO 500
        WRITE(13, 70)annio, mes, dia, hora, min, sec

        ! Recursive process to calculate tau--> coorrect by tau and Sagnac effect
        k = ntoc
        tau = 0.0D0
        dif = 9.9D1
        dist = 0.0D0

        200   CONTINUE
        disant = dist

        ! Estimate satellite coordinates in geocentric system:
        CALL posorb(t - tau, toe(ntoc), m0(ntoc), sqrta(ntoc), dn(ntoc)&
                , e(ntoc), omp(ntoc), om0(ntoc), w0(ntoc), cuc(ntoc), cus(ntoc)&
                , crc(ntoc), crs(ntoc), cic(ntoc), cis(ntoc), i0(ntoc), ip(ntoc)&
                , x, y, z, rel)

        ! Change reference system (Sagnac effect)
        xpr = x * DCOS(WE * tau) + y * DSIN(WE * tau)
        ypr = -x * DSIN(WE * tau) + y * DCOS(WE * tau)

        ! New satellite-receiver distance and threshold
        dist = DSQRT((xpr - xo * 1.0D3)**2.0D0 + (ypr - yo * 1.0D3)**2.0D0&
                + (z - zo * 1.0D3)**2.0D0)
        tau = dist / c
        dif = abs(dist - disant)

        IF(dif.GT.umbral) GOTO 200

        ! a) Correcting by polinomic errors
        clerr = a0(ntoc) + a1(ntoc) * (t - toc(ntoc))&
                + a2(ntoc) * (t - toc(ntoc))**2.0D0

        ! b) Correction for using L1. Normaly small values (<0.6D-8),
        ! in meters 1.8m
        !        clerr=clerr-tgd(ntoc)
        ! Not using them because it seems GPSpace doesn't correct by tgd(ntoc)

        ! Change coordinate units to km and clock error to meters
        xpr = xpr / 1.0D3
        ypr = ypr / 1.0D3
        z = z / 1.0D3
        clerr = clerr * c

        ! Record data for its use in subroutine posic
        WRITE(13, 80) xpr, ypr, z, clerr, rel&
                , (DSQRT((xpr - xo)**2 + (ypr - yo)**2 + (z - zo)**2)&
                - DSQRT((x / 1.0D3 - xo)**2 + (y / 1.0D3 - yo)**2&
                        + (z - zo)**2)) * 1.0D3

        500   CONTINUE
    ENDDO
    CLOSE(13)

    RETURN
END