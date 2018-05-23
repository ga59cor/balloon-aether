C   Simulated GPS Observations Generator (SiGOG)                        
C
C   (c) Copyright 2004 Elsa Mohino <emohino@fis.ucm.es>,
C   Universidad Complutense de Madrid, Facultad de Ciencias Físicas
C
C   This program is free software; you can redistribute it and/or
C   modify it under the terms of the GNU General Public License as
C   published by the Free Software Foundation; either version 2 of
C   the License, or (at your option) any later version.
C
C   The author does NOT admit liability nor provide warranty for any
C   of this software. This material is provided "AS-IS" in the hope
C   that it may be useful for others.

C       *****************************************************************
C       *****************************************************************
C       *                       PROGRAM SiGOGprcs                       *
C       *****************************************************************
C       *****************************************************************
C       V2.2                     by: E. Mohino
C                                                Creation date : 11/XI/02
C                                            Last actualization: 28/IV/03
C       *****************************************************************
C       Purpose: This programme will generate a RINEX observation file
C               given the location of observer and period of observation
C               from precise orbits.
C       *****************************************************************
C       Family of files involved:
C       Input: 
C               -input.txt (Input data file)
C               -files with precise orbits for the previous, next and 
C                                       actual date
C       Output:         
C               -rinex file
C               -s//igs//weekday.sp3 (files with precise orbits "clean"
C                       defects
C       Intermediate:   several families(names depend on station)
C               -aux1//igsweekday.sp3:auxiliar files to clean orbits from
C                       problematic satellites.
C               -aux2//igsweekday.sp3:auxiliar files to clean orbits from
C                       problematic satellites.
C               -sat#.aux family (satellite position for each prn# and
C                       observation epoch
C               -"esta"sat#.aux family (distance, elevation and azimuth of satellite
C                       prn # for each observation epoch. "esta" station's name)
C               -"esta"sat#.aux family (distance, elevation and azimuth of satellite
C                       prn # for each observation epoch for which elevation is above
C                       threshold)
C       *****************************************************************
C       List of SUBROUTINES used by this program:
C               limpia(filein,nnsat,niden,nep)
C               copia(filein,filout)
C               elimina(filein,filout,sateli)
C               delete(filein)
C               lectura(files,annio0,mes0,dia0,hora0,min0,sec0,inter,nep,
C                       nnep,iden,nsat,xo,yo,zo)
C               escribe(filout,annio,mes,dia,hora0,min0,sec0,inter,auxt,
C                       auxx,auxy,auxz,l,auxct,auxc,m,nep,nnep,xo,yo,zo)
C               lagpol(t,x,n,tt,xt,xp)
C               posic(filein,nsat,nnep,xo,yo,zo,estac,iden,lato,lono)
C               calculo(xo,yo,zo,lato,lono,xs,ys,zs,dist,caz,lats,lons,
C                       azim,alt)
C               umbral(iden,nsat,nnep,umb,estac)
C               rinex(fecha,annio0,mes0,dia0,hora0,min0,sec0,nep,
C                    nsat,iden,interv,xpos,ypos,zpos,estac
C                    ,dh,de,dn)
C               antena(xo,yo,zo,dh,x,y,z)
C       *****************************************************************
C       List of FUNCTIONS used by this programme:
C               diajul(annio,mes,dia)
C               seeber2(P,temp,hr,e,xo,yo,zo,lato)
C               cdiaj(annio,mes,dia)
C       *****************************************************************
C       *****************************************************************


      PROGRAM SiGOGprcs

C Formats
   10 FORMAT(A)
   20 FORMAT(2X,3(F14.7,1X))
   30 FORMAT(2X,A4,1X,5A2)
   32 FORMAT(2X,A20)
   40 FORMAT(2X,F4.1,1X,A1)
   50 FORMAT(2X,I4,4(1X,I2),1X,F11.8)
   60 FORMAT(2X,2(I2,1X),F11.8)
   70 FORMAT(2X,F8.2)
C Fin formats


C Declaration of variable
      IMPLICIT NONE
      INTEGER annioi,mesi,diai,horai,mini,horaf,minf,i,dias,smn
     1  ,diajul,smna,smnp,dant,dpost,nsat,nep,nnep
C               annioi,mesi,diai,horai,mini: date & initial epoch of observation
C               horaf,minf: final epoch of observation
C               i:counter
C               dias,smn:GPS week and day in that week for date of observation
C               diajul:function that calculates julian day given year, month and day
C               smna:GPS week for date previous to observation date
C               smnp:GPS week for date next to observation date
C               dant:day for date previous to observation date
C               dpost:day for date next to observation date
C               nsat:total number of satellites in order for observation day
C               nep:total number of epochs for which there are satellite positions
C               nnep:total number of epochs to observe
      DOUBLE PRECISION seci,secf,inter,xo,yo,zo,umb,dnnep
     *          ,lato,lono,dh,de,dn,xor,yor,zor
C               seci:seconds of initial epoch
C               secf:seconds of final epoch
C               inter: inteval between consecutive observations
C               xo,yo,zo:antenna position of receiver in km
C               umb:elevation threshold (elevation mask)
C               dnnep:auxiliar variable for calculus of integer number of epochs
C               lato:observation position latitude 
C               lono:observation position longitude
C               dh,de,dn: antenna position from observer
C               xor,yor,zor: observer position (NOT antenna)
      CHARACTER aux2*2,files(3)*30,iden(85)*3,estac*4,fecha*20
     *          ,to(5)*2,trer*1,files2(3)*30
C               aux2:auxiliar variable to detect relevant information in input file
C               files:array with names of precise orbit files prior cleaning
C               files2:array with names of precise orbit files after cleaning
C               iden:arrya with prn of satellites
C               estac:observation station name
C               fecha:date of creation of RINEX observation file
C               to:types of observable to generate: C1P1P2L1L2
C               trer: calculate tropospheric error? S/N
C End of declaration of variables



C Reading input
      OPEN(10,FILE='input.txt')

   99 CONTINUE

C 1) Receiver coordinates (km)
  100 CONTINUE
        READ(10,10,END=500)aux2
      IF(aux2.NE.'**')GOTO 100
      BACKSPACE 10
      READ(10,20) xor,yor,zor
                        
C 2) Station name and observable to generate (C1,P1,P2,L1,L2)
  110 CONTINUE
        READ(10,10)aux2
      IF(aux2.NE.'**')GOTO 110
      BACKSPACE 10
      READ(10,30) estac,to

C 3) Elevation mask in degrees
  120 CONTINUE
        READ(10,10)aux2
      IF(aux2.NE.'**')GOTO 120
      BACKSPACE 10
      READ(10,40) umb,trer

C 4) Date and initial epoch of observation
  130 CONTINUE
        READ(10,10)aux2
      IF(aux2.NE.'**')GOTO 130
      BACKSPACE 10
      READ(10,50) annioi,mesi,diai,horai,mini,seci

C 5) Final epoch
  140 CONTINUE
        READ(10,10)aux2
      IF(aux2.NE.'**')GOTO 140
      BACKSPACE 10
      READ(10,60) horaf,minf,secf

C 6) Interval between observations
  150 CONTINUE  
        READ(10,10)aux2
      IF(aux2.NE.'**')GOTO 150
      BACKSPACE 10
      READ(10,70)inter

C 7) Date of creation of RINEX observation file
  160 CONTINUE  
        READ(10,10)aux2
      IF(aux2.NE.'**')GOTO 160
      BACKSPACE 10
      READ(10,32)fecha

C 8) Position of antenna from receiver
  170 CONTINUE
        READ(10,10)aux2
      IF(aux2.NE.'**')GOTO 170
      BACKSPACE 10
        READ(10,'(8X,3F8.4)')dh,de,dn
C Finished reading input file

C Coordinates of antenna:
      CALL antena(xor*1.0D3,yor*1.0D3,zor*1.0D3,dh,de,dn,xo,yo,zo)
        xo=xo/1.0D3
        yo=yo/1.0D3
        zo=zo/1.0D3

C GPS week and day for the date of observation
      dias=-6
      IF(annioi.GT.1980) THEN
       DO i=1980,annioi-1
      
        IF(MOD(i,100).NE.0.AND.MOD(i,4).EQ.0) THEN
          dias=dias+366
        ELSE
          IF(MOD(i,400).EQ.0) THEN
            dias=dias+366
          ELSE
            dias=dias+365
          ENDIF
        ENDIF

       ENDDO
      ELSE
       IF(annioi.LT.1980) WRITE(*,10) 'Anno anterior a epocas GPS,
     *    se tomara como anno 1980)'
        annioi=1980
      ENDIF 
      dias=dias+diajul(annioi,mesi,diai)
      smn=dias/7
      dias=MOD(dias,7)
      
C      WRITE(*,*) 'GPS week and day in that week:',smn,dias


C Precise orbit files to open: previous and following dates
      smna=smn
      smnp=smn
      dant=dias-1
      dpost=dias+1
      IF(dias.EQ.0)THEN
       smna=smna-1
       dant=6
      ENDIF
      IF(dias.EQ.6) THEN
       smnp=smnp+1
       dpost=0
      ENDIF

C Previous date
      files(1)='igs'//CHAR(48+smna/1000)//
     1 CHAR(48+MOD(smna,1000)/100)//
     2 CHAR(48+MOD(smna,100)/10)//
     3 CHAR(48+MOD(smna,10))//
     4 CHAR(48+dant)//'.sp3'

C Actual observation date
      files(2)='igs'//CHAR(48+smn/1000)//
     1 CHAR(48+MOD(smn,1000)/100)//
     2 CHAR(48+MOD(smn,100)/10)//
     3 CHAR(48+MOD(smn,10))//
     4 CHAR(48+dias)//'.sp3'

C Following day
      files(3)='igs'//CHAR(48+smnp/1000)//
     1 CHAR(48+MOD(smnp,1000)/100)//
     2 CHAR(48+MOD(smnp,100)/10)//
     3 CHAR(48+MOD(smnp,10))//
     4 CHAR(48+dpost)//'.sp3'

C Checking orbit data quality. Subroutine limpia will "clean" orbit files
C from satellites that do not reach certain quality. Subroutine limpia 
C generates "clean" orbit files, named with an "s" before actual igs name.

      CALL limpia(files,nsat,iden,nep)


C We shall now work with the following "clean" orbit files:
      DO i=1,3
        files2(i)='s'//files(i)
      ENDDO



C Reading orbit information. Interpolation of data. Satellite position 
C calculation for epoch t-tau. Satellite clock error estimation.
C Information recorded in files sat//input-orbit-file.

      nnep=(horaf-horai)*3600+(minf-mini)*60+(secf-seci)
      dnnep=(horaf-horai)*3600.+(minf-mini)*60.+(secf-seci)
      nnep=nnep/inter

      CALL lectura(files2,annioi,mesi,diai,horai,mini,seci,inter
     *,nep,nnep,iden,nsat,xo,yo,zo)

C Calculating azimuths and elevations, as well as tropospheric errors, once
C known elevation of each satellite

      CALL posic(files2(2),nsat,nnep,xo,yo,zo,estac,iden,lato,lono,trer)


C Filtering data with elevation mask given by umb
      CALL umbral(iden,nsat,nnep,umb,estac)

C Finally, generating RINEX observation file:
      CALL rinex(fecha,annioi,mesi,diai,horai,mini,seci
     1          ,nnep,nsat,iden,inter,xor*1000.0d0
     2          ,yor*1000.0d0,zor*1000.0d0,estac
     3          ,to,trer,dh,de,dn)

C Go back to begining and check if there are more RINEX observation files
C to create.
      GOTO 99

  500 CONTINUE
      CLOSE(10)


      END



C       *****************************************************************
C       *****************************************************************
C       *                       SUBROUTINE ANTENA                        *
C       *****************************************************************
C       *****************************************************************
C       V1.1                     By: E. Mohíno
C                                                Creation date:   25/X/04
C                                            Last actualization:  25/X/04
C       *****************************************************************
C       Purpose:
C               This subroutine will include antenna position referred to
C               observators position in order to calculate RINEX observation
C               files referred to phase center antenna. In a first aproach
C               it will only consider antenna height displacement.
C       *****************************************************************
C       Input Variables
C               -xo,yo,zo: observator position in METERS in cartesian 
C                       ECEF frame
C               -dh,de,dn: antenna position referred to receiver position
C                       in METERS.
C       Output Variables 
C               -x,y,z: phase center antenna position in cartesian ECEF reference
C                       system in METERS.
C       *****************************************************************
C       *****************************************************************

      SUBROUTINE antena(xo,yo,zo,dh,de,dn,x,y,z)

C This subroutine is based in ecuations from Seeber (1993), page 19 and following.
C A similar procedure can be seen in Hofmann-Wellenhof, pp255 and following.

C Declaration of variables
      IMPLICIT NONE
      DOUBLE PRECISION xo,yo,zo,dh,dn,de,x,y,z,a,e2
     *          ,lat,p,N,h,hant,lon,PI,dlon,dlat
C               xo,yo,zo:position of receiver
C               dh,de,dn: position of antenna referred to receiver
C               x,y,z:position of antenna in ECEF reference system
C               a:Earth semimajor axis of ellipsoid
C               e2:first numerical eccentricity
C               lat:elipsoidical latitude of antenna phase center
C               p:radius of ecuatorial proyection of antenna phase center
C               N:radius of curvature in prime vertical
C               h:height of antena phase center
C               hant:height of antena phase center in previous step
C               lon:elipsoidical longitude of antena phase center
C               PI:pi number
C               dlat:elipsoidical latide difference between receiver and antenna
C               dlon:elipsoidical longitude difference between receiver and antenna
      PARAMETER(a=6378137.0D0,e2=6.69437999013D-3)
      INTEGER cont
C               cont: counter
C End declaration variables

        PI=4.0D0*DATAN(1.0D0)

      p=DSQRT(xo**2.0D0+yo**2.0D0)
      h=0.0D0
      N=1.0D0
        cont=0
  100 CONTINUE
        cont=cont+1
      hant=h
      lat=DATAN(zo/(p*(1-e2*N/(N+h))))
      N=a/DSQRT(1-e2*(DSIN(lat))**2.0D0)
      h=p/DCOS(lat)-N
      IF(ABS(hant-h).GT.1.0D-6) GOTO 100

        lon=DATAN(yo/xo)
        if(xo.LT.0.0D0)lon=lon+PI

        dlat=dn/(N+h)
        dlon=de/(N*DCOS(lat))

      x=(N+h+dh)*DCOS(lat+dlat)*DCOS(lon+dlon)
      y=(N+h+dh)*DCOS(lat+dlat)*DSIN(lon+dlon)
      z=((1-e2)*N+h+dh)*DSIN(lat+dlat)

C      x=(N+h+dh)*DCOS(lat)*DCOS(lon)
C      y=(N+h+dh)*DCOS(lat)*DSIN(lon)
C      z=((1-e2)*N+h+dh)*DSIN(lat)

      RETURN
      END



C       *****************************************************************
C       *****************************************************************
C       *                       FUNCTION DIAJUL                         *
C       *****************************************************************
C       *****************************************************************
C       V1.2                     By: E. Mohíno
C                                                Creation date : 03/XI/02
C                                            Last actualization:02/XII/02
C       *****************************************************************
C       Purpose: calculate julian date corresponding to a particular day
C               of month of year.
C       *****************************************************************
C       Input variables 
C               -annio:year (important if it is a leap year)
C               -mes: month
C               -dia: day
C       *****************************************************************
C       Output of function: diajul: integer variable containing julian day
C       *****************************************************************
C       *****************************************************************

      INTEGER FUNCTION diajul(annio,mes,dia)
      
C Declaration of variables
      IMPLICIT NONE
      INTEGER annio,mes,dia,diames(12),jdia,j
C       annio= year
C       mes= month
C       dia= day
C       diames= array with number of days for each month
C       jdia= variable with days elapsed from begining of year
C       j= counter
C End of declaration of variables

C Values for diames vector:
      diames(1)=31
      diames(2)=28
      diames(3)=31
      diames(4)=30
      diames(5)=31
      diames(6)=30
      diames(7)=31
      diames(8)=31
      diames(9)=30
      diames(10)=31
      diames(11)=30
      diames(12)=31

C Be careful if annio is a leap year
      IF (MOD(annio,4).EQ.0.AND.MOD(annio,100).NE.0) diames(2)=29
      IF (MOD(annio,400).EQ.0) diames(2)=29
  
      jdia=dia
      IF (mes.LT.2) GOTO 100

      DO j=1,mes-1
       jdia=jdia+diames(j)
      ENDDO

  100 CONTINUE
      diajul=jdia
      RETURN
      END





C       *****************************************************************
C       *****************************************************************
C       *                       SUBROUTINE LIMPIA                       *
C       *****************************************************************
C       *****************************************************************
C       V1.1                     By: E. Mohíno
C                                                Creation date : 03/XI/02
C                                            Last actualization:02/XII/02
C       *****************************************************************
C       Purpose: This subroutine will verify data quality for precise
C               orbits. Satellites that do not fulfill certain requirements
C               will be eliminated.
C       *****************************************************************
C       Files involved
C       Input files
C               -filein:array with names of the 3 precise orbit files to 
C                       analyze
C       Output:
C               -s//filein:The same files but "clean" from problematic 
C                       satellites. Name is equal as in filein but for 
C                       an "s" at the begining.
C       Intermediate: two families just to eliminate problematic satellites.
C               -aux1//filein
C               -aux2//filein
C       *****************************************************************
C       Input variables
C               -filein:array with names of the 3 precise orbit files to
C                       analyze
C       Output variables:
C               -nnsat:number of satellites in output files
C               -niden:prn numbers of those satellites
C               -nep:total number of epochs including the 3 days of data
C       *****************************************************************
C       List of SUBROUTINES used
C               copia(filein,filout)
C               elimina(filein,filout,sateli)
C               delete(filein)
C       *****************************************************************
C       *****************************************************************



      SUBROUTINE limpia(filein,nnsat,niden,nep)

C Formats
   20 FORMAT (//4X,I2,3X,17(A3)/9X,17(A3)/9X
     *  ,17(A3)/9X,17(A3)/9X,17(A3))
   30 FORMAT (A1)
   40 FORMAT (1X,A3,3(F14.6),I7)
C End Formato

C Declaration Variables
      IMPLICIT NONE
      CHARACTER filein(3)*30,filout(3)*30,iden(85)*3,aux*1
     1   ,nom*3,flaux1*30,flaux2*30,niden(85)*3
C       filein: array with names of files to clean
C       filout: array with names of cleaned files
C       iden:array with prn identifiers of up to 85 satellites
C       aux=auxiliar variable to locate position data
C       nom:auxiliar variable to see if a satellite is missing in file
C       flaux1,flaux2:variable with name of auxiliar files to delete 
C               problematic satellites
C       niden: array with final prn numbers of adequated satellites present
C               in output files
      INTEGER nep,k,i,j,total,nsat,datos(4,85),loc,clerr,nnsat
C       nep=total number of epochs including the 3 files
C       i,j,k=counters
C       total=variable with total number of epochs in each satellite. It will
C               be used to determine if there is a problematic satellite
C               for which there are not enough data.
C       nsat=number of satellites in each file
C       datos=matrix containing number of data for each variable (x,y,z,clockerror)
C               and each satellite (maximum 85 satellites)
C       loc=used to locate the prn of a determined satellite in iden
C       clerr=integer part of clock error, to evaluate if there is a failure in 
C               it: sp3 standar states that is compulsory to write 6 nines in
C               the integer part of this error if there is a failure.
C       nnsat=final number of satellite included in each output file once "cleaned"
      DOUBLE PRECISION x,y,z
C       x,y,z=satellite positions. Will be used to check that positions are OK
C End declaraction of variables     


      nep=0

      DO k=1,3
        filout(k)='s'//filein(k)

        DO i=1,4
         DO j=1,85
          datos(i,j)=0
         ENDDO
        ENDDO

        total=0

C Open files, read number of satelllites (nsat) and prn in them
        OPEN(15,FILE=filein(k),STATUS='OLD')
        READ(15,20) nsat,iden

C Verify for each record data is OK:
C register number of data for each variable (x,y,z,clerr) and total number
C of data that should be present, this is, total number of epochs for file.

  100   CONTINUE
        READ(15,30,END=200) aux
        IF(aux.EQ.'P') THEN
          BACKSPACE 15
          READ(15,40) nom,x,y,z,clerr
          DO i=1,nsat
            IF(iden(i).EQ.nom) loc=i
          ENDDO
         
          IF(x.NE.0.) datos(1,loc)=datos(1,loc)+1
          IF(y.NE.0.) datos(2,loc)=datos(2,loc)+1
          IF(z.NE.0.) datos(3,loc)=datos(3,loc)+1       
          IF(clerr.NE.999999) datos(4,loc)=datos(4,loc)+1       
        ELSE
          IF(aux.EQ.'*') total=total+1
        ENDIF
        GOTO 100
  200   CONTINUE
        nep=nep+total
        CLOSE(15)
      

C Evaluate missing values:

C       CRITERIA:
C       --------
C       -positions: no more that a position missing value for each file.
C       -satellite clock error: missing value must be under 10% of total.
C If any of these two upper limits for missing values is surpassed, satellite
C will be deleted

        flaux1='aux1'//filein(k)
        flaux2='aux2'//filein(k)
        CALL copia(filein(k),flaux2)

        DO j=1,nsat
        IF(datos(1,j).LT.total-1.OR.datos(2,j).LT.total-1.OR.datos(3,j)
C     1     .LT.total-1.OR.datos(4,j).LT.total-1) THEN
     1      .LT.total-1.OR.datos(4,j).LT.total-total/10) THEN

             WRITE(*,*)' '
             WRITE(*,*) 'Satellite ',iden(j),' from file ',filein(k)
             WRITE(*,*) 
     $       'showed too many missing values, we will delete it.'
             CALL copia(flaux2,flaux1)
             CALL delete(flaux2)
             CALL elimina(flaux1,flaux2,iden(j))
             CALL delete(flaux1)
          ENDIF
        ENDDO
C DO of j=1,nsat

        CALL delete(filout(k))
        CALL copia(flaux2,filout(k))
        CALL delete(flaux2)
      ENDDO
C DO of k=1,3

C We will now check if there are the very same satellites in all files.
C First we decide common prn identifiers for all files.

      OPEN(15,FILE=filout(1),STATUS='OLD')
      READ(15,20) nnsat,niden
      CLOSE(15)
     
      OPEN(15,FILE=filout(2),STATUS='OLD')
      READ(15,20) nsat,iden
      CLOSE(15)


      i=1
  300 CONTINUE
       nom=niden(i)
       DO j=1,nsat
        IF(nom.EQ.iden(j)) nom='  0'
       ENDDO
       IF(nom.NE.'  0') THEN
        WRITE(*,*)' '
        WRITE(*,*) 'Problems with satellite ',niden(i)
        DO j=1,i-1
         niden(j)=niden(j)
        ENDDO
        DO j=i+1,nnsat
         niden(j-1)=niden(j)
        ENDDO
        niden(nnsat)='  0'
        nnsat=nnsat-1
        i=i-1
       ENDIF
       i=i+1
       IF(i.GT.nnsat) GOTO 310
       GOTO 300
  310 CONTINUE

      OPEN(15,FILE=filout(3),STATUS='OLD')
      READ(15,20) nsat,iden
      CLOSE(15)
     
      i=1
  400 CONTINUE
       nom=niden(i)
       DO j=1,nsat
        IF(nom.EQ.iden(j)) nom='  0'
       ENDDO
       IF(nom.NE.'  0') THEN
        WRITE(*,*)' '
        WRITE(*,*) 'Problems with satellite ',niden(i)
        DO j=1,i-1
         niden(j)=niden(j)
        ENDDO
        DO j=i+1,nnsat
         niden(j-1)=niden(j)
        ENDDO
        niden(nnsat)='  0'
        nnsat=nnsat-1
        i=i-1
       ENDIF
       i=i+1
       IF(i.GT.nnsat) GOTO 410
      GOTO 400
  410 CONTINUE


C Deleting non common satellites:
      DO k=1,3
       OPEN(15,FILE=filout(k),STATUS='OLD')
       READ(15,20) nsat,iden
       CLOSE(15)
       IF(nsat.NE.nnsat) THEN
         DO i=1,nsat
          nom=iden(i)
          DO j=1,nnsat
           IF(iden(i).EQ.niden(j)) nom='abc'
          ENDDO
          IF(nom.NE.'abc') THEN
            WRITE(*,*) '  '
         WRITE(*,*) 'Satellite',iden(i),' is not common to all files,'
            WRITE(*,*) 'so it will be deleted from file ',filout(k)
            CALL copia(filout(k),flaux1)
            CALL delete(filout(k))
            CALL elimina(flaux1,filout(k),iden(i))
            CALL delete(flaux1)
          ENDIF 
         ENDDO
       ENDIF
      ENDDO 
      
      RETURN
      END






C       *****************************************************************
C       *****************************************************************
C       *                       SUBROUTINE COPIA                        *
C       *****************************************************************
C       *****************************************************************
C       V1.1                     By: E. Mohíno
C                                                Creation date: 03/XI/02
C                                           Last actualization:02/XII/02
C       *****************************************************************
C       Purpose: 
C               This subroutine copies file content in filein to filout.
C               As it is used for sp3 file type, it will only copy 60
C               first characters of each record line.
C       *****************************************************************
C       Files involved:
C       Input:
C               -filein:file to copy
C       Output:
C               -filout:copy of filein
C       *****************************************************************
C       Input variables:
C               -filein:name of file to copy
C               -filout:name of output file
C       Output variables: There aren't any
C       *****************************************************************
C       *****************************************************************



      SUBROUTINE copia(filein,filout)

C Formats
   10 FORMAT(A100)
C Fin formats

C Declaration of variables
      IMPLICIT NONE
      CHARACTER aux*100,filein*30,filout*30
C       aux=auxiliar variable for copying
C       filein=name of file to copy
C       filout=name of output file
C End declaration of variables

      OPEN(15,FILE=filein,STATUS='OLD')
      OPEN(16,FILE=filout)

  100 CONTINUE
        READ(15,10,END=200) aux
        WRITE(16,10) aux
      GOTO 100
      
  200 CONTINUE

      CLOSE(15)
      CLOSE(16)
      
      END



C       *****************************************************************
C       *****************************************************************
C       *                       SUBROUTINE ELIMINA                      *
C       *****************************************************************
C       *****************************************************************
C       V1.1                        By: E. Mohíno
C                                                Creation date: 03/XI/02
C                                           Last actualization:02/XII/02
C       *****************************************************************
C       Purpose: to copy sp3 file content in filein to filout deleting
C               all reference to a satellite (sateli)
C       *****************************************************************
C       Involved files:
C       Input
C               -filein: file to copy
C       Output
C               -filout:copy of filein without satellite sateli
C       *****************************************************************
C       Input Variables 
C               -filein:name of file to copy
C               -filout:name of output file
C               -sateli:prn identifier of satellite to delete
C       Output Variables : there aren't any
C       *****************************************************************
C       *****************************************************************


      SUBROUTINE elimina(filein,filout,sateli)

C Formats
   10 FORMAT (A)
   20 FORMAT (//4X,I2,3X,17(A3)/9X,17(A3)/9X,17(A3)/9X
     1          ,17(A3)/9X,17(A3)/9X,17(A3)/9X,17(A3)/9X,17(A3)/9X
     2          ,17(A3)/9X,17(A3)/9X)
   30 FORMAT(A60)
   40 FORMAT (A1,3X,I2,3X,17(A3))
   50 FORMAT (A1,8X,17(A3))
   60 FORMAT (A2,7X,17(A3))
   70 FORMAT(/////////A)
   80 FORMAT(A1,A3)
C Fin Formats

C Declaration variables
      IMPLICIT NONE
      CHARACTER filein*30,filout*30,sateli*3,iden(85)*3,aux1*1,aux2*2
     *          ,niden(85)*3,copia*60,aux3*3,prec(85)*3,nprec(85)*3
C       filein,filout=name of input-output files
C       sateli=prn of satellite to delete
C       iden(85)=array with satellites in filein
C       aux1=auxiliar variable to write in sp3 format(+) 
C       aux2=auxiliar variable to write in sp3 format(++)
C       niden(85)=array with satellite in filout(equal to iden but for sateli)
C       copia=variable to copy a whole line
C       aux3=auxiliar variable to identify lines to delete
C       prec(85)=array with precisions of satellites in input file
C       nprec(85)=array with precisions of satellites in output file
      INTEGER nsat,i,num
C       nsat=number of satellites in filein
C       i=counter
C       num=number where sateli is in iden(i)
C End declaration variables

CInicialization of variables
      num=0
      DO i=1,85
       niden(i)='  0'
       nprec(i)='  0'
      ENDDO
CEnd inicializacion


C Open filein, read number of satellites, prns and precision for each satellite
      OPEN(15,FILE=filein)
      OPEN(16,FILE=filout)

      READ(15,20) nsat,iden,prec
      REWIND 15
      DO i=1,nsat
      IF(iden(i).EQ.sateli) num=i
      ENDDO

      IF(num.EQ.0) THEN
       WRITE(*,10) 'Error in satellite introduced'
       GOTO 700
      ENDIF

C We copy the first two lines without modification
      READ(15,30) copia
      WRITE(16,30) copia
      READ(15,30) copia
      WRITE(16,30) copia

C Change iden and write niden. Change prec and write nprec
      DO i=1,num-1
       niden(i)=iden(i)
       nprec(i)=prec(i)
      ENDDO
      
      DO i=num+1,nsat
       niden(i-1)=iden(i)
       nprec(i-1)=prec(i)
      ENDDO
      
C Change following 10 lines
      aux1='+'
      WRITE(16,40) aux1,nsat-1,(niden(i),i=1,17)
      WRITE(16,50) aux1,(niden(i),i=18,34)
      WRITE(16,50) aux1,(niden(i),i=35,51)
      WRITE(16,50) aux1,(niden(i),i=52,68)
      WRITE(16,50) aux1,(niden(i),i=69,85)
      aux2='++'
      WRITE(16,60) aux2,(nprec(i),i=1,17)
      WRITE(16,60) aux2,(nprec(i),i=18,34)
      WRITE(16,60) aux2,(nprec(i),i=35,51)
      WRITE(16,60) aux2,(nprec(i),i=52,68)
      WRITE(16,60) aux2,(nprec(i),i=69,85)

C We jump following 10 lines in input file
      READ(15,70)aux1


C Copy every line except for the ones that contain information of sateli
  100 CONTINUE
      READ(15,80,END=700) aux1,aux3
      IF(aux3.NE.sateli.OR.aux1.EQ.'*') THEN
        BACKSPACE 15
        READ(15,30) copia
        WRITE(16,30) copia
      ENDIF
      GOTO 100


  700 CONTINUE

      CLOSE(16)
      CLOSE(15)

  800 CONTINUE
      END




C       *****************************************************************
C       *****************************************************************
C       *                       SUBROUTINE DELETE                       *
C       *****************************************************************
C       *****************************************************************
C       V1.1                     By: E. Mohíno
C                                                Creation date: 03/XI/02
C                                           Last actualization:02/XII/02
C       *****************************************************************
C       Purpose: delete file named in filein
C       *****************************************************************
C       Involved files
C       Input:
C               -filein:name of file to delete
C       Output: No output files
C       *****************************************************************
C       Input variables:
C               -filein:name of file to delete
C       Output variables: There aren't any
C       *****************************************************************
C       *****************************************************************


      SUBROUTINE delete(filein)

C Declaration of variables
      IMPLICIT NONE
      CHARACTER filein*30
C       filein: name of file to delete
C End of declaration of variables

      OPEN(15,FILE=filein,ERR=100)
      CLOSE(15,STATUS='DELETE')
      
      GOTO 110
  100 WRITE(*,*) 'Error while deleting file:',filein
      WRITE(*,*) 'Subroutine delete'
  110 CONTINUE
      RETURN 
      END


C       *****************************************************************
C       *****************************************************************
C       *                       SUBROUTINE LECTURA                      *
C       *****************************************************************
C       *****************************************************************
C       V2.1                     By: E. Mohíno
C                                                Creation date: 03/XI/02
C                                           Last actualization:02/XII/02
C       *****************************************************************
C       Purpose: Read position and clock error data from the 3 "cleaned"
C               precise orbit files, give it to subroutine escribe with
C               which we will calculate positions for t-tau time, Stagnac
C               effect (rotation of reference frame during signal propagation
C               time: tau), satellite clock error and relativistic effect.
C       *****************************************************************
C       Involved files
C       Input:
C               -files: "cleaned" precise orbit files
C       Output: family:
C               -sat#files: register positions, satellte clock error and 
C                       relativistic error for each epoch of observation
C                       of satellite prn #. This data is generated by
C                       subroutine escribe. Later on, in subroutine
C                       posic these files will be deleted, once the data
C                       is used.
C       *****************************************************************
C       Input Variables
C               -files:names of precise "cleaned" files
C               -annio0: year of first epoch
C               -dia0: day of first epoch
C               -hora0: hour of first epoch
C               -min0: minute of first epoch
C               -sec0: second of first epoch
C               -inter: period of time between observations
C               -nep:total number of positions recorded in igs all 3 files
C               -nnep:total number of observation epochs to generate
C               -iden: prn of satellites
C               -nsat: total number of satellites in igs files
C               -xo,yo,zo: antenna phase center position
C       Output Variables: there aren't
C       *****************************************************************
C       List of SUBROUTINES used
C               escribe(filout,annio,mes,dia,hora0,min0,sec0,
C                       inter,auxt,auxx,auxy,auxz,l,auxtc,auxc,m,nep,nnep
C                       ,xo,yo,zo)
C       List of FUNCTIONS used
C               diajul(annio,mes,dia)
C       *****************************************************************
C       *****************************************************************


      SUBROUTINE lectura(files,annio0,mes0,dia0,hora0
     *  ,min0,sec0,inter,nep,nnep,iden,nsat,xo,yo,zo)
  
C Formats:
   10 FORMAT(A)
   20 FORMAT(/////////////////////A1)
   30 FORMAT(3X,I4,4(1X,I2),1X,F11.8)
   40 FORMAT(A1,3X,4(1X,F13.6))
C End formats

C Declaration of variables
      IMPLICIT NONE
      CHARACTER files(3)*30,iden(85)*3,filout*30,aux*1
C               files=precise "clean" orbits
C               iden(85)=prn number of satellites
C               filout=output file name
C               aux=used to skip lines
      INTEGER annio0,mes0,dia0,hora0,min0,nep,nnep,nsat,i,j,k,l,m
     * ,diajul,annio,mes,dia,hora,min 
C               First 5 variables: year,month,day,hour,minute and 
C                       second of initial epoch
C               nep=total number of positions recorded in igs all 3 files
C               nnep=total number of observation epochs to generate
C               nsat= total number of satellites
C               i,j,k=counters
C               l=total amout of non-zero values in auxiliar variables
C                       auxt,auxx,auxy,auxz
C               m=total number of data in auxiliar variables auxtc,auxc
C               diajul=julian date function, used to take into account there
C                       could be a change of month between consecutive days
C               annio,mes,dia,hora,min= year,month,day,hour,minute of each epoch
      DOUBLE PRECISION sec0,inter,sat(4),sec,tt,c(2,85,300)
     1  ,t(85,300),x(85,300),y(85,300),z(85,300)
     2  ,auxx(300),auxy(300),auxz(300),auxt(300)
     3  ,auxc(300),auxtc(300),xo,yo,zo
C               sec0=seconds of initial epoch
C               inter=time interval between observations
C               sat=reads position and clock error of each satellite in each epoch
C               sec=seconds of epoch
C               tt=time in seconds elapsed between a certain epoch and the initial one
C               c=data of clock error (c(2,i,j)) and time to which it corresponds (c(1,i,j))
C                       for satellite i and epoch j
C               t,x,y,z=time and position of satellite for each epoch
C               auxt,auxx,auxy,auxz,auxc,auxtc=auxiliar variables to record epoch, position,
C                       clock errors to give to subroutine escribe that will interpolate the
C                       data for the precise epoch of observation
C               xo,yo,zo=coordinates of phase center of antenna receiver
C End declaration of variables


      DO i=1,85
       DO j=1,300
        t(i,j)=-9.3
        x(i,j)=-9.3
        y(i,j)=-9.3
        z(i,j)=-9.3
        c(1,i,j)=-9.3
        c(2,i,j)=-9.3
       ENDDO
      ENDDO


C Open 3 files and prepare them for reading data
      DO k=1,3
       OPEN(14+k,FILE=files(k),STATUS='OLD')
       READ(14+k,20) aux
      ENDDO


C Reading data from each input file. All epochs referred to initial epoch (annio0,
C mes0,..). 
      k=1
      j=1

  150 CONTINUE
        READ(14+k,30,END=200) annio,mes,dia,hora,min,sec
        tt=sec-sec0+(min-min0)*60.0+(hora-hora0)*3600.
     *  +(diajul(annio,mes,dia)-diajul(annio0,mes0,dia0))*24.*3600.

        IF(mes.EQ.12.AND.dia.EQ.31)tt=tt
     *  +(annio-annio0)*diajul(annio,12,31)*24.*3600.

        IF(mes0.EQ.12.AND.dia0.EQ.31)tt=tt
     *  +(annio-annio0)*diajul(annio0,12,31)*24.*3600.

        DO i=1,nsat
        READ(14+k,40,END=200) aux,sat   
C               If it is position data (P) and non-zero         
C               (x,y o z=0.):
            IF (aux.EQ.'P'.AND.sat(1).NE.0.0.AND.sat(2)
     *                       .NE.0.0.AND.sat(3).NE.0.0) THEN
                 t(i,j)=tt
                 x(i,j)=sat(1)
                 y(i,j)=sat(2)
                 z(i,j)=sat(3)
             ENDIF
             IF(sat(4).LT.999999.) THEN
               c(1,i,j)=tt
               c(2,i,j)=sat(4)
             ENDIF
        ENDDO

        GOTO 210
  200   CONTINUE
        IF(annio.EQ.0) j=j-1
        IF (k.LT.3) THEN
          k=k+1
        ELSE
          WRITE(*,*) 'Finished orbit files prior to nep!'
          GOTO 220
        ENDIF
  210   CONTINUE

      j=j+1
      IF(j.LE.nep) GOTO 150


  220 CONTINUE

      DO k=1,3
       CLOSE(14+k)
      ENDDO

C       El de k=1,3



C Interpolate data and generate output files

      DO i=1,nsat
       filout='sat'//iden(i)//files(2)

C How many data different from -9.3 are there?
       l=0
       m=0
       DO j=1,nep
         IF(t(i,j).NE.-9.3) THEN
            l=l+1
            auxx(l)=x(i,j)
            auxy(l)=y(i,j)
            auxz(l)=z(i,j)
            auxt(l)=t(i,j)
          ENDIF
          IF(c(1,i,j).NE.-9.3) THEN
            m=m+1
            auxtc(m)=c(1,i,j)
            auxc(m)=c(2,i,j)
          ENDIF
         ENDDO

C For each satellite we call subrutine escribe to interpolate data:
          CALL escribe(filout,annio0,mes0,dia0,hora0,min0,sec0,inter
     *          ,auxt,auxx,auxy,auxz,l,auxtc,auxc,m,nnep,xo,yo,zo)

    
      ENDDO
C       El de i=1,nsat


      END



C       *****************************************************************
C       *****************************************************************
C       *                       SUBROUTINE ESCRIBE                      *
C       *****************************************************************
C       *****************************************************************
C       V2.1                     By: E. Mohíno
C                                                Creation date: 03/XI/02
C                                           Last actualization:02/XII/02
C       *****************************************************************
C       Purpose: calculate satellite positions in t-tau, Sagnac effect,
C               satellite clock error and relativistic error. Registers
C               result in output file
C       *****************************************************************
C       Involved files
C       Input: none
C       Output: 
C               -sat#files: registers positions, clock errors and relativistic
C                       effect for satellite # for each epoch of observation.
C                       It will be deleted later on, in subrutine posic, once
C                       data is used.
C       *****************************************************************
C       Input Variables 
C               -filout:name of output file in which to record calculated data
C               -annio: year of first epoch (and all epochs)
C               -mes:month of first epoch (and all epochs)
C               -dia: day of first epoch (and all epochs)
C               -hora0: hour of first epoch
C               -min0: minute of first epoch
C               -sec0: second of first epoch
C               -inter: interval between observations
C               -auxt,auxx,auxy,auxz: auxiliar data with epochs and positions
C                       recorded in precise orbits files
C               -l:number of valid values of former arrays
C               -auxtc,auxc:auxiliar data with epochs and clock error 
C                       recorded in precise orbits files
C               -m:number of valid values of former arrays
C               -nep:total number of epochs of registered data in precise files
C               -nnep:total number of epochs of observations to generate
C               -xo,yo,zo: position of phase center of receiver
C       Output Variables: none
C       *****************************************************************
C       List of SUBROUTINES used
C               lagpol(t,x,n,tt,xt,xp)
C       *****************************************************************
C       *****************************************************************

      
      SUBROUTINE escribe(filout,annio,mes,dia,hora0,min0,sec0,inter
     *          ,auxt,auxx,auxy,auxz,l,auxtc,auxc,m,nnep,xo,yo,zo)



C Formats
   20 FORMAT(I4,1X,4(I2,1X),F11.8)
   30 FORMAT(3(2X,F13.6),3(2X,F17.8))
C End formats

C Declaration of variables
      IMPLICIT NONE
      CHARACTER filout*30
C               filout=name of output file
      INTEGER annio,mes,dia,hora0,min0,hora,min,l,m,i,k,nnep
     * ,nc,nt
C               annio,mes,dia,hora0,min0= initial epoch
C               hora,min=hours and minutes of epoch to generate
C               l= lenght of position vectors
C               m= lenght of clock error vector
C               i,k= counter
C               nnep= number of observation epochs to generate
C               nc=used to calculate the position in vector tcl
C                       of closest time to desired observation epoch
C               nt=used to calculate the position in vector t
C                       of closest time to desired observation epoch
      DOUBLE PRECISION sec0,inter,auxt(300),auxx(300),auxy(300)
     1    ,auxz(300),auxtc(300),auxc(300),t(300),x(300),y(300),z(300)
     2    ,cl(300),tcl(300),sec,tau,xt,yt,zt,tt
     3    ,dist,disant,dif,umbral,xo,yo,zo,clerr
     4    ,xant,yant,zant,x1,y1,z1
     5    ,dc,clter(4),tclter(4),difter,difer
     6    ,xspr,yspr,w,c,rel
     7    ,tnov(10),xnov(10),ynov(10),znov(10)
C               sec0= seconds of initial epoch
C               inter= interval between observation epochs
C               auxt,auxx,auxy,auxz: auxiliar data with epochs and positions
C                       recorded in precise orbits files
C               auxtc,auxc= auxiliar data with epochs and clock error
C                       recorded in precise orbits files
C               t,x,y,z=vectors of lenght l with positions and epochs to which
C                       they refer
C               cl,tcl= vectors of lenght m with clock errors and epochs to which
C                       they refer
C               sec=seconds of observation epoch to generate
C               tau=elapsed time between emision of signal at satellite and reception
C                       at receiver
C               xt,yt,zt,tt=satellite position at tt elapsed time from initial epoch
C               dist=satellite-receiver geometric distance
C               disant=satellite-receiver geometric distance in previous step
C               dif=absolute geometric distances difference between one step and the previous one
C               umbral=threshold to accept convergence
C               xo,yo,zo= receiver (antenna phase center) position
C               clerr=clock error for observationepoch
C               xant,yant,zant=previous step positions
C               x1,y1,z1= velocity of satellite for relativistic effect calculus
C               dc=error in polinomic interpolation procedure
C               clter=array for interpolation of clock error
C               tclter= array of corresponding times
C               difter,difer=auxiliar variables used to locate index corresponding to 
C                       closest time to t-tau in tcl array
C               xspr,yspr=position in reference system t-tau (to estimate Sagnac effect)
C               w=angular rotation velocity of Earth
C               c=vacuum light speed
C               rel=relativistic effect
C               tnov=vector with closest 10 values to tt for 9th degree interpolation
C               xnov=array for 9th degree polinomic interpolation
C               ynov=array for 9th degree polinomic interpolation
C               znov=array for 9th degree polinomic interpolation
      PARAMETER(c=299792458.0d0,w=7.292115d-5)
C End declaration of variables

C Open of output file
      OPEN(15,FILE=filout)

C Iteration threshold in km
        umbral=1.0D-9

C Cleaning verctors in case there were missing data
      DO i=1,l
       t(i)=auxt(i)
       x(i)=auxx(i)
       y(i)=auxy(i)
       z(i)=auxz(i)
      ENDDO
      DO i=1,m
       cl(i)=auxc(i)
       tcl(i)=auxtc(i)
      ENDDO


C Epochs to generate:
      DO i=0,nnep

       tt=i*inter
       hora=hora0+tt/3600
       min=min0+(tt-(hora-hora0)*3600)/60
       sec=sec0+tt-(hora-hora0)*3600-(min-min0)*60
       WRITE(15,20) annio,mes,dia,hora,min,sec

C Closest vector to interpolation time
        nt=0
        DO k=1,l
          IF(t(k).LT.tt.AND.t(k+1).GE.tt) nt=k
        ENDDO
        IF(nt.EQ.0) WRITE(*,*)'Problems to locate interval'
C Building vectors for interpolation
        DO k=1,10
          tnov(k)=t(nt-5+k)
          xnov(k)=x(nt-5+k)
          ynov(k)=y(nt-5+k)
          znov(k)=z(nt-5+k)
        ENDDO

C Recursive process to calculate t-tau 
        tau=0.0d0
        disant=0.0d0
        xant=0.0d0
        yant=0.0d0
        zant=0.0d0

  100  CONTINUE

        CALL lagpol(tnov,xnov,10,tt-tau,xt,x1)
        CALL lagpol(tnov,ynov,10,tt-tau,yt,y1)
        CALL lagpol(tnov,znov,10,tt-tau,zt,z1)
                

C Satellite positions in t-tau with reference to system in t:
        xspr=xt*cos(tau*w)+yt*sin(tau*w)
        yspr=-xt*sin(tau*w)+yt*cos(tau*w)

        dist=SQRT((xspr-xo)**2.+(yspr-yo)**2.+(zt-zo)**2.)
        tau=dist*1000.0D0/c
        dif=abs(dist-disant)

        disant=dist
        xant=xt
        yant=yt
        zant=zt

        IF(dif.GT.umbral) GOTO 100

C Calculate error in t-tau; interpolation with 3rd degree polinomial
      nc=1
      difter=abs(tt-tau-tcl(1))
      DO k=1,m
        difer=abs(tt-tau-tcl(k))
        IF(difer.LT.difter) THEN
          nc=k
          difter=difer
        ENDIF
      ENDDO



      DO k=1,4
        clter(k)=cl(nc-2+k)
        tclter(k)=tcl(nc-2+k)
      ENDDO
        CALL lagpol(tclter,clter,4,tt-tau,clerr,dc)
        clerr=clerr*c/1000000.0D0


C Relativistic error (-2VR/C en metros)
C All in reference system t-tau
C Units: m and m/s
        xt=xt*1000.0D0
        yt=yt*1000.0D0
        zt=zt*1000.0D0
        
        x1=x1*1000.0D0
        y1=y1*1000.0D0
        z1=z1*1000.0D0

        rel=2.*(xt*x1+yt*y1+zt*z1)/c

C Writing position (km), clock error and relativistic effect (meters)
       WRITE(15,30) xspr,yspr,zt/1000.D0,clerr,rel
     *  ,(DSQRT((xspr-xo)**2+(yspr-yo)**2+(zt/1.0D3-zo)**2)
     *  -DSQRT((xt/1.0D3-xo)**2+(yt/1.0D3-yo)**2
     *  +(zt/1.0D3-zo)**2))*1.0D3


      ENDDO

      CLOSE(15)

      END





C       *****************************************************************
C       *****************************************************************
C       *                       SUBROUTINE LAGPOL                       *
C       *****************************************************************
C       *****************************************************************
C       V1.0                     By: E. Mohíno
C                                                Creation date:10/XII/02
C                                           Last actualization:11/XII/02
C       *****************************************************************
C       Purpose: interpolate a function for a time t given values
C               x(i) in times t(i). This interpolation is made through a
C               n-1 degree Lagrange polinomy.
C       *****************************************************************
C       Involved files: none
C       *****************************************************************
C       Input Variables 
C               -t: vector of n size with time points
C               -x: vector of n size with corresponding value of function
C                       to interpolate in time points given by t
C               -n: size of input x,t
C               -tt: time point in which we want to interpolate function
C                       x and its first derivative
C       Output Variables 
C               -xt: value of interpolation in tt
C               -xp: value of first derivative in tt
C       *****************************************************************
C       *****************************************************************


      SUBROUTINE lagpol(t,x,n,tt,xt,xp)

C Definition of variables
      IMPLICIT NONE
      INTEGER n,i,j,k
C               n=size of input vectors, n-1 degree of polinomy
C               i,j,k= counters
      DOUBLE PRECISION t(30),x(30),tt,xt,xp
     1 ,p(30),pp(30),nump,numpp,denom,aux 
C       t(30)=input vector of time points 
C       x(30)=input vector of position function 
C       tt=time point in which we want to interpolate function 
C               x and its first derivative 
C       xt=value of interpolation in tt 
C       xp=value of first derivative in tt 
C       p(30)=Lagrange terms for time tt 
C       pp(30)=first derivative of Lagrangian terms 
C       nump=numerator to calculate each p(i) 
C       numpp=numerator to calculate each pp(i) 
C       denom=denominator to calculate each p(i) and pp(i)
C       aux=auxiliar variable to calculate numpp
C End definition of variables


C Check there are more than 1 point in vector
      IF(n.EQ.1) THEN
        xt=x(1)
        xp=0.0D0
        GOTO 100
      ENDIF

C Caluculating lagrangian terms
      DO i=1,n
        nump=1.0D0
        numpp=0.0D0
        denom=1.0D0
        DO j=1,n
          IF(j.NE.i) THEN
             nump=nump*(tt-t(j))
             denom=denom*(t(i)-t(j))
          aux=1.0D0
          DO k=1,n
            IF(k.NE.j.AND.k.NE.i) aux=aux*(tt-t(k))
          ENDDO
          numpp=numpp+aux
          ENDIF
        ENDDO
        IF(denom.EQ.0.0D0)THEN
         WRITE(*,*) 'Falure in lagpol'
         xt=0.0D0
         xp=0.0D0
         GOTO 100
        ENDIF
        p(i)=nump*x(i)/denom
        pp(i)=numpp*x(i)/denom
      ENDDO

C Calculating interpolation in tt
      xt=0.0D0
      xp=0.0D0
      DO i=1,n
        xt=xt+p(i)
        xp=xp+pp(i)
      ENDDO

  100 CONTINUE

      END





C       *****************************************************************
C       *****************************************************************
C       *                       SUBROUTINE POSIC                        *
C       *****************************************************************
C       *****************************************************************
C       V1.3                     By: E. Mohíno
C                                                Creation date :  28/X/02
C                                          Last actualization  :03/XII/02
C       *****************************************************************
C       Purpose: calculate distance, elevation and azimuth to each satellite
C               in each epoch with respect to a receiver. Elevation and
C               azimuth will be estimated using an espherical aproximation
C               of Earth. Relativistic effect, satellite clock error and
C               tropospheric error (if so stated in input) will be added
C               to geometric distance (that already includes Sagnac effect).
C       *****************************************************************
C       Files involved:
C       Input
C               -sat#filein: recorded satellite positions, clock error and
C                       relativistic effect of satellite num # for each
C                       epoch of observation. These files will be deleted
C                       once their data is used.
C       Output
C               -'estac'sat#.aux: 'estac' is the station name and # prn 
C                       number of satellite. These files will record distance,
C                       elevation and azimuth of each satellite for each epoch.
C                       These files will be deleted in subroutine umbral.
C       *****************************************************************
C       Input Variables 
C               -filein:name of precise orbits files
C               -nsat: total number of satellites available
C               -nnep: total number of observation epochs to generate
C               -xo,yo,zo: receiver coordinates (phase center antenna) 
C               -estac: name of receiver
C               -iden: array of prn of satellites
C               -trer: should we generate tropospheric error? S/N
C       Output Variables: none
C       *****************************************************************
C       List of SUBROUTINES used:
C               calculo(xo,yo,zo,lato,lono,xs,ys,zs,dist,caz,lats,lons,
C                       azim,alt)
C       *****************************************************************
C       List of FUNCTIONS used
C               seeber2(P,temp,hr,e,xo,yo,zo,lato)
C       *****************************************************************
C       *****************************************************************

      
      SUBROUTINE posic(filein,nsat,nnep,xo,yo,zo
     *          ,estac,iden,lato,lono,trer)

C Formats
   10 FORMAT(A)
   20 FORMAT(2A)
   30 FORMAT(I4,4(1X,I2),1X,F11.8/3(2X,F13.6),3(2X,F17.8))
   40 FORMAT(A,F10.6)
   50 FORMAT(I4,4(1X,I2),1X,F11.8)
   60 FORMAT(12(F14.6,1X))
C End  formats

C Variable definition
      IMPLICIT NONE
      CHARACTER filein*30,iden(85)*3,filout*30,filsav*30,caz,estac*4
     *          ,trer*1
C       filein=name of precise orbit file
C       iden=array of prn of satellites
C       filout=file whith interpolated positions of satellites
C       filsav=output files with elevation and azimuth of satellites
C       caz=auxiliar variable: can we calculate azimuth?
C       estac=receiver name 
C       trer=generate trop error? S/N
      INTEGER nsat,annio,mes,dia,hora,min,i,j,nnep
C       nsat=number of satellites
C       annio,mes,dia,hora,min=observation epoch
C       i,j=couters
C       nnep=total number of observation epocs to generate
      DOUBLE PRECISION sec,xo,yo,zo,xs,ys,zs,dist,alt,azim
     *  ,lato,lono,lats,lons,PI,clerr,troperr,Pres,Temp,hr,rel
     *  ,rot,seeber2
C       sec=seconds of observation epoch
C       xo,yo,zo=receiver coordinates
C       xs,ys,zs=satellite coordinates
C       dist=satellite-receiver geometric distance
C       alt=satellite elevation
C       azim=satellite azimuth
C       lato,lono=latitude and longitude of receiver
C       lats,lons=satellite latitude and longitude
C       PI=pi number
C       clerr=satellite clock error
C       trerr=tropospheric error
C       Pres=surface pressure in mb for tropospheric error estimation
C       Temp=surface temperature in ºC for tropospheric error estimation
C       hr= relative humidity in % for tropospheric error estimation
C       rel=relativistic effect
C       rot=Sagnac effect
C       seeber2=function to calculate tropopheric error
      PARAMETER(PI=3.141592d0)
C       PI=pi number
C End definition of variables

C Pressure, temperature and relative humidity
      Pres=1013.25d0
      Temp=20.0d0
      hr=50.0d0

C Verification of coordinates to avoid receiver latitude and longitude 
C non sense (center Earth, or poles: if longitude of receiver is undefined,
C azimuth of satellites cannot be estimated-->caz='n').
      IF(xo.EQ.0.0.AND.yo.EQ.0.) THEN
        caz='n'
        lono=-999.9*PI/180.
        IF (zo.EQ.0.) THEN
            WRITE(*,10) 'Receiver coordinates in Center of Earth!!!!'
            GOTO 500
        ELSE
          lato=PI/2
          IF (zo.LT.0.) lato=-lato
          WRITE(*,40) 'Receiver latitude is',lato*180./PI
          WRITE(*,10) 'Longitude not defined'
          WRITE(*,10) 'Satellite azimuth cannot be estimated'
          DO i=1,nsat
            filout='sat'//iden(i)//filein
            OPEN(16+i,FILE=filout,STATUS='OLD')
            filsav=estac//'sat'//iden(i)//'.aux'
            OPEN(16+nsat+i,FILE=filsav)
            DO j=0,nnep
               READ(16+i,30,END=100) annio,mes,dia,hora,min
     *                          ,sec,xs,ys,zs,clerr,rel,rot
               dist=SQRT((xs-xo)**2.+(ys-yo)**2.+(zs-zo)**2.)
               CALL calculo(xo,yo,zo,lato,lono,xs,ys,zs,dist,caz
     *                  ,lats,lons,azim,alt)
               troperr=seeber2(Pres,Temp,hr,alt*180./PI,xo,yo,zo,lato)
               IF(trer.EQ.'N')troperr=0.0D0
               WRITE(16+i+nsat,50) annio,mes,dia,hora,min,sec
               dist=dist+(-clerr+troperr+rel)/1000.
               WRITE(16+i+nsat,60) dist,alt*180./PI,azim*180./PI
     *          ,lats*180.0D0/PI,lons*180.0D0/PI
     *          ,xs,ys,zs
     *          ,troperr,rel,rot,clerr
            ENDDO

            GOTO 110
 100        WRITE(*,20) 'Unproper exit in file',filout
 110        CONTINUE

C           CLOSE(16+i)
            CLOSE(16+i,STATUS='DELETE')
            CLOSE(16+nsat+i)
          ENDDO
        ENDIF
      ELSE

C Satellite azimuth can be estimated: we are not in center of Earth nor
C poles:
        caz='y'
        lato=atan(zo/SQRT(xo**2.+yo**2.))

        IF(xo.EQ.0.) THEN
         lono=PI/2.
         IF(yo.LT.0.) lono=-lono
        ELSE
         lono=atan(yo/xo)
         IF(xo.LT.0.) lono=lono+PI
        ENDIF 

        DO i=1,nsat
          filout='sat'//iden(i)//filein
          OPEN(16+i,FILE=filout,STATUS='OLD')
          filsav=estac//'sat'//iden(i)//'.aux'
          OPEN(16+nsat+i,FILE=filsav)     
          DO j=0,nnep
             READ(16+i,30,END=200) annio,mes,dia,hora,min
     *          ,sec,xs,ys,zs,clerr,rel,rot
             dist=SQRT((xs-xo)**2.+(ys-yo)**2.+(zs-zo)**2.)
             CALL calculo(xo,yo,zo,lato,lono,xs,ys,zs,dist,caz
     *          ,lats,lons,azim,alt)
             WRITE(16+i+nsat,50) annio,mes,dia,hora,min,sec
             troperr=seeber2(Pres,Temp,hr,alt*180./PI,xo,yo,zo,lato)
             IF(trer.EQ.'N')troperr=0.0D0
             dist=dist+(-clerr+troperr+rel)/1000.
             WRITE(16+i+nsat,60) dist,alt*180./PI,azim*180./PI
     *          ,lats*180.0D0/PI,lons*180.0D0/PI
     *          ,xs,ys,zs
     *          ,troperr,rel,rot,clerr
          ENDDO

          GOTO 210
  200     WRITE(*,20) 'Unproper exit in file',filout
  210     CONTINUE

        CLOSE(16+i,STATUS='DELETE')
C       CLOSE(16+i)
        CLOSE(16+nsat+i)
      ENDDO
      ENDIF     
  500 CONTINUE

      END





C       *****************************************************************
C       *****************************************************************
C       *                       SUBROUTINE CALCULO                      *
C       *****************************************************************
C       *****************************************************************
C       V1                       By: E. Mohíno
C                                                Creation date:   28/X/02
C                                            Last actualization:03/XII/02
C       *****************************************************************
C       Purpose: calculate satellite latitude (lats) and longitude (lons)
C               as well as elevation and azimuth with respect to receiver
C       *****************************************************************
C       Input Variables 
C               -xo,yo,zo: receiver coordinates 
C               -lato: receiver latitud 
C               -lono: receiver longitud 
C               -xs,ys,zs: satellite coordinates
C               -dist: satellite-receiver distance
C               -caz: can we calculate azimuth?
C       Output Variables 
C               -lats: satellite latitude
C               -lons: satellite longitude
C               -azim: satellite azimuth
C               -alt: satellite elevation
C       *****************************************************************
C       *****************************************************************


      SUBROUTINE calculo(xo,yo,zo,lato,lono,xs,ys
     * ,zs,dist,caz,lats,lons,azim,alt)


C Formats
   10 FORMAT(A)
C End formats 

C Declaration of variables
      IMPLICIT NONE
      DOUBLE PRECISION xo,yo,zo,lato,lono,xs,ys,zs,lats,lons
     *                ,dist,azim,alt,PI,sinaz,cosaz,aux1
C       xo,yo,zo=receiver coordinates
C       lato,lono=receiver latitude and longitude
C       xs,ys,zs=satellite coordinates
C       lats,lons=satellite latitude and longitude
C       dist=satellite-receiver distance
C       azim=satellite azimuth
C       alt=satellite elevation
C       PI=pi number
C       sinaz=sin of azimuth
C       cosaz=cosin of azimuth
C       aux1=auxiliar variable to avoid /0.
      PARAMETER(PI=3.141592d0)
C       PI=pi number
      CHARACTER caz
C       caz=can we calculate azimuth?
C End of declaration of variables

C Calculate latitude of satellite: verify latitude and longitud
C can be estimated (not center of Earth, not in poles)
      lats=-999.9d0*PI/180.
      lons=-999.9d0*PI/180.
      alt=-999.9d0*PI/180.
      azim=-999.9d0*PI/180.

C Verify position of satellite not equal to receiver
      IF(dist.EQ.0.) GOTO 110

      IF (xs.EQ.0.0.AND.ys.EQ.0.) THEN
        IF(zs.EQ.0.) THEN
            WRITE(*,10) 'Posic erronea de 1 sat'        
            WRITE(*,10) 'alt=-999.9; azim=-999.9'
            GOTO 110
        ELSE
            lats=PI/2
            IF (zs.LT.0.) lats=-lats
            WRITE(*,10) 'Polar satellite?'
            alt=asin(((xs-xo)*xo+(ys-yo)*yo+(zs-zo)*zo)/
     *                  ((dist)*SQRT(xo**2.+yo**2.+zo**2.)))
            IF (caz.EQ.'y'.AND.lats.GT.0.) azim=0.0
            IF (caz.EQ.'y'.AND.lats.LT.0.) azim=PI          
            GOTO 110
        ENDIF 
      ELSE
          lats=atan(zs/SQRT(xs**2.+ys**2.))
      ENDIF


C Calculate longitud of satellite.
      IF (xs.EQ.0.) THEN
        lons=PI/2
        IF(ys.LT.0.) lons=-lons
      ELSE
        lons=atan(ys/xs)
        IF (xs.LT.0.) lons=lons+PI
      ENDIF

C Calculate elevation and azimuth, careful if it is not possible 
C to estimate azimuth (caz='n')
      alt=asin(((xs-xo)*xo+(ys-yo)*yo+(zs-zo)*zo)/
     *          ((dist)*SQRT(xo**2.+yo**2.+zo**2.)))
      aux1=acos(sin(lats)*sin(lato)+
     *            cos(lats)*cos(lato)*cos(lons-lono))
      IF (caz.EQ.'y'.AND.sin(aux1).NE.0.) THEN
           cosaz=(sin(lats)-cos(aux1)*sin(lato))
     *              /(sin(aux1)*cos(lato))
           sinaz=(cos(lats))*sin(lons-lono)/(sin(aux1))
           IF (abs(cosaz).LE.1.0) THEN     
             azim=acos(cosaz)
             IF (sinaz.LT.0.) azim=2.*PI-azim
           ELSE 
C               It could happen in extreme cases in which azimuth
C               is close to zero and sin(aux) is very small
             azim=asin(sinaz)
             IF (cosaz.LT.0.) azim=azim+PI
           ENDIF
           
      ENDIF

  110 CONTINUE       
      RETURN
      END



C       *****************************************************************
C       *****************************************************************
C       *                       SUBROUTINE UMBRAL                       *
C       *****************************************************************
C       *****************************************************************
C       V1.3                     By: E. Mohíno
C                                                Creation date :  28/X/02
C                                           Last actualization :03/XII/02
C       *****************************************************************
C       Purpose: select those epochs in which elevation of satellite is
C               above some threshold (umb) given in degrees.
C       *****************************************************************
C       Files involved
C       Input
C               -'estac'sat#.aux: 'estac' is the station name and # prn
C                       number of satellite. These files will record distance,
C                       elevation and azimuth of each satellite for each epoch.
C                       These files will be deleted in subroutine umbral.
C       Output:
C               -'estac'satumb#.aux: (variable filsav). Same information but
C                       just for those epochs above certain threshold
C       *****************************************************************
C       Input Variables 
C               -iden: array with prn satellites
C               -nsat: total number of satellites
C               -nnep: total number of epochs to generate in RINEX output file
C               -umb: elevation mask
C               -estac: name of receiver
C       Output Variables: none
C       *****************************************************************
C       *****************************************************************

      SUBROUTINE umbral(iden,nsat,nnep,umb,estac)
      

C Formats
   10 FORMAT(3A,I4)
   20 FORMAT(//9X,17A3/9X,17A3/9X,17A3/9X,17A3/9X,17A3)
   30 FORMAT(I4,4(1X,I2),1X,F11.8/12(F14.6,1X))
   40 FORMAT(I2,85(A3))
   50 FORMAT(85(I2,1X))
C End formats

C Declaracion de variables
      IMPLICIT NONE
      CHARACTER iden(85)*3,filsav*30,filout*30,estac*4
C               iden=array with satellite prn
C               filsav=output file with elevation above elevation mask
C               filout=input files with data for all elevations
C               estac=receiver name
      INTEGER annio,mes,dia,hora,min,nsat,nnep,i,j
C               5 first variables: epoch of observation
C               nsat=total number of available satellites
C               nnep=total number of observation epochs
C               i,j= counters
      DOUBLE PRECISION sec,umb,dist,alt,azim,lats,lons,xs,ys,zs
     *                  ,troperr,rel,rot,clerr
C               sec=seconds of observation epoch
C               umb=elevation mask
C               dist=satellite-receiver distance
C               alt=satellite elevation
C               azim=satellite azimuth
C               lats=satellite latitude
C               lons=satellite longitude
C               xs,ys,zs=satellite coordinates
C               troperr=tropospheric error 
C               rel=relativity effect
C               rot=Sagnac effect
C               clerr=satellite clock error
C End declaration variables


C Open each file containing information of satellite, copy epochs in 
C which elevation above elevation mask

      DO i=1,nsat
        filout=estac//'sat'//iden(i)//'.aux'
        OPEN(16+i,FILE=filout,STATUS='OLD')
        filsav=estac//'satumb'//iden(i)//'.aux'
        OPEN(16+nsat+i,FILE=filsav)

        DO j=1,nnep+1
          READ(16+i,30,ERR=100) annio,mes,dia,hora,min
     *                          ,sec,dist,alt,azim
     *                          ,lats,lons,xs,ys,zs
     *                          ,troperr,rel,rot,clerr

          IF (alt.GT.umb) THEN
             WRITE(16+nsat+i,30) annio,mes
     *                  ,dia,hora,min,sec,dist,alt,azim
     *                  ,lats,lons,xs,ys,zs
     *                  ,troperr,rel,rot,clerr

          ENDIF

          GOTO 110     
  100     WRITE(*,10) 'Error in file', filout,' Epoch #',j

  110     CONTINUE      
        ENDDO

C       CLOSE(16+i)
        CLOSE(16+i,STATUS='DELETE')
        CLOSE(16+nsat+i)
      ENDDO

      END


C       *****************************************************************
C       *****************************************************************
C       *                       SUBROUTINE RINEX                        *
C       *****************************************************************
C       *****************************************************************
C       V2.2                     By: E. Mohíno
C                                                Creation date :  14/X/02
C                                            Last actualization:03/XII/02
C       *****************************************************************
C       Purpose: create RINEX observation file from data of satellite 
C               position and different effects recorded in files.
C               This RINEX file is created according to RINEX version 2.1
C       *****************************************************************
C       Involved files
C       Input
C               -'estac'satumb#.aux: files with satellite information for
C                       all epochs in which elevation above elevation mask
C                       These files will be deleted once used.
C       Output
C               -'estac'dia_#.añoO: RINEX observation file for receiver 
C                       'esta', julian date dia, year año.
C       *****************************************************************
C       Input Variables 
C               -fecha: date of RINEX observation file creation given in
C                       input.txt
C               -annio0,mes0,dia0,hora0,min0,sec0:initial epoch of observation
C               -nep: total number of epochs to generate
C               -nsat: total number of available satellites.
C               -iden: array with satellite prn
C               -interv:time span between consecutive observations
C               -xpos,ypos,zpos: receiver coordinates (not antenna phase center)
C               -estac: receiver name
C               -to: types of observable to generate. Maximum 5 possibilities
C               -trer: generate tropospheric error? S/N
C               -dh,de,dn: antenna phase center with respect to receiver
C       Output Variables: none
C       *****************************************************************
C       List of FUNCTIONS used
C               cdiaj(annio,mes,dia)
C       *****************************************************************
C       *****************************************************************


      SUBROUTINE rinex(fecha,annio0,mes0,dia0,hora0,min0
     *          ,sec0,nep,nsat,iden,interv,xpos,ypos,zpos,estac
     *          ,to,trer,dh,de,dn)


C Formats
   10 FORMAT(F9.2,11X,A20,A20,A20)
   20 FORMAT(4A20)
   30 FORMAT(A60,A20)
   40 FORMAT(A20,A40,A20)
   50 FORMAT(3F14.4,18X,A20)
   60 FORMAT(2I6,48X,A20)
   70 FORMAT(I6,5(4X,A2),24X,A20)
   80 FORMAT(F10.3,50X,A20)
   90 FORMAT(5I6,F13.7,5X,A3,9X,A20)
  100 FORMAT(60X,A20)
  110 FORMAT(I4,4(1X,I2),1X,F11.8)
  120 FORMAT(F14.6)
C  120 FORMAT(12(F14.6,1X))
  130 FORMAT(1X,I2.2,4(1X,I2),F11.7,2X,I1,I3,12A3)
  140 FORMAT(F14.3,1X,I1)
  145 FORMAT(5(F14.3,2X))
  150 FORMAT(32X,12A3)
  160 FORMAT(12(F15.8,1X))
C End formats

C Definition of variables
      IMPLICIT NONE
      INTEGER wvfaL1,wvfaL2,nobs,annio,mes,dia,hora,min
     1          ,annio0,mes0,dia0,hora0,min0,nep,nsat,i,j,k
     2          ,satobs,annio2,flag,nok,nto,cto
C       wvfaL1=wavelength factor for L1
C       wvfaL2=wavelength factor for L2
C       nobs=number of observables to generate
C       annio,mes,dia,hora,min=observation epoch
C       annio0,mes0,dia0,hora0,min0=initial observation epoch
C       nep=total number of observation epochs
C       nsat=total number of available satellites
C       i,j,k=counters
C       satobs=number of available satellites for one observation epoch 
C       annio2=two last digits of year
C       flag=epoch flag. 0 means OK.
C       nok=counter of satellite number (avoid more than 12)
C       nto=total number of observables to generate
C       cto=counter for generating observables
      REAL vers
C       vers=RINEX version (2.1)
      DOUBLE PRECISION xpos,ypos,zpos,interv,sec,sec0,dist(85),tt,PI
     *          ,vto(5),alp(5),f1,f2,c
     *          ,dh,de,dn
C       xpos,ypos,zpos=receiver approx. position
C       interv=interval between 2 epochs
C       sec=second of observation epoch
C       sec0=second of initial observation epoch
C       dist=vector with satellite-receiver distances for obs. epoch
C       tt=elapsed seconds from initial epoch
C       PI=pi number
C       vto=vector with 5 observables
C       f1,f2=carrier frequencies
C       alp= coefficient to estimate observables
C       c=vacuum speed of light
C       dh,de,dn=position of antenna with respect to receiver
      CHARACTER ssss*4,ddd*3,f,yy*2,t,filenm*12,tsys*3,cdiaj*3
C       ssss=4-character station name
C       ddd=julian date of first observation
C       f=number of file sequence
C       yy=year
C       t=type of RINEX file (O= observation)
C       filenm=obs RINEX file name (ssssdddf.yyt)
C       tsys=positioning system used (GPS)
C       cdiaj=function that calculates julian date 
      CHARACTER to(5)*2,sto*2
C       to=types of observables
C       sto=each type of observable
      CHARACTER*20 caux1,caux2,caux3,caux4,aux
      CHARACTER*40 caux40
      CHARACTER*60 coment
      CHARACTER iden(85)*3,fecha*20,fileop*30
      CHARACTER idnval(85)*3,estac*4,trer*1
C       caux1,2,3 and 4,aux,caux40=auxiliar variables to write RINEX file
C       coment=comments to write in RINEX file
C       filein=file with relevant data
C       iden=array with prn of available satellites
C       fecha=creation date of RINEX files
C       fileop=file with satellite-receiver distances for sat prn #
C       idnval=array with available satellite at one observation epoch
C       estac=receiver name
C       trer=generate tropospheric error? S/N
C End definition variables

      c=299792458.0d0
      PI=4.0D0*DATAN(1.0D0)
      f1=1575.42D6
      f2=1227.60D6

C To generate different types of observables:
        DO cto=1,5
        vto(cto)=0.0D0
        alp(cto)=1.0D0
        ENDDO
        cto=1
        nto=0
      DO WHILE (cto.LE.5)
        sto=to(cto)
        IF(sto.EQ.'C1'.OR.sto.EQ.'P1'.OR.sto.EQ.'P2'.
     *          OR.sto.EQ.'L1'.OR.sto.EQ.'L2')THEN
         IF(sto.EQ.'L1')THEN
          alp(cto)=f1/c
         ENDIF  
         IF(sto.EQ.'L2')THEN
          alp(cto)=f2/c
         ENDIF  
         nto=cto
         cto=cto+1
         ELSE
          cto=6
         ENDIF      
        ENDDO
        IF(nto.EQ.0)THEN
         nto=1
         to(1)='C1'
        ENDIF
        IF(nto.LT.5)THEN
         DO cto=nto+1,5
          to(cto)='  '
         ENDDO
        ENDIF

        IF(nto.EQ.0)THEN
         nto=1
         to(1)='C1'
        ENDIF
        IF(nto.LT.5)THEN
         DO cto=nto+1,5
          to(cto)='  '
         ENDDO
        ENDIF

C Name of observation RINEX file to create
      ssss=estac
      ddd=cdiaj(annio0,mes0,dia0)
      f='0'
      yy=CHAR(48+MOD(annio0,100)/10)//CHAR(48+MOD(annio0,10))
      t='O'
      filenm=ssss//ddd//f//'.'//yy//t
      OPEN(13,FILE=filenm)

C Begin writing of data into RINEX file: header section
              vers=2.1
              caux1='OBSERVATION DATA'
              caux2='G (GPS)'
              caux3='RINEX VERSION / TYPE'
              WRITE(13,10) vers,caux1,caux2,caux3
        
              caux1='SiGOGprcs v.1       '
              caux2='                    '
              caux3='PGM / RUN BY / DATE '
              WRITE(13,20) caux1,caux2,fecha,caux3
              
              coment='SIMULATED RINEX from  PRECISE ORBITS'
              caux1='COMMENT'
              WRITE(13,30) coment,caux1

              IF(trer.EQ.'N')THEN
               coment='No tropospheric error being generated'
               caux1='COMMENT'
               WRITE(13,30) coment,caux1
              ENDIF


              coment=ssss
              caux1='MARKER NAME'
              WRITE(13,30) coment,caux1

              caux1='elsa mohino'
              caux40='UCM'
              caux2='OBSERVER / AGENCY'
              WRITE(13,40) caux1,caux40,caux2

              caux1='X124A123'
              caux2='XX'
              caux3='ZZZ'
              caux4='REC # / TYPE / VERS'
              WRITE(13,20) caux1,caux2,caux3,caux4

              caux1='234'
              caux2='YY'
              caux3=' '
              caux4='ANT # / TYPE'
              WRITE(13,20) caux1,caux2,caux3,caux4

              caux1='APPROX POSITION XYZ'
              WRITE(13,50) xpos,ypos,zpos,caux1

              caux1='ANTENNA: DELTA H/E/N'
              WRITE(13,50) dh,de,dn,caux1

              wvfaL1=1
              wvfaL2=1
              caux1='WAVELENGTH FACT L1/2'
              WRITE(13,60) wvfaL1,wvfaL2,caux1

              nobs=nto
              caux1='# / TYPES OF OBSERV'
              WRITE(13,70) nobs,to,caux1

              caux1='INTERVAL'
              WRITE(13,80) interv,caux1

              tsys='GPS'
              caux1='TIME OF FIRST OBS'
              WRITE(13,90) annio0,mes0,dia0,hora0,min0,sec0,tsys,caux1

              caux1='END OF HEADER'
              WRITE(13,100) caux1



C Begin data section:
      flag=0

C Open files with satellite-receiver distances

      DO i=1,nsat
       fileop=estac//'satumb'//iden(i)//'.aux'
       OPEN(16+i,FILE=fileop,STATUS='OLD')
      ENDDO
       
C Which satellites were available at that precise epoch?
      DO j=0,nep

       satobs=0
       DO i=1,85 
         idnval(i)='   '
       ENDDO

       DO i=1,nsat
        READ(16+i,110,END=450,ERR=450) annio,mes,dia,hora,min,sec
        tt=sec-sec0+(min-min0)*60.0+(hora-hora0)*3600.
     *      +(dia-dia0)*24.*3600.       

        IF(tt.GT.j*interv) THEN
           BACKSPACE 16+i
           dist(i)=-9.
           GOTO 500
        ELSE
          IF(tt.LT.j*interv) GOTO 400
          READ(16+i,120) dist(i)
          satobs=satobs+1
          idnval(satobs)=iden(i)
          GOTO 500
        ENDIF
        

  400   WRITE(*,*) 'Error, could not read epoch before!:'
        WRITE(*,*) 'Satellite:',iden(i)
        READ(16+i,*) aux
        WRITE(*,*) aux
        READ(16+i,110) annio,mes,dia,hora,min,sec
        tt=sec-sec0+(min-min0)*60.0+(hora-hora0)*3600.
     *      +(dia-dia0)*24.*3600.

  450   CONTINUE

        dist(i)=-9.
        
  500   CONTINUE
       ENDDO
    
C Prepare variables to write in RINEX file
       annio=annio0
       mes=mes0
       dia=dia0+interv*j/(24.*3600.)
       hora=hora0+(interv*j-24.*3600.*(dia-dia0))/3600.
       min=min0+(interv*j-24.*3600.*(dia-dia0)-
     *      3600.*(hora-hora0))/60.
       sec=sec0+(interv*j-24.*3600.*(dia-dia0)-
     *      3600.*(hora-hora0)-(min-min0)*60.)     
       annio2=MOD(annio,100)


C Maximum number of satellites for one epoch=12
       IF(satobs.GT.12) satobs=12

       WRITE(13,130) annio2,mes,dia,hora,min,sec,flag,
     *   satobs,(idnval(k),k=1,12)


        nok=0

        DO i=1,nsat
          IF (dist(i).NE.-9.) THEN
            nok=nok+1
            dist(i)=dist(i)*1000.0D0
C Build vto with observables:
            DO cto=1,nto
             vto(cto)=dist(i)*alp(cto)
            ENDDO
            IF(nok.LE.12) WRITE(13,145) (vto(cto),cto=1,nto)
          ENDIF
        ENDDO
        

      ENDDO


C Close files
      DO i=1,nsat
C      CLOSE(16+i)
      CLOSE(16+i,STATUS='DELETE')
      ENDDO
C      CLOSE(12)
      CLOSE(13)
      CLOSE(14)
      WRITE(*,*)' '
      WRITE(*,*)'********************************************'
      WRITE(*,*)'       OUTPUT RINEX OBSERVATION FILE:       '
      WRITE(*,*)filenm
      WRITE(*,*)'********************************************'
      WRITE(*,*)' '
      END


C       ***************************************************************** 
C       ***************************************************************** 
C       *                       FUNCTIONN CDIAJ                         *
C       ***************************************************************** 
C       ***************************************************************** 
C       V1.1            By: E. Mohíno 
C                                                  Creation date:03/XI/02 
C                                            Last actualization:02/XII/02 
C       ***************************************************************** 
C       Purpose: calculate julian date for a given day of month of year 
C       ***************************************************************** 
C       Input Variables 
C               -annio:year 
C               -mes: month
C               -dia: day
C       ***************************************************************** 
C       Function output: cdiaj: character with julian day
C       ***************************************************************** 
C       *****************************************************************

      CHARACTER*3 FUNCTION cdiaj(annio,mes,dia)

C Declaration of variables
      IMPLICIT NONE
      INTEGER annio,mes,dia,diames(12),jdia,j,cent,dec,num
C       annio= year
C       mes= month
C       dia= day
C       diames= vector with total number of days per month
C       jdia= elapsed days from begining of year
C       j= couter
C       cent=hundres in jdia
C       dec=tens in jdia
C       num=units in jdia
C End declaration variables

C Definition of diames
      diames(1)=31
      diames(2)=28
      diames(3)=31
      diames(4)=30
      diames(5)=31
      diames(6)=30
      diames(7)=31
      diames(8)=31
      diames(9)=30
      diames(10)=31
      diames(11)=30
      diames(12)=31

C Modify if annio is leap year
      IF (MOD(annio,4).EQ.0.AND.MOD(annio,100).NE.0) diames(2)=29
      IF (MOD(annio,400).EQ.0) diames(2)=29

C Estimate days elapsed form begining of year
      jdia=dia
      IF (mes.LT.2) GOTO 100
      DO j=1,mes-1
       jdia=jdia+diames(j)
      ENDDO
  100 CONTINUE

C Calculate each number in jdia
      cent=jdia/100
      dec=(jdia-cent*100)/10
      num=(jdia-cent*100-dec*10)
      cdiaj=CHAR(48+cent)//CHAR(48+dec)//CHAR(48+num)     
      RETURN
      END




C       *****************************************************************
C       *****************************************************************
C       *                       FUNCTION SEEBER2                        *
C       *****************************************************************
C       *****************************************************************
C       V1.2                      By: E. Mohíno
C                                                Creation date : 20/XI/02
C                                           Last actualization :03/XII/02
C       *****************************************************************
C       Purpose: estimate tropospheric error using modified hopfield model
C               See Seeber for a referece.
C       *****************************************************************
C       Input Variables 
C               -P: surface pressure in mb
C               -temp: surface temperature in ºC
C               -hr: relative humidity (%)
C               -e: elevation of satellite
C               -xo,yo,zo: position of receiver (in km)
C               -lato:receiver latitude
C       *****************************************************************
C       Function output: seeber2: tropospheric error
C       *****************************************************************
C       *****************************************************************


      DOUBLE PRECISION FUNCTION seeber2(P,temp,hr,e,xo,yo,zo,lato)

C Declaraction variables
      IMPLICIT NONE
      DOUBLE PRECISION P,temp,T,hr,e,pv,Nd0,Nw0,hd,hw,PI,facd,facw
     *  ,xo,yo,zo,Re,r,h,a,ec,reli,lato,b,Ppr
C       P=surface pressure in mb
C       Ppr=surface pressure in mb
C       temp=surface temperature in ºC
C       T=surface temperature in K
C       hr=relative humidity (%)
C       e=elevation of satellite
C       pv=partial water vapour pressure
C       Nd0=dry air factor 
C       Nw0=water factor 
C       hd=dry layer height
C       hw=water layer height
C       PI=pi number
C       facd,facw=dry and water factors
C       xo,yo,zo=receiver coordinates (km)
C       Re=Earth radius
C       r=receiver distance to Earth center
C       h=height of receiver above Earth surface 
C       a= Earth semimajor axis
C       ec= eccentricity of ellipsoid
C       reli=Earth radius at receiver latitude
C       lato=receiver latitude
C       b=semiminor Earth axis
C End declaration variables

       Re=6371.0D3
       PI=(DATAN(1.d0))*4.d0

C Earth ellipsoid
        a=6378137.0D0
        b=6356752.314D0
        ec=DSQRT(1-(b/a)**2.0D0)
        reli=a*DSQRT((1-ec**2.0D0)/(1-(ec*DCOS(lato))**2.0D0))


        r=DSQRT(xo**2.0D0+yo**2.0D0+zo**2.0D0)*1.0D3
C receiver heihgt (m)
        h=r-reli

C Temperature in K
        T=temp+273.15d0

C New hidrostatic pressure
        Ppr=P*DEXP(-9.81D0*h/(T*287.0D0))

        pv=hr/100.*DEXP(-37.2465+.213166*T-.256908d-3*T**2)
        IF(pv.GT.1.0d0) pv=pv/100.d0

C Hopfield modified model:

      hd=40136.0d0+148.72*(T-273.15)
      hw=11.0d3

        Nd0=155.2d-7*hd*P/T
        Nw0=1.0D-6*hw/5*(-12.96D0*T+3.718D5)*pv/T**2.0D0

C Modification to take into account receiver height
        Nd0=Nd0*((hd-h)/hd)**5.0D0
        Nw0=Nw0*((hw-h)/hw)**5.0D0

        IF(h.GT.hd) Nd0=0.0D0
        IF(h.GT.hw) Nw0=0.0D0

C Geometric factors
      facd=1.0/sin(dsqrt((e*e+6.25))*PI/180.)
      facw=1.0/sin(dsqrt((e*e+2.25))*PI/180.)
      seeber2=Nd0*facd+Nw0*facw
      RETURN
      END
