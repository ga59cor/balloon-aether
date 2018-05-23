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
C       *                       PROGRAM SiGOGbcst                       *
C       *****************************************************************
C       *****************************************************************
C       V2.2                     by: E. Mohino
C                                                Creation date : 11/XI/02
C                                            Last actualization: 15/IX/04
C       *****************************************************************
C       Purpose: This programme will generate a RINEX observation file
C               given the location of observer and period of observation
C               from a RINEX navigation file.
C       *****************************************************************
C       Family of files involved:
C       Input:
C               -input.txt (Input data file)
C               -file with navigation RINEX orbits 
C       Output:
C               -rinex file
C               -s//igs//weekday.sp3 (files with precise orbits "clean"
C                       defects
C       Intermediate:   several families(names depend on station)
C               -sat0#.aux :prn # satellite ephemeris
C               -sat#.aux family (satellite position for each prn# and
C                       observation epoch
C               -"esta"sat#.aux family (distance, elevation and azimuth of satellite
C                       prn # for each observation epoch. "esta" station's name)
C               -"esta"sat#.aux family (distance, elevation and azimuth of satellite
C                       prn # for each observation epoch for which elevation is above
C                       threshold)
C       *****************************************************************
C       List of SUBROUTINES used by this program:
C               lec21(filein,nsat,iden,niden)
C               intorb(filein,sat,nep,annio0,mes0,dia0,hora0,min0,
C                       sec0,nnep,inter,xo,yo,zo)
C               posorb(t,toe,m0,sqrta,dn,e,omp,om0,w0,cuc,cus,crc,crs,
C                       cic,cis,i0,ip,x,y,z,rel)
C               posic(filein,nsat,nnep,xo,yo,zo,estac,iden,lato,lono)
C               calculo(xo,yo,zo,lato,lono,xs,ys,zs,dist,caz,lats,lons,
C                       azim,alt)
C               umbral(iden,nsat,nnep,umb,estac)
C               rinex(fecha,annio0,mes0,dia0,hora0,min0,sec0,nep,nsat,iden,
C                       interv,xpos,ypos,zpos,estac,dh,de,dn)
C       *****************************************************************
C       List of FUNCTIONS used by this programme:
C               cdiaj(annio,mes,dia)
C               segGPS(annio,mes,dia,hora,min,sec)
C               diajul(annio,mes,dia)
C               seeber2(P,temp,hr,e,xo,yo,zo,lato)
C       *****************************************************************
C       *****************************************************************


      PROGRAM observa

C Formats
   10 FORMAT(A)
   20 FORMAT(2X,3(F14.7,1X))
   30 FORMAT(2X,A4,1X,5A2)
   32 FORMAT(2X,A20)
   40 FORMAT(2X,F4.1,1X,A1)
   50 FORMAT(2X,I4,4(1X,I2),1X,F11.8)
   60 FORMAT(2X,2(I2,1X),F11.8)
   70 FORMAT(2X,F8.2)


      IMPLICIT NONE
      INTEGER annioi,mesi,diai,horai,mini,horaf,minf,nsat,nnep,niden(85)
     1  ,i,nep
C       annioi,mesi,diai,horai,mini: date & initial epoch of observation
C       horaf,minf: final epoch of observation
C       nsat:total number of satellites in order for observation day
C       nnep:total number of epochs to observe
C       niden:total number of satellites available
C       i:counter
C       nep:total number of epochs for which there are satellite positions
      DOUBLE PRECISION seci,secf,inter,xo,yo,zo,umb,dnnep,lato,lono
     *  ,xor,yor,zor,dh,de,dn
C       seci:seconds of initial epoch
C       secf:seconds of final epoch
C       inter: inteval between consecutive observations
C       xo,yo,zo:antenna position of receiver in km
C       umb:elevation threshold (elevation mask)
C       dnnep:auxiliar variable for calculus of integer number of epochs
C       lato,lono: observation position latitude and longitude
C       xor,yor,zor: observer position (NOT antenna)
C       dh,de,dn:antenna position from observer
      CHARACTER aux2*2,filein*30,iden(85)*3,estac*4,fecha*20
     1  ,ssss*4,ddd*3,yy*2,f*1,t*1,cdiaj*3,sat*3,to(5)*2,trer*1
C       aux2:auxiliar variable to detect relevant information in input file
C       filein:broadcasted ephemeris file name
C       iden:arrya with prn of satellites
C       estac:observation station name
C       fecha:date of creation of RINEX observation file
C       ssss:receiver name
C       ddd:julian date of observation
C       yy:year of observation
C       f:number of RINEX file
C       t:type of RINEX file (navigation)
C       cdiaj:julian date of observation
C       sat:one satellite prn
C       to:types of observable to generate: C1P1P2L1L2
C       trer: calculate tropospheric error? S/N
C End declaration variables


C Reading input file:
      OPEN(12,FILE='input.txt')

   99 CONTINUE
C 1) Receiver coordinates (km)
  100 CONTINUE
        READ(12,10,END=500)aux2
      IF(aux2.NE.'**')GOTO 100
      BACKSPACE 12
      READ(12,20) xor,yor,zor
                        
C 2) Station name and observable to generate (C1,P1,P2,L1,L2)
  110 CONTINUE
        READ(12,10)aux2
      IF(aux2.NE.'**')GOTO 110
      BACKSPACE 12
      READ(12,30) estac,to

C 3) Elevation mask in degrees
  120 CONTINUE
        READ(12,10)aux2
      IF(aux2.NE.'**')GOTO 120
      BACKSPACE 12
      READ(12,40) umb,trer

C 4) Date and initial epoch of observation
  130 CONTINUE
        READ(12,10)aux2
      IF(aux2.NE.'**')GOTO 130
      BACKSPACE 12
      READ(12,50) annioi,mesi,diai,horai,mini,seci

C 5) Final epoch
  140 CONTINUE
        READ(12,10)aux2
      IF(aux2.NE.'**')GOTO 140
      BACKSPACE 12
      READ(12,60) horaf,minf,secf

C 6) Interval between observations
  150 CONTINUE  
        READ(12,10)aux2
      IF(aux2.NE.'**')GOTO 150
      BACKSPACE 12
      READ(12,70)inter

C 7) Date of creation of RINEX observation file
  160 CONTINUE  
        READ(12,10)aux2
      IF(aux2.NE.'**')GOTO 160
      BACKSPACE 12
      READ(12,32)fecha

C 8) Position of antenna from receiver
  170 CONTINUE  
        READ(12,10)aux2
      IF(aux2.NE.'**')GOTO 170
      BACKSPACE 12
      READ(12,'(8X,3F8.4)')dh,de,dn
C Finished reading input file

C Coordinates of antenna:
      CALL antena(xor*1.0D3,yor*1.0D3,zor*1.0D3,dh,de,dn,xo,yo,zo)
        xo=xo/1.0D3
        yo=yo/1.0D3
        zo=zo/1.0D3

C Name of RINEX navigation file:
      ssss='brdc'
      ddd=cdiaj(annioi,mesi,diai)
      f='0'
      yy=CHAR(48+MOD(annioi,100)/10)//CHAR(48+MOD(annioi,10))
      t='n'
      filein=ssss//ddd//f//'.'//yy//t

 
C Reading broadcasted ephemeris, calculating satellite positions in t-tau,
C satellite clock error. Record data in files sat//input file
      CALL lec21(filein,nsat,iden,niden)                                        

C Reading orbit information. Interpolation of data. Satellite position
C calculation for epoch t-tau. Satellite clock error estimation.
      nnep=(horaf-horai)*3600+(minf-mini)*60+(secf-seci)
      dnnep=(horaf-horai)*3600.+(minf-mini)*60.+(secf-seci)
      nnep=nnep/inter

      DO i=1,nsat
        sat=iden(i)
        nep=niden(i)
        CALL intorb(filein,sat,nep,annioi,mesi,diai,horai,mini,
     *                  seci,nnep,inter,xo,yo,zo)
      ENDDO

C Calculating azimuths and elevations, as well as tropospheric errors, once
C known elevation of each satellite

      CALL posic(filein,nsat,nnep,xo,yo,zo,estac,iden,trer,lato,lono)


C Filtering data with elevation mask given by umb
      CALL umbral(iden,nsat,nnep,umb,estac)

C Finally, generating RINEX observation file:
      CALL rinex(fecha,annioi,mesi,diai,horai,mini,seci
     1          ,nnep,nsat,iden,inter,xor*1000.0d0
     2          ,yor*1000.0d0,zor*1000.0d0,estac
     3          ,to,trer,dh,de,dn)


      GOTO 99

  500 CONTINUE
      CLOSE(12)

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
      DOUBLE PRECISION xo,yo,zo,dh,de,dn,x,y,z,a,e2
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
C       *                       SUBROUTINE LEC21                        *
C       *****************************************************************
C       *****************************************************************
C       V1.0                     By: E. Mohíno
C                                                Creation date :10/XII/02
C                                         Last actualization :  19/XII/02
C       *****************************************************************
C       Purpose: separate ephemeris for each satellite recorded in a
C               RINEX navigation file
C       *****************************************************************
C       Files involved
C       Input:
C               -filein:RINEX navigation file
C       Output:
C               -sat0#filein: satellite prn # ephemeris
C       *****************************************************************
C       Input Variables 
C               -filein:name of RINEX navigation file
C       Output Variables 
C               -nsat:total number of available satellites in filein
C               -iden:array with prn of those satellites
C               -niden:array with number of epochs recorded for each sat.
C       *****************************************************************
C       *****************************************************************



      SUBROUTINE lec21(filein,nsat,iden,niden)


C Formats
   10 FORMAT(A2)
   20 FORMAT(60X,A20) 
   30 FORMAT(//////A3)
   40 FORMAT(A80)

C Definition  variables
      IMPLICIT NONE
      CHARACTER filein*30,iden(85)*3,endh*20,aux2*2,aux3*3
     1 ,aux1*1,fileout*30,aux80*80
C       filein=name of RINEX navigation file
C       iden=array with prn of available satellites
C       endh=to locate END OF HEADER line
C       aux2,aux3,aux1,aux80=auxiliar variables to locate different parts
C       fileout=name of output files
      INTEGER nsat,niden(85),i,j
C       nsat=total number of available satellites
C       niden=array with number of epochs recorded for each satellite
C       i,j=counters

      DO i=1,85
        iden(i)='  '
        niden(i)=0
      ENDDO

      OPEN(15,FILE=filein,STATUS='OLD')

C GO to END OF HEADER
  200 CONTINUE
      READ(15,20)endh
      IF(endh.NE.'END OF HEADER') GOTO 200

C How many satellites are they in filein? How many epochs recorded?
  210 CONTINUE
      READ(15,10,END=220)aux2
      aux3=' '//aux2
      aux1='y'
      DO i=1,85
        IF(aux3.EQ.iden(i)) aux1='n'
      ENDDO
      IF(aux1.EQ.'y') THEN
        DO i=85,1,-1
          IF(iden(i).EQ.'   ') j=i
        ENDDO
        iden(j)=aux3
      ENDIF
      READ(15,30)aux2
      GOTO 210
  220 CONTINUE
      nsat=j


      REWIND 15

C GO to END OF HEADER
  230 CONTINUE
      READ(15,20)endh
      IF(endh.NE.'END OF HEADER') GOTO 230

C Open all output files
      DO i=1,nsat
        fileout='sat0'//iden(i)//filein
        OPEN(15+i,FILE=fileout)
      ENDDO

C Separate each satellite
  240 CONTINUE
      READ(15,10,END=250)aux2
      aux3=' '//aux2
      j=-1
      DO i=1,nsat
        IF(iden(i).EQ.aux3) j=i
      ENDDO

      BACKSPACE 15
      niden(j)=niden(j)+1
      DO i=1,8
        READ(15,40)aux80
        WRITE(15+j,40)aux80
      ENDDO
      GOTO 240

  250 CONTINUE

      DO i=1,nsat
        CLOSE(15+i)
      ENDDO
      CLOSE(15)

      RETURN
      END



C       *****************************************************************
C       *****************************************************************
C       *                       SUBROUTINE POSORB                       *
C       *****************************************************************
C       *****************************************************************
C       V1.0                     By: E. Mohíno
C                                                Creation date :10/XII/02
C                                     Last actualization       :18/XII/02
C       *****************************************************************
C       Purpose: calculate satellite coordinates (x,y,x) and relativity
C               effect for each epoch given satellite ephemeris terms.
C       *****************************************************************
C       Input Variables 
C               -t: elapsed seconds from begining GPS week
C               -toe: time of ephemeris 
C               -m0:mean keplerian orbit anomaly 
C               -sqrta: sqrt of semimajor orbit axis 
C               -dn: rate of angular velocity of satellite (n)
C               -e: keplerian orbit eccentricity
C               -omp: rate of chang of right ascention
C               -om0: right ascention at toe
C               -w0: argument of perigee at reference time
C               -cuc: amplitude of cosine armonic correction term to the
C                       argument of perigee
C               -cus: amplitude of sine armonic correction term to the
C                       argument of perigee
C               -crc: amplitude of cosine armonic correction term to the
C                       orbit radius
C               -crs: amplitude of sine armonic correction term to the
C                       orbit radius
C               -cic: amplitude of cosine armonic correction term to the
C                       angle of inclination
C               -cis: amplitude of sine armonic correction term to the
C                       angle of inclination
C               -i0: inclination angle at reference time
C               -ip: rate of inclination angle
C       Output Variables 
C               -x,y,z:satellite coordinates
C               -rel: relativistic effect
C       *****************************************************************
C       *****************************************************************


      SUBROUTINE posorb(t,toe,m0,sqrta,dn,e,omp,om0
     *  ,w0,cuc,cus,crc,crs,cic,cis,i0,ip,x,y,z,rel)

C Definition variables
      IMPLICIT NONE
      DOUBLE PRECISION t,toe,m0,sqrta,dn,mu,m,tt,e
     1 ,emay,emayan,cv,sv,v,r0,r,a,PI,WE,omp,t2,om0,l,w0,w,u
     2 ,want,cuc,cus,crc,crs,i0,i,ip,cic,cis,rx,ry,x,y,z
     3 ,rel,fmay,c
C       t= time for which we want satellite coordinates
C       toe= time of reference
C       m0= mean keplerian orbit anomaly
C       sqrta= sqrt of semimajor orbit axis
C       dn= rate of angular velocity of satellite (n)
C       mu= WGS 84 value of Earth's universial gravitational parameter
C       m=mean anomaly at time t
C       tt=elapsed time since toe 
C       e= orbit eccentricity
C       emay= Eccentric anomaly
C       emayan=previous eccentric anomaly
C       cv=cosine of v
C       sv=sine of v
C       v=true anomaly
C       r0=satellite geocentric distance at toe
C       r=satellite geocentric distance at t
C       a=semimajor orbit axis
C       PI= pi number
C       WE=rotational velocity of Earth
C       omp= rate of chang of right ascention
C       t2=elapsed time since toe taking into account crossovers
C       om0= right ascention at toe
C       l= angle between Greenwich and ascending node
C       w0= argument of perigee at toe
C       w=argument of perigee at t
C       u=angle between satellite and ascending node (w+v)
C       want=w in previous step
C       cuc: amplitude of cosine armonic correction term to the
C                       argument of perigee
C       cus: amplitude of sine armonic correction term to the
C             argument of perigee
C       crc: amplitude of cosine armonic correction term to the
C                       orbit radius
C       crs: amplitude of sine armonic correction term to the
C                 orbit radius
C       i0: inclination angle at reference time
C       i=inclination at time t
C       ip= rate of inclination angle
C       cic: amplitude of cosine armonic correction term to the
C                       angle of inclination
C       cis: amplitude of sine armonic correction term to the
C                       angle of inclination
C       rx=position x component at orbit plane
C       ry=position y component at orbit plane
C       x,y,z=satellite coordinates
C       rel=relativity effect
C       fmay=constant to calculate re. (2*SQRT(mu)/c^2)
C       c=vacuum light speed
      PARAMETER(mu=3986005.0D8,WE=7292115.1467D-11
     1 ,fmay=-4.442807633D-10,c=299792458.0D0)
      INTEGER j
C       j=counter




      PI=4.0D0*DATAN(1.0D0)
      tt=t-toe
      a=sqrta*sqrta



      m=m0+(DSQRT(mu)/sqrta**3.0D0+dn)*tt

C Calculate l taking into account crossovers
      t2=t
      IF(t.GT.604799.0D0) t2=t-IDINT(toe/604799.0D0)*604800.0D0
      l=om0+omp*tt-WE*t2

C Calculate eccentric anomaly
        emayan=m
      DO j=1,10
        emay=m+e*DSIN(emayan)
        IF(abs((emay-emayan)/emay).LT.1.0D-14) GOTO 100
        emayan=emay
      ENDDO
  100 CONTINUE      

C Calculate true anomaly
      r0=a*(1-e*DCOS(emay))
      cv=a*(DCOS(emay)-e)/r0
      sv=(a*DSQRT(1-e*e)*DSIN(emay))/r0
      v=DATAN(sv/cv)
      IF(cv.LT.0.0D0) v=v+PI
      IF(v.LT.0.0D0)v=v+2.0D0*PI


C Calculate argument of perigee
      want=w0+cuc*DCOS(2.0D0*(w0+v))+cus*DSIN(2.0D0*(w0+v))
      DO j=1,10
        w=w0+cuc*DCOS(2.0D0*(want+v))+cus*DSIN(2.0D0*(want+v))
        IF(abs((w-want)/w).LT.1.0D-14)GOTO 110
        want=w
      ENDDO
  110 CONTINUE

      u=w+v

      r=r0+crc*DCOS(2.0D0*u)+crs*DSIN(2.0D0*u)

      i=i0+cic*DCOS(2.0D0*u)+cis*DSIN(2.0D0*u)+ip*tt

C Calculate rx,ry coordinates of position at orbit plane
      rx=r*DCOS(u)
      ry=r*DSIN(u)

C Rotate to a geocentric reference sistem
C R=R{-l}R{-i}
      x=rx*DCOS(l)-ry*DSIN(l)*DCOS(i)
      y=rx*DSIN(l)+ry*DCOS(l)*DCOS(i)
      z=ry*DSIN(i)

C Relativity effect in meters
      rel=-fmay*e*sqrta*DSIN(emay)*c


      RETURN
      END



C       *****************************************************************
C       *****************************************************************
C       *                       FUNCTION SEGGPS                         *
C       *****************************************************************
C       *****************************************************************
C       V1.0                       By: E. Mohíno
C                                                Creation date :10/XII/02
C                                       Last actualization :    10/XII/02
C       *****************************************************************
C       Purpose: calculate seconds of GPS week that define a certain epoch
C               given year,month,hour,minute and second of that epoch
C       *****************************************************************
C       Input Variables 
C               -annio:year
C               -mes: month
C               -dia: day
C               -hora:hour
C               -min: minute
C               -sec: second
C       *****************************************************************
C       Output of function: segGPS: seconds elapsed since begining of GPS week
C       *****************************************************************
C       List of FUNCTIONS used
C               diajul(annio,mes,dia)
C       *****************************************************************
C       *****************************************************************

      
      DOUBLE PRECISION FUNCTION segGPS(annio,mes,dia,hora,min,sec)

C Definition variables
      IMPLICIT NONE
      INTEGER annio,mes,dia,hora,min,dias,smn,i,diajul,annio4
C       annio=year
C       mes=month
C       dia=day
C       hora=hour
C       min=minute
C       dias=days elapsed from begining GPS week
C       smn,i=counter
C       diajul=function to calculate julian day
C       annio4=year with 4 digits
      DOUBLE PRECISION sec
C       sec=seconds

C Year in 4 digits, in case it only comes with 2
        annio4=annio
      IF(annio.LT.100) annio4=annio+1900
      IF(annio.LT.80) annio4=annio+2000

C Calculate GPS week and day inside that week
      dias=-6
       IF(annio4.LE.1980) THEN
        annio4=1980
        GOTO 200
       ENDIF
       DO i=1980,annio4-1

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
  200 CONTINUE

      dias=dias+diajul(annio4,mes,dia)
      smn=dias/7
      dias=MOD(dias,7)

C Calculate seconds elapsed from begining of GPS week
      segGPS=sec+min*60.0D0+hora*3600.0D0+dias*3600.0D0*24.0D0

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
C       *                       SUBROUTINE POSIC                        *
C       *****************************************************************
C       *****************************************************************
C       V1.3                     By: E. Mohíno
C                                                Creation date :  28/X/02
C                                       Last modified          :03/XII/02
C       *****************************************************************
C       Purpose: calculate distance, elevation and azimuth of each satellite
C               Spherical approximation of Earth used.
C               Also calculated relativity effect, satellite clock error,
C               and tropospheric errors (if so stated in input file input.txt)
C       *****************************************************************
C       Files involved
C       Input:
C               -sat#filein: with satellite coordinates, clock errors for
C                       satelite #. Once used, these files will be 
C                       deleted
C       Output:
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
C       Output Variables 
C               -lato,lono:satellite latitude and longitude
C       *****************************************************************
C       List of SUBROUTINES used:
C               calculo(xo,yo,zo,lato,lono,xs,ys,zs,dist,caz,lats,lons,
C                       azim,alt)
C       *****************************************************************
C       List of FUNCTIONS used
C               seeber2(P,temp,hr,e,xo,yo,zo,lato)
C       *****************************************************************
C       *****************************************************************

      
      SUBROUTINE posic(filein,nsat,nnep,xo,yo,zo,estac,iden,trer
     *                  ,lato,lono)

C Formats
   10 FORMAT(A)
   20 FORMAT(2A)
   30 FORMAT(I4,4(1X,I2),1X,F11.8/3(2X,F13.6),3(2X,F17.8))
   40 FORMAT(A,F10.6)
   50 FORMAT(I4,4(1X,I2),1X,F11.8)
   60 FORMAT(3(F13.6,1X),9(2X,F17.8))

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
     *  ,seeber2,rot
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
C       seeber2=function to calculate tropopheric error
C       rot=Sagnac effect
      PARAMETER(PI=3.141592d0)

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
            WRITE(*,10) 'El punto no tiene sentido'
            GOTO 500
        ELSE
          lato=PI/2
          IF (zo.LT.0.) lato=-lato
          WRITE(*,40) 'Receiver latitude is ',lato*180./PI
          WRITE(*,10) 'Receiver longitude is not defined'
          WRITE(*,10) 'Satellite azimuth will not be calculated'
          DO i=1,nsat
            filout='sat'//iden(i)//filein
            OPEN(16+i,FILE=filout,STATUS='OLD')
            filsav=estac//'sat'//iden(i)//'.aux'
            OPEN(16+nsat+i,FILE=filsav)
            DO j=0,nnep
               READ(16+i,30,END=110) annio,mes,dia,hora,min
     *                          ,sec,xs,ys,zs,clerr,rel,rot
               dist=SQRT((xs-xo)**2.+(ys-yo)**2.+(zs-zo)**2.)
               CALL calculo(xo,yo,zo,lato,lono,xs,ys,zs,dist,caz
     *                  ,lats,lons,azim,alt)
               troperr=seeber2(Pres,Temp,hr,alt*180./PI,xo,yo,zo,lato)
               IF(trer.EQ.'N')troperr=0.0D0
               WRITE(16+i+nsat,50) annio,mes,dia,hora,min,sec
               dist=dist+(-clerr+troperr+rel)/1000.
               WRITE(16+i+nsat,60) dist,alt*180./PI,azim*180./PI
     *          ,clerr,rel,rot,troperr,xs,ys,zs
     *          ,lats*180.0D0/PI,lons*180.0D0/PI
            ENDDO

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
             READ(16+i,30,END=210) annio,mes,dia,hora,min
     *          ,sec,xs,ys,zs,clerr,rel,rot
             dist=SQRT((xs-xo)**2.+(ys-yo)**2.+(zs-zo)**2.)
             CALL calculo(xo,yo,zo,lato,lono,xs,ys,zs,dist,caz
     *          ,lats,lons,azim,alt)
             WRITE(16+i+nsat,50) annio,mes,dia,hora,min,sec
             troperr=seeber2(Pres,Temp,hr,alt*180./PI,xo,yo,zo,lato)
             IF(trer.EQ.'N')troperr=0.0D0
             dist=dist+(-clerr+troperr+rel)/1000.
             WRITE(16+i+nsat,60) dist,alt*180./PI,azim*180./PI
     *          ,clerr,rel,rot,troperr,xs,ys,zs
     *          ,lats*180.0D0/PI,lons*180.0D0/PI
          ENDDO

  210     CONTINUE

        CLOSE(16+i,STATUS='DELETE')
C       CLOSE(16+i)
        CLOSE(16+nsat+i)
      ENDDO
      ENDIF     
  500 CONTINUE

      RETURN
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
            WRITE(*,10) 'Un satelite en el polo'
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
   30 FORMAT(I4,4(1X,I2),1X,F11.8/3(F13.6,1X),9(2X,F17.8))
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

        DO j=0,nnep
          READ(16+i,30,ERR=100,END=110) annio,mes,dia,hora,min
     *                          ,sec,dist,alt,azim
     *          ,clerr,rel,rot,troperr,xs,ys,zs,lats,lons
          IF (alt.GT.umb) THEN
             WRITE(16+nsat+i,30) annio,mes
     *                  ,dia,hora,min,sec,dist,alt,azim
     *          ,clerr,rel,rot,troperr,xs,ys,zs,lats,lons
          ENDIF

          GOTO 110
  100     WRITE(*,10) 'Incorrect data in file:  ',filout,' epoca #',j
  110     CONTINUE
        ENDDO

C       CLOSE(16+i)
        CLOSE(16+i,STATUS='DELETE')
        CLOSE(16+nsat+i)
      ENDDO

      RETURN
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
        
              caux1='SiGOGbcst v.1       '
              caux2='                    '
              caux3='PGM / RUN BY / DATE '
              WRITE(13,20) caux1,caux2,fecha,caux3
              
              coment='SIMULATED RINEX from BROADCASTED ORBITS'
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





C       *****************************************************************
C       *****************************************************************
C       *                       SUBRoUTINe INTORB                       *
C       *****************************************************************
C       *****************************************************************
C       V1.0                     By: E. Mohíno
C                                                Creation date:10/XII/02
C                                               Last modified :18/XII/02
C       *****************************************************************
C       Purpose: generate coordinates of satellite #, clock error and
C               relativistic effect from ephemeris
C       *****************************************************************
C       Files involved
C       Input
C               -sat0#filein: ephemeris of satellite prn #. This file
C                       will be deleted after use.
C       Salida:
C               -sat#filein: record satellite coordinates, clock error
C                       and relativistic effect for each epoch of observation 
C       *****************************************************************
C       Input Variables 
C               -filein:name of Rinex navigation file
C               -sat:prn of satellite 
C               -nep:total number of epochs registered in RINEX navigation
C                       file
C               -annio0:year of initial epoch
C               -mes0:month of initial epoch
C               -dia0:day of initial epoch
C               -hora0:hour of initial epoch
C               -min0:minute of initial epoch
C               -sec0:second of initial epoch
C               -nnep:total number of observation epochs to generate
C               -inter:time interval between observations
C               -xo,yo,zo: receiver (center phase antenna) coordinates
C       Output Variables: none
C       *****************************************************************
C       List of SUBROUTINES used
C                posorb(t,toe,m0,sqrta,dn,e,omp,om0,w0,cuc,cus,crc,crs,
C                       ,cic,cis,i0,ip,x,y,z,rel)
C       *****************************************************************
C       *****************************************************************


      SUBROUTINE intorb(filein,sat,nep,annio0,mes0,dia0,hora0,min0
     * ,sec0,nnep,inter,xo,yo,zo)

C Formats
   10 FORMAT(3X,I2.2,4(1X,I2),F5.1,3(D19.12))
   20 FORMAT(22X,3(D19.12))
   30 FORMAT(3X,4(D19.12))
   40 FORMAT(3X,D19.12)
   50 FORMAT(41X,D19.12)
   60 FORMAT(A)
   70 FORMAT(I4,1X,4(I2,1X),F11.8)
   80 FORMAT(3(2X,F13.6),3(2X,F17.8))


C Definition of variables
      IMPLICIT NONE
      CHARACTER filein*30,file1*30,sat*3,salto*1
C       filein=name of Rinex navigation file
C       file1=name of file with ephemeris of satellite # sat
C       sat=satellite prn
C       salto=auxiliar variable to skip lines
      INTEGER nep,annio0,mes0,dia0,hora0,min0,nnep,j,k,annios,mess
     1          ,dias,horas,mins,annio,mes,dia,hora,min,ntoc
C       nep=total number of epochs registered in RINEX navigation
C                       file
C       annio0=year of initial epoch
C       mes0=month of initial epoch
C       dia0=day of initial epoch
C       hora0=hour of initial epoch
C       min0=minute of initial epoch
C       nnep=total number of observation epochs to generate
C       j,k=counter
C       annios=year of epoch recorded in file1
C       mess=month of epoch recorded in file1
C       dias=day of epoch recorded in file1
C       horas=hour of epoch recorded in file1
C       mins=minute of epoch recorded in file1
C       annio,mes,dia,hora,min=observation epoch
C       ntoc=subindex of the nearest ephemeris epoch to observation epoch
      DOUBLE PRECISION sec0,inter,t,t0,x,y,z,clerr,toc(48),secs,sec
     1          ,a0(48),a1(48),a2(48)
     2          ,crs(48),dn(48),m0(48)
     3          ,cuc(48),e(48),cus(48),sqrta(48)
     4          ,toe(48),cic(48),om0(48),cis(48)
     5          ,i0(48),crc(48),w0(48),omp(48),ip(48)
     6          ,tgd(48),aux1,aux2,aux3,aux4,segGPS
     7          ,rel,dif,c
     8          ,tau,umbral,xpr,ypr
     9          ,dist,disant,WE,xo,yo,zo
C       sec0=seconds of initial epoch
C       inter=time interval between epochs
C       t=time elapsed in seconds since begining of GPS week
C       t0=time of first epoch, elapsed in seconds since begining of GPS week
C       x,y,z=satellite coordinates at time t
C       clerr=satellite clock error at time t
C       toc=array of times of clock in seconds since begining of GPS week
C       secs=seconds of epoch of ephemeris in file1
C       sec=seconds of observation epoch
C       a0=array of coeficients a0 for each ephemeris epoch (clock error)
C       a1=array of coeficients a1 for each ephemeris epoch (clock error)
C       a2=array of coeficients a2 for each ephemeris epoch (clock error)
C       crs=array of amplitude of sine armonic correction term to the
C                 orbit radius
C       dn=array of rate of angular velocity of satellite (n)
C       m0=array of mean keplerian orbit anomaly
C       cuc=array of amplitude of cosine armonic correction term to the
C                       argument of perigee 
C       e=array of orbit eccentricity
C       cus=array of amplitude of sine armonic correction term to the
C             argument of perigee
C       sqrta=array of sqrt of semimajor orbit axis
C       toe=array of times of reference
C       cic=array of amplitude of cosine armonic correction term to the
C                       angle of inclination
C       om0=array of right ascention at toe
C       cis=array of amplitude of sine armonic correction term to the
C                       angle of inclination
C       i0=array of inclination angle at reference time
C       crc=array of amplitude of cosine armonic correction term to the
C                       orbit radius
C       w0=array of argument of perigee at toe
C       omp=array of rate of chang of right ascention
C       ip=array  of rate of inclination angle
C       tgd=array of satellite clock error at toe
C       aux1,aux2,aux3,aux4=auxiliar variables to read file1
C       segGPS=function that calculates time in seconds elapsed since 
C               begining of GPS week
C       rel=relativity correction
C       dif=to calculate closest ephemeris epoch to observation epoch
C       c=vacuum light speed
C       tau=time elapsed from satellite emision and receiver reception
C       umbral=threshold for convergence in tau calculus
C       xpr,ypr=coordinates including Sagnac effect
C       dist=satellite-receiver distance
C       distant=satellite-receiver distance in previous step
C       WE=rotational angular velocity of Earth
C       xo,yo,zo=receiver coordinates
      PARAMETER(c=299792458.0D0,WE=7.2921151467D-5)


C Fix convergence threshold in meters
      umbral=1.0D-6

C Open file1 and read relevant information
      file1='sat0'//sat//filein
      OPEN(13,FILE=file1)

      DO j=1,nep
        READ(13,10)annios,mess,dias,horas,mins,secs,aux2,aux3,aux4
        toc(j)=segGPS(annios,mess,dias,horas,mins,secs)
        a0(j)=aux2
        a1(j)=aux3
        a2(j)=aux4
        READ(13,20)aux2,aux3,aux4
        crs(j)=aux2
        dn(j)=aux3
        m0(j)=aux4
        READ(13,30)aux1,aux2,aux3,aux4
        cuc(j)=aux1
        e(j)=aux2
        cus(j)=aux3
        sqrta(j)=aux4
        READ(13,30)aux1,aux2,aux3,aux4
        toe(j)=aux1
        cic(j)=aux2
        om0(j)=aux3
        cis(j)=aux4
        READ(13,30)aux1,aux2,aux3,aux4
        i0(j)=aux1
        crc(j)=aux2
        w0(j)=aux3
        omp(j)=aux4
        READ(13,40)aux1
        ip(j)=aux1      
        READ(13,50)aux3
        tgd(j)=aux3
        READ(13,60)salto


      ENDDO
      CLOSE(13,STATUS='DELETE')

C Calculate observation epochs, for each choose data used to interpolate,
C goto posorb and obtain x,y,z and clock error and group delay
C Not taking into account crossovers for clock correction because we 
C will not accept data so far away for the calculations (if  t-toc>302400)
      file1='sat'//sat//filein
      OPEN(13,FILE=file1)

C Observation epochs:
      DO j=0,nnep
        t0=segGPS(annio0,mes0,dia0,hora0,min0,sec0)
        t=j*inter
        annio=annio0
        mes=mes0
        dia=dia0
        hora=hora0+t/3600
        min=min0+(t-(hora-hora0)*3600)/60
        sec=sec0+t-(hora-hora0)*3600-(min-min0)*60
        t=t+t0

C Nearest toc to t?:
        ntoc=1
        dif=abs(t-toc(1))
        DO k=2,nep
         IF(abs(t-toc(k)).LT.dif) THEN
           dif=abs(t-toc(k))
           ntoc=k
         ENDIF
        ENDDO
        IF(abs(t-toc(ntoc)).GT.7200.0D0) GOTO 500
        WRITE(13,70)annio,mes,dia,hora,min,sec

C Recursive process to calculate tau--> coorrect by tau and Sagnac effect
        k=ntoc
        tau=0.0D0
        dif=9.9D1
        dist=0.0D0
        
  200   CONTINUE
        disant=dist

C Estimate satellite coordinates in geocentric system:
        CALL posorb(t-tau,toe(ntoc),m0(ntoc),sqrta(ntoc),dn(ntoc)
     1    ,e(ntoc),omp(ntoc),om0(ntoc),w0(ntoc),cuc(ntoc),cus(ntoc)
     2    ,crc(ntoc),crs(ntoc),cic(ntoc),cis(ntoc),i0(ntoc),ip(ntoc)
     3    ,x,y,z,rel)

C Change reference system (Sagnac effect)
        xpr=x*DCOS(WE*tau)+y*DSIN(WE*tau)
        ypr=-x*DSIN(WE*tau)+y*DCOS(WE*tau)

C New satellite-receiver distance and threshold
        dist=DSQRT((xpr-xo*1.0D3)**2.0D0+(ypr-yo*1.0D3)**2.0D0
     *          +(z-zo*1.0D3)**2.0D0)   
        tau=dist/c
        dif=abs(dist-disant)

        IF(dif.GT.umbral) GOTO 200      

C a) Correcting by polinomic errors
        clerr=a0(ntoc)+a1(ntoc)*(t-toc(ntoc))
     *      +a2(ntoc)*(t-toc(ntoc))**2.0D0

C b) Correction for using L1. Normaly small values (<0.6D-8),
C in meters 1.8m
C        clerr=clerr-tgd(ntoc)
C Not using them because it seems GPSpace doesn't correct by tgd(ntoc)

C Change coordinate units to km and clock error to meters
        xpr=xpr/1.0D3
        ypr=ypr/1.0D3
        z=z/1.0D3
        clerr=clerr*c

C Record data for its use in subroutine posic
        WRITE(13,80) xpr,ypr,z,clerr,rel
     *  ,(DSQRT((xpr-xo)**2+(ypr-yo)**2+(z-zo)**2)
     *  -DSQRT((x/1.0D3-xo)**2+(y/1.0D3-yo)**2
     *  +(z-zo)**2))*1.0D3

  500   CONTINUE 
      ENDDO
      CLOSE(13)

      RETURN
      END
