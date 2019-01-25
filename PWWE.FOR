!c              PWWE - программа для выделения модовых вкладов планетарных волн с заданными 
!                 зональными волновыми числами
!c	  
!с        
!с    np - 19- кол-во параметров, nt - 24 - кол-во моментов времени	
!с    30 - высоты, 37- широты, 24 - долготы
!	  tide24(np,2,nh,its,ids) - глобальные массивы амплитуд и фаз приливов параметров
!		1- амплитуды    2 - фазы ( в местном времени)
!
!	  sutut(np,nh,its,ids),dsutut(np,nh,its,ids)	- массивы средних значений параметров
!	  и дисперсия - осредненные по времени (UT)
!
! 
!	  sutlt(np,nh,its,nt),dsutlt(np,nh,its,nt)	- массивы средних значений параметров
!	и дисперсия как функция местного времени - осредненные по долготам  
!
!
! 	  sutj(np,nh,its,nt),dsutj(np,nh,its,nt)	- массивы средних значений параметров
!	и дисперсия - осредненные по долготам
!

!с	   PAR24(its,ids,2)
!с	   PAR12(its,ids,2)
!	   PAR8(its,ids,2)
!	   PARut(its,ids,2)
!	   PARlt(its,ids,2)
!        PARj(its,ids,2)
!
!
!	   
!с
	subroutine PWWE(path,nd1,nd2,ng,kpars,nh,ids,its,nt)
      dimension pars(8)
  	dimension r(nh),stet(its)
	character path*120
	character pp*140
	character vc*1
	character jj*4
	character j2*4
	character j0*4
	integer j
      print*,'PWWE'
	ndd=nd2-nd1+1  
      
	do k=1,5
          print*,'current mode = ',k
	write(vc,10) k

	pp=trim(path) // 'pw' // vc // '\'

	open(125,file=trim(pp) // 'PW0.dat')
	open(121,file=trim(pp) // 'PWest.dat')
      open(122,file=trim(pp) // 'PEast.dat')
	open(123,file=trim(pp) // 'PWh1.dat')
    	open(124,file=trim(pp) // 'PWh2.dat')

	do j=nd1,nd2
	write(jj,10) j
10	format(i1)
	write(j2,11) j
11	format(i2)
	if (j.LE.9) then
	j0="0" // trim(jj)
	else 
	j0=trim(j2)
     	end if
	open(j,file=trim(pp) //'PW' // vc // '-' // trim(j0) // '.dat')
	end do

	ngar=10

	do i = 1 , its
	stet(i)=90.- 5*(i-1)
	enddo
	
!c           Чтение массивов амплитуд и фаз гармоник


	call RW1(nd1,nd2,ndd,nh,its,stet,r,rng,npr,ngar,ng)
 	end do
!         Спектр планетарных волн
	return
      end
	
      subroutine RW1(nd1,nd2,ndd,nh,its,stet,r,rng,npr,ngar,ng)
	dimension am(ndd,nh,its),ph(ndd,nh,its), stet(its), r(nh)
	dimension  ame(10,nh,its),phe(10,nh,its)
     *,amw(10,nh,its),phw(10,nh,its),p0(2,nh,its) 

	rng=ng
      do k = 1 , nh
	  do i = 1 , its
		pl=1
	  	do j=1,ndd
              ndt=nd1+j-1
		read(ndt,*)rng,stet(i),r(k),npr,am(pl,k,i),ph(pl,k,i)
		pl=pl+1
		end do
        enddo
	enddo

           call pwwestr(ng,its,ndd,nh,ngar,am,ph,amw,phw,ame,phe,p0)

c		       запись в файл
            do 34 nsg=1, ngar


		rnsg=nsg
		per=ndd/rnsg
            do 34 k =1,nh
	           do 34 i = 1 , its
		write(121,203)per,stet(i),r(k),npr,amw(nsg,k,i),phw(nsg,k,i)
		write(122,203)per,stet(i),r(k),npr,ame(nsg,k,i),phe(nsg,k,i)
 		write(125,204)stet(i),r(k),npr,p0(1,k,i),p0(2,k,i)
	
 34     continue

		do 35 m = 1, ngar
		rm=m
		per=ndd/rm
		do 35 i = 1, its
	write(123,103)per,stet(i),r(10),npr,amw(m,10,i),phw(m,10,i)
     *,ame(m,10,i),phe(m,10,i)
	write(124,103)per,stet(i),r(12),npr,amw(m,12,i),phw(m,12,i)
     *,ame(m,12,i),phe(m,12,i)
	
  35	continue

103	format(3f6.0,i4,2p,2x,e9.2,2x,e9.2,2x,e9.2,2x,e9.2)
203	format(3f6.0,i4,2x,e9.2,2x,e9.2)
204	format(2f6.0,i4,2x,e9.2,2x,e9.2)
 102	format(3f6.1,i4,1p,2e9.2)

 	close(125)
	close(121)
      close(122)
	close(123)
    	close(124)

	return
	end  

      subroutine pwwestr(ng,its,ndd,nh,ngar,am,ph,amw,phw,ame,phe,p0)
      dimension am(ndd,nh,its),ph(ndd,nh,its)
     *,amw(ngar,nh,its),phw(ngar,nh,its),ame(ngar,nh,its),
     *phe(ngar,nh,its)
     *,fc(40),fs(40),p0(2,nh,its) 

        pi=3.14152
              
        ar=2*pi/ndd
        do 22 i = 1, its
        do 22 k = 1,nh 
        do 11 nd = 1,ndd
       fc(nd)=am(nd,k,i)*cos(ng*pi*ph(nd,k,i)/180.)
       fs(nd)=am(nd,k,i)*sin(ng*pi*ph(nd,k,i)/180.)

  11    continue
        s=0.
        s1=0.
       do nd=1,ndd
        s=s+fc(nd)
        s1=s1+fs(nd)
       enddo
        s=s/ndd
        s1=s1/ndd
        am0=sqrt(s**2+s1**2)
         s3=s+0.1

	

	 if(abs(s1).lt.0.01) s1=0.

        ph0=atan2(s1,s3)*180./pi    
         if(ph0.lt.0.)ph0=ph0+360./ng
         p0(1,k,i)=am0
         p0(2,k,i)=ph0
        do 20 nsg=1, ngar
        ga1=0.
        gb1=0.
        ga2=0.
        gb2=0. 
         do nd=1,ndd
          ga1=ga1+(fc(nd)-s)*cos(2*pi*nsg*nd/ndd)
          gb1=gb1+(fc(nd)-s)*sin(2*pi*nsg*nd/ndd)
          ga2=ga2+(fs(nd)-s1)*cos(2*pi*nsg*nd/ndd)
          gb2=gb2+(fs(nd)-s1)*sin(2*pi*nsg*nd/ndd)

           enddo
          ga1=ga1*2/ndd
          gb1=gb1*2/ndd
          ga2=ga2*2/ndd
          gb2=gb2*2/ndd
          D=0.5*(ga1-gb2)
          E=0.5*(ga2+gb1)
          C=0.5*(ga1+gb2)
          G=0.5*(gb1-ga2)
          amw(nsg,k,i)=sqrt(D**2+E**2) 
          ame(nsg,k,i)=sqrt(C**2+G**2)
            D=D+0.1
            C=C+0.1


            if(abs(b1).lt.0.01) E=0.
            if(abs(b2).lt.0.01) G=0.
          phw(nsg,k,i)= atan2(E,D)*180./pi
          if(phw(nsg,k,i).lt.0) phw(nsg,k,i)=phw(nsg,k,i)+360./ng
          phe(nsg,k,i)= atan2(G,C)*180./pi
           if(phe(nsg,k,i).lt.0) phe(nsg,k,i)=phe(nsg,k,i)+360./ng

  20      continue
  22      continue

			return
            end
