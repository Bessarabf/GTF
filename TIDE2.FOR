!    TIDE5x5 - для нейтралов на 5х5 сетке 
!
!    kpars - кол-во параметров, nt - 24 - кол-во моментов времени	
!    30 - высоты, 37- широты, 72 - долготы
!    tide24(kpars,2,nh,its,ids) - глобальные массивы амплитуд и фаз приливов параметров
!                             1- амплитуды    
!                             2 - фазы ( в местном времени)
!
!    sutut(kpars,nh,its,ids),dsutut(kpars,nh,its,ids)	- массивы средних значений параметров
!                                                   	  и дисперсия - осредненные по времени (UT)
!    sutlt(kpars,nh,its,nt),dsutlt(kpars,nh,its,nt) - массивы средних значений параметров
!	                         и дисперсия как функция местного времени - осредненные по долготам  
!    sutj(kpars,nh,its,nt),dsutj(kpars,nh,its,nt)	- массивы средних значений параметров
!	                                                и дисперсия - осредненные по долготам
!
      subroutine TIDE(path,nd1,nd2,kpars,nh,ids,its,nt,npril,dtet,ddolg,
     *ssp)
      integer nd1, nd2, npr,kpars,nh,ids,its,nt,npril
      allocatable pgl(:,:,:,:,:)
      allocatable tide24(:,:,:,:,:),tide12(:,:,:,:,:),tide8(:,:,:,:,:)
      allocatable sutut(:,:,:,:),sutlt(:,:,:,:)
     *	       ,sutj(:,:,:,:),dsutut(:,:,:,:),dsutlt(:,:,:,:)
     *	       ,dsutj(:,:,:,:)
	allocatable rads(:),tut(:),tl(:),tet(:),dolg(:)
	!
      character list*140
	character path*140
      character fn*2
	character fn1*1
	character fn2*2
	character nnc*2
	character nc1*1
	character nc2*2
	character pp*140
      character pp1*140
	integer*2 lstat
      
      kpall=kpars+2  ! Add 2 parameters
      allocate (pgl(kpall,nt,nh,its,ids))
      allocate (tide24(kpall,npril,nh,its,ids)
     *	     ,tide12(kpall,npril,nh,its,ids)
     *	     ,tide8(kpall,npril,nh,its,ids))
      allocate (sutut(kpall,nh,its,ids),sutlt(kpall,nh,its,ids)
     *	,sutj(kpall,nh,its,ids)
     *	,dsutut(kpall,nh,its,ids),dsutlt(kpall,nh,its,ids)
     *	,dsutj(kpall,nh,its,ids))
      allocate (rads(nh),tut(nt),tl(nt),tet(its),dolg(ids))
      
      print*,'TideAnalise'
      do jk = nd1,nd2 ! Цикл по дням
        print*,'current day = ',jk
	  lstat=-1
        call getarg(1,list,lstat)
        write(fn1,10) jk ! Запись номера дня в виде строковой переменной
10	  format(i1)
	  write(fn2,11) jk
11	  format(i2)
	  if (jk.LE.9) then
	    fn="0" // trim(fn1)
	  else 
	    fn=trim(fn2)
     	  end if
        pp=trim(path) // '\' // fn // '\'
        pp1=trim(path) // '\' // 'file4_0' // fn // '.'
	  open(jk, file="TideAnalise\list" // fn // ".txt")
        
        do nc=1,24 ! Для способа считывания в прямом порядке
	  write(nc1,10) nc
          write(nc2,11) nc
	  if (nc.LE.9) then
	    nnc="0" // trim(nc1)
	  else 
	    nnc=trim(nc2)
        end if
        
      ! Выбор способа считывания данных
          if(ssp.EQ.1) then
              write(jk,*) trim(pp1) // nnc ! В прямом порядке
          else
              write(jk,*) trim(pp) // 'file4.' // nnc ! По дням
          endif
         end do
	close(jk)
        list="TideAnalise\list" // fn // ".txt"

    	open(90,file=trim(pp) // 'sutut.dat')
	open(130,file=trim(pp) // 'sutlt.dat')
	open(140,file=trim(pp) // 'sutj.dat')
	open(100,file=trim(pp) // 'tide24.dat')
	open(110,file=trim(pp) // 'tide12.dat')
	open(120,file=trim(pp) // 'tide8.dat')
      
	dt=1. 
	pi = 3.14159
	
        do i=1,its
	  tet(i)=dtet*(i-1)
	enddo
	do j = 1, ids
	  dolg(j)=ddolg*(j-1)
	enddo
	do ntt = 1,nt
	  tut(ntt)=dt*(ntt-1)
	  tl(ntt)=dt*(ntt-1)
      enddo
c     Формируем массив pgl(nt,kpall,nh,its,ids) 
      
      ins=6

      call formPGL_PGI(pgl,rads,kpall,kpars,ins,nt,nh,its,ids,list,pp,
     *             dtet,ddolg)

c	print *,pgl(1,1,5,6,7)
      open(111,file=trim(pp) // 'sutvar-80.dat')
      open(222,file=trim(pp) // 'sutvar-319.dat')
      call testwr(pgl,kpars,nt,nh,its,ids,dolg,tet)
      close(111)
      close(222)
      open(112,file=trim(pp) // 'varint.dat')
      open(113,file=trim(pp) // 'sumdelt.dat')
      call tides(pgl,kpall,nt,nh,its,ids,tide24,tide12,tide8,dolg,dt)
      close(112)
      close(113)
c	print *,tide24(7,1,5,6,7)
	call means(pgl,kpall,nt,nh,its,ids,sutut,sutlt,sutj,
     *	dsutut,dsutlt,dsutj)  
      
      call bongi2(tide24,npril,kpall,nh,its,ids)
      call bongi2(tide12,npril,kpall,nh,its,ids)
      call bongi2(tide8,npril,kpall,nh,its,ids)
      call bongi(sutut,kpall,nh,its,ids)
      call bongi(sutlt,kpall,nh,its,ids)
      call bongi(sutj,kpall,nh,its,ids)
      call bongi(dsutut,kpall,nh,its,ids)
      call bongi(dsutlt,kpall,nh,its,ids)
      call bongi(dsutj,kpall,nh,its,ids)
      
!с  Запись глобальных мсассивов характеристик приливов
	do m = 1,kpall
	  do k = 1, nh
	    do i = 1, its
	      do j = 1 , ids
	         write (90,102) dolg(j),tet(i),rads(k)*1.e-5,m,
     *                       sutut(m,k,i,j),dsutut(m,k,i,j)
	      end do
          end do
	  end do
	end do

	do m = 1,kpall
	  do k = 1, nh
	    do i = 1, its
	      do j = 1 , nt
	        write (130,102) tl(j),tet(i),rads(k)*1.e-5,m
     *                      ,sutlt(m,k,i,j),dsutlt(m,k,i,j)

            end do
          end do
	  end do
	end do

	do m = 1,kpall
	  do k = 1, nh
	    do i = 1, its
	      do j = 1 , nt
	        write (140,102) tut(j),tet(i),rads(k)*1.e-5,m
     *                      ,sutj(m,k,i,j),dsutj(m,k,i,j)
	      end do
          end do
	  end do
	end do
 
	do m = 1,kpall
	  do k = 1, nh
	    do i = 1, its
	      do j = 1 , ids
           	write (100,102)dolg(j),tet(i),rads(k)*1.e-5,m,
     *                      tide24(m,1,k,i,j),tide24(m,2,k,i,j)
			write (110,102)dolg(j),tet(i),rads(k)*1.e-5,m,
     *					  tide12(m,1,k,i,j),tide12(m,2,k,i,j)
			write (120,102)dolg(j),tet(i),rads(k)*1.e-5,m,
     *					  tide8(m,1,k,i,j),tide8(m,2,k,i,j)
            end do
          end do
        end do
	end do
 102	format(3f6.1,i4,1x,1p,2e9.2)
      end do
      close(90)
      close(130)
      close(140)
      close(100)
      close(110)
      close(120)
      deallocate (pgl)
      deallocate (tide24
     *	,tide12
     *	,tide8)
      deallocate (sutut,sutlt
     *	,sutj
     *	,dsutut,dsutlt
     *	,dsutj)
      deallocate (rads,tut,tl,tet,dolg)
      return
      end
c
c
      subroutine testwr(pgl,kpars,nt,nh,its,ids,dolg,tet)
      dimension pgl(kpars,nt,nh,its,ids),dolg(ids),
     *		  tet(its)
      
      k1=1
      k2=24
      m=7
	i=19
	do j = 1,ids
          do kt = 1,nt
              tz1=pgl(m,kt,k1,i,j)
              tz2=pgl(m,kt,k2,i,j)
              write (111,*) dolg(j),kt,tz1
              write (222,*) dolg(j),kt,tz2
          end do
      end do
      return
      end
c
c
c	    TIDES															  
	subroutine tides(pgl,kpars,nt,nh,its,ids,
     *                 tide24,tide12,tide8,dolg,dt)
	dimension pgl(kpars,nt,nh,its,ids),dolg(ids),
     *		  tide24(kpars,2,nh,its,ids),tide12(kpars,2,nh,its,ids),
     *		  tide8(kpars,2,nh,its,ids)
	allocatable f(:),f1(:)
	allocate(f(nt),f1(nt))
      data pi/3.14159/
	
	T= dt*nt ! Период
	om=2.*pi/T 
	do m =1,kpars
	  do k = 1 , nh
	    do i=1,its
	      do j = 1, ids
			do kt = 1,nt
			   f(kt)=pgl(m,kt,k,i,j)
      		end do
			sr=0.
			! mean 
			do kt = 1 , nt
			   sr=sr+f(kt)
			end do
			sr=sr/nt
              testsum=0
			do kt = 1, nt
			   f1(kt)=f(kt)-sr
                 testsum=testsum+f1(kt)
              end do
              if(m.EQ.7) then ! Запись тестовых данных в фиксированной точке
                   if(k.EQ.1) then
                        if(i.EQ.7) then
                            write (113,*) dolg(j),testsum
                        endif
                    endif
                  endif
!с       суточный прилив
			cc1=0.
			ss1=0.
			do kt = 2, nt
                ccx=(cos(om*kt)*f1(kt)+cos(om*(kt-1))*f1(kt-1))*dt/2
                ssx=(sin(om*kt)*f1(kt)+sin(om*(kt-1))*f1(kt-1))*dt/2
			  cc1=cc1+(cos(om*kt)*f1(kt)+cos(om*(kt-1))*f1(kt-1))*dt/2
			  ss1=ss1+(sin(om*kt)*f1(kt)+sin(om*(kt-1))*f1(kt-1))*dt/2
                if(m.EQ.7) then
                    if(k.EQ.1) then
                        if(i.EQ.7) then
                            amp1=sqrt(ccx**2+ssx**2)
                            write (112,*) dolg(j),kt,amp1,f1(kt)
                        endif
                    endif
                  endif
              end do

			cc1=2.*cc1/T
			ss1=2.*ss1/T
			amp=sqrt(cc1**2+ss1**2)
	
			if(cc1==0.) cc1=cc1+0.01 ! На случай нулевого знаменателя
			faz=atan2(ss1,cc1)
			fazt=faz*T/2./pi

c	   фаза прилива в местном времени
			tlt=fazt+dolg(j)/15. ! Пересчёт в местное время
              tlt=-tlt
              if(tlt.lt.-T) tlt=tlt+2*T ! Пересчёт в интервал [0; T]
			if(tlt.lt.0.) tlt=tlt+T
			if(tlt.gt.T) tlt=tlt-T

			tide24(m,1,k,i,j)=amp
			tide24(m,2,k,i,j)=tlt

c    полусуточный прилив
			cc2=0.
			ss2=0.
			do kt = 2, nt
				cc2=cc2+(cos(2.*om*kt)*f1(kt)+cos(2.*om*(kt-1))*
     *			    f1(kt-1))*dt/2
				ss2=ss2+(sin(2.*om*kt)*f1(kt)+sin(2.*om*(kt-1))*
     *     			f1(kt-1))*dt/2
			end do
			cc2=2.*cc2/T
			ss2=2.*ss2/T
			amp=sqrt(cc2**2+ss2**2)
          	if(cc2==0.) cc2=cc2+0.01

			faz=atan2(ss2,cc2)
			fazt=faz*T/2./2./pi
c	   фаза прилива в местном времени
			tlt=fazt+dolg(j)/15.
              tlt=-tlt
			if(tlt.lt.-T) tlt=tlt+T/2
              if(tlt.lt.-T/2) tlt=tlt+T/2
              if(tlt.lt.0.) tlt=tlt+T/2
			if(tlt.gt.T/2) tlt=tlt-T/2
			tide12(m,1,k,i,j)=amp
			tide12(m,2,k,i,j)=tlt

c            8 часовой прилив
			cc3=0.
			ss3=0
			do kt = 2, nt
				cc3=cc3+(cos(3.*om*kt)*f1(kt)+cos(3.*om*(kt-1))*
     *				f1(kt-1))*dt/2
				ss3=ss3+(sin(3.*om*kt)*f1(kt)+sin(3.*om*(kt-1))*
     *				f1(kt-1))*dt/2
			end do

			cc3=2*cc3/T
			ss3=2*ss3/T

			amp=sqrt(cc3**2+ss3**2)
			if(cc3==0.) cc3=cc3+0.01

			faz=atan2(ss3,cc3)
			
			fazt=faz*T/3./2./pi
	
c	   фаза прилива в местном времени
			tlt=fazt+dolg(j)/15.
              tlt=-tlt
			if(tlt.lt.-T) tlt=tlt+T/3.
              if(tlt.lt.-2*T/3) tlt=tlt+T/3.
			if(tlt.lt.-T/3) tlt=tlt+T/3.
			if(tlt.lt.0.) tlt=tlt+T/3.
	        if(tlt.gt.T/3.) tlt=tlt-T/3.
			tide8(m,1,k,i,j)=amp
			tide8(m,2,k,i,j)=tlt
		  end do
	    end do
	  end do 
      end do
	deallocate (f,f1)

	return
	end



c     MEANS
      subroutine means(pgl,kpars,nt,nh,its,ids,sutut,sutlt,sutj,
     *	       dsutut,dsutlt,dsutj) 
	dimension pgl(kpars,nt,nh,its,ids),dolg(ids),tut(nt),tl(nt)
      dimension sutut(kpars,nh,its,ids),sutlt(kpars,nh,its,nt)
	dimension dsutut(kpars,nh,its,ids),dsutlt(kpars,nh,its,nt)
	dimension sutj(kpars,nh,its,nt),dsutj(kpars,nh,its,nt)

c    среднесуточные вариации параметров
c    осреднение по времени
	do m = 1 , kpars
	  do k = 1, nh
	    do i = 1, its
	      do j = 1 ,ids
	        sr=0.
	        do kt = 1, nt
	           sr=sr+pgl(m,kt,k,i,j)
	        end do
	        sutut(m,k,i,j)=sr/nt
		   	sr=0.
	        do kt = 1, nt
	           sr=sr+(pgl(m,kt,k,i,j)-sutut(m,k,i,j))**2
	        end do
	        sr=sqrt(sr)
	        dsutut(m,k,i,j)=sr/nt

            end do
		end do
	  end do
	end do  	  	
!c    среднезональные суточные вариации параметров (местное время)
!с     осреднение по долготам
	ntlt=nt
	do  m = 1 , kpars
	  do  k = 1, nh
		do  i = 1, its
		  do nlt = 1 ,ntlt

	        sr=0.
	        do kt = 1,nt
	           nj=nlt-kt
	           if(nj.lt.0) nj=nj+24
	           j=ids*nj/24+1
	           sr=sr+pgl(m,kt,k,i,j)
	        enddo
	        sutlt(m,k,i,nlt)=sr/nt
          	sr=0.
	        do kt = 1, nt
	           nj=nlt-kt
	           if(nj.lt.0) nj=nj+24
	           j=ids*nj/24+1
	           sr=sr+(pgl(m,kt,k,i,j)-sutlt(m,k,i,nlt))**2

	        enddo
	        sr=sqrt(sr)
	        dsutlt(m,k,i,nlt)=sr/nt
		 enddo
		enddo
	   enddo	
   	end do 

!c    зональноосредненные  суточные вариации параметров
!с	зависимость от UT -  осреднение по долготам


	do 3 m = 1 , kpars
	do 3 k = 1, nh
	do 3 i = 1, its
	do 3 kt = 1 ,nt
	sr=0.
	do j = 1, ids
	sr=sr+pgl(m,kt,k,i,j)
	enddo
	sutj(m,k,i,kt)=sr/ids

	sr=0.
	 do j = 1, ids
	sr=sr+(pgl(m,kt,k,i,j)-sutj(m,k,i,kt))**2
	enddo
	sr=sqrt(sr)
	dsutj(m,k,i,kt)=sr/ids

  3	continue

	return
	end

	subroutine formPGL_PGI(pgl,rads,kpall,kpars,ins,nt,nh,its,ids,list,pp,
     *                       dtets,ddolgs)
	character pp*140
!	character path*120
	include 'kdf_kdu.inc' 

      character list*80 ! file name with f4-file listing
	character fname*80
	
	dimension pgl(kpars+2,nt,nh,its,ids),kdf(20),kdu(20),rads(nh)
	allocatable pglS(:,:,:,:),gins(:,:,:,:)
	allocate(pglS(kpars,nh,its,ids),gins(ins,nh,its,ids))
    
    	open(66,file=list,err=10)
	data Pi/3.14159/
	nfile=5
	npgl=kpars*nh*its*ids
	kdf=kdf5 
	kdu=kdu5
	ldor=4096
      nk=16     ! altitude point for tube
	!
!	dtets=180.0/(its-1)
!	ddolgs=360.0/ids
c	Tp=24
c	Om=2.*Pi/Tp
c	  волновое число
c      wav=2.

	do iread=1,nt
	   read(66,'(a80)',end=11) fname
	   write(*,'(1x,a80)') fname
	   open(5,file=fname,status='old',access='direct',
     *           form='unformatted',recl=ldor)
         call  globrw(pglS,rads,kpars,ldor,
     *                   nh,kdf,isp,npgl,its,ids)
	! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   !      print*,ins,nh,its,ids
         npins=ins*nh*its*ids
	   call globGINS(gins,rads,ins,ldor,
     *                 nh,kdf,kdu,isp,npins,its,ids)
	
         do i=1,its
           do j=1,ids
              gins(1,1:nk,its,ids)=pglS(6,1:nk,its,ids) ! заполняем массивы до 175 км
              gins(6,1:nk,its,ids)=pglS(8,1:nk,its,ids)
           end do
         end do
     	open(99,file=trim(pp) // 'surf1.dat')
      call write_surf(pglS,nh,its,ids,kpars,dtets,ddolgs)
	close(99)
	
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          call filegeo(pglS,nh,its,ids,kpars,dtets,ddolgs,
     *                 rads)
          call filegeo(gins,nh,its,ids,ins,dtets,ddolgs,
     *                 rads)
          do np=1,kpars
           call bospgl(pglS,kpars,nh,its,ids,np)
         enddo
         Do np=1,ins
           call bospgl(gins,ins,nh,its,ids,np)
         enddo
!	! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      open(991,file=trim(pp) // 'surf-1.dat') 
      open(992,file=trim(pp) // 'surf-10.dat') 
      open(993,file=trim(pp) // 'surf-14.dat') 
      open(994,file=trim(pp) // 'surf-24.dat') 
      call write_surf(pglS,nh,its,ids,kpars,dtets,ddolgs)
	close(991)    
      close(992)    
      close(993)    
      close(994)    
      
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      do  m = 1 , kpars
        do  k = 1 , nh
	    do  j = 1, ids
	      do  i = 1 , its
		     pgl(m,iread,k,i,j)= pglS(m,k,i,j)
            end do
          end do
	   end do
      end do
      im=1
      do  m = 20 , 21
         do  k = 1 , nh
	    do  j = 1, ids
	      do  i = 1 , its
		     pgl(m,iread,k,i,j)= gins(im,k,i,j) ! ne&Ti

            end do
          end do
         end do
         im=6
	 end do
	close(5)
	end do
	DEALLOCATE(pglS,gins) 
      return
10 	print*,'not found list f4-files',list 
	stop
11	print*,'too small list of f4-files, nt=',iread,', must equal',nt
      DEALLOCATE(pglS,gins)
      stop
	end

	 
! исправлено ноябрь 2004
! nfile - логический  номер файла - 4 или 5, описан в open
! readfl - логическое условие запись в файл (false)или чтение (truth)
! pgl1 - массив размерностью kpar*nh*its*ids
! nh - число узлов по высоте
! ids - число узлов по долготе
! its -число узлов по широте
! mass- массив управляющих параметров
! ldor - длина записи - 4096 (4*1024) байта
! kdf - массив с номерами начальных записей. Помогает выделить параметры шара, трубки и т.д.
!  
      subroutine globrw(pgl1,rads,kpar,ldor,
     *                  nh,kdf,isp,npgl,its,ids)
!      include 'parametr.inc'
      dimension pgl1(npgl),kdf(20),pole(ldor/4),rads(NH)
      
!
 
!      print *,' globrw - begin'
      npg=kpar*nh*its*ids
! проверка соответствия длины массива и количества параметров. 
! Необходимо для соответствия версий File4
      if (npgl.lt.npg) then 
!        print 700,npg,npgl
700     format (' globrw :   ********  error  ********'/
     *   '  npg=',i8,'  >   npgl=',i8,'  !!!!!!  STOP  !')
        stop
      end if  
      
! чтение 1-й записи и массива высот
      read(5,rec=1) pole
	rads=pole(106:135)

! номер начальной записи ШАРА

      isp=kdf(5)+1
      mdor=ldor/4     ! число вещественных значений в записи (4 байта на число)
      ndor=npgl/mdor  ! число записей (ndor - число элементов в 4х мерном массив 
      nost=npgl-ndor*mdor      ! остаток
                      ! чтение  
         k=1
         do j=1,ndor
           read(5,rec=isp) pole
           do i=1,mdor
             pgl1(k)=pole(i)
             k=k+1
           end do 
           isp=isp+1
         end do 
         if(nost.NE.0) then
           read(5,rec=isp)pole
           do i=1,nost            ! читаем остаток
             pgl1(k)=pole(i)
             k=k+1
           end do 
         end if
      
  !    print *, 'globrw - end'
      return
      end

	
	 
		subroutine write_surf(pglS,nh,its,ids,kpars,dtets,ddolgs)
	  
		dimension pglS(kpars,nh,its,ids),kdf(20),kdu(20),rads(nh)
	  tet=90.
	  do i=1,its
	    dolg=0
		do j=1,ids
		  write(991,*) dolg,tet,pglS(2,1,i,j)
            write(992,*) dolg,tet,pglS(2,10,i,j)
            write(993,*) dolg,tet,pglS(2,14,i,j)
            write(994,*) dolg,tet,pglS(2,24,i,j)

		  dolg=dolg+ddolgs
		end do
		tet=tet-dtets
	  end do 
	  return

          end

          