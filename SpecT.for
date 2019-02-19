!	PARAMETER (kpars=19,nh=30,ids=72,its=37,nt=24,npril=2,
!     *            dtet=5.,ddolg=5.)

	character*200 path
      character*1 np1
      character*2 np2
      character*2 npp
	integer nd1, nd2, npr,kpars,nh,ids,its,nt,npril 
	integer h(5), ps(5), nz(7),ng
      dimension pars(8)

	open(100, file='info.txt')
      read(100,*) kpars,nh,ids,its,nt,npril,dtet,ddolg ! Параметры
	read(100,*) ps(1), ps(2), ps(3), ps(4), ps(5) ! Признаки запуска
      read(100,*) path					   ! Путь к папке с данными
	read(100,*) nd1, nd2				   ! Номера первого и последнего дней
	read(100,*) npr						   ! Номер параметра
	read(100,*) nz(1),nz(2),nz(3),nz(4),nz(5),nz(6),nz(7) ! Номера широтных точек
	read(100,*) h(1), h(2), h(3), h(4), h(5)	   ! Номера высотных точек
	close(100)
      
      write(np1,10) npr
	write(np2,11) npr
      if (npr.LE.9) then
	npp='0' // trim(np1)
      else 
	npp=trim(np2)
      end if
      
10	format(i1)
11	format(i2)
      
      path=trim(path) // '\par' // trim(npp) // '\'
      do nnj=1,5
          do ng=1,3
!			print*,path,nd1,nd2,its,h,nnj,ng,npr
!			pause
              call TideJJ(path,nd1,nd2,its,h,nz(nnj),ng,npr)
          enddo
      enddo
	stop
	end
		
	subroutine TideJJ(path,nd1,nd2,its,h,nj,ng,npr)
!      PARAMETER (kpars=19,nh=30,ids=72,its=37,nt=24,npril=2,
!     *            dtet=5.,ddolg=5.)
      integer nd1, nd2, npr,kpars,nh,ids,its,nt,npril,h(5),h1,gg
      integer stet(its),mg(11)
      dimension amp(nd2,11,its)
      real AM
      character list*140
	character path*200
      character path1*200
      character ngg*2
      character ng1*1
      character ng2*2
      character fn*2
	character fn1*1
	character fn2*2
      character hh*3
	character hh1*1
	character hh2*2
	character nnc*2
	character nc1*1
	character nc2*2
	character pp*200
	character njj*2
      character nj1*1
      character nj2*2

      if(nj.EQ.0) goto 22
      
	do i = 1 , its
		stet(i)=90.-5*(i-1)
      enddo

      do i = 1 , 11
		mg(i)=i-6
      enddo

      
      if(ng.EQ.1) then 
         gg=24
      endif
      
      if(ng.EQ.2) then 
         gg=12
      endif
      
      if(ng.EQ.3) then
         gg=8
      endif
      

      nn=ng+6
     
      write(ng1,10) gg
	write(ng2,11) gg
	if (gg.LE.9) then
	ngg=trim(ng1)
      else 
	ngg=trim(ng2)
      end if

	write(nj1,10) nj
	write(nj2,11) nj
	if (nj.LE.9) then
	njj=trim(nj1)
	else 
	njj=trim(nj2)
      end if

      do nh=1,5
      h1=h(nh)
      if(h1.EQ.0) goto 22
 !     print*,h1
      write(hh1,10) h1
10	format(i1)
	write(hh2,11) h1
11	format(i2)
	if (h1.LE.9) then
	hh="h0" // trim(hh1)
	else 
	hh="h" // trim(hh2)
      end if
      
      path1=trim(path) // hh // '\'
      
	do jk = nd1,nd2

      write(fn1,10) jk
	write(fn2,11) jk
	if (jk.LE.9) then
	fn="0" // trim(fn1)
	else 
	fn=trim(fn2)
     	end if

	pp=trim(path1) // '\spec' // trim(ngg) // '-' // fn // '.dat'
	
!      print*,pp

	open(jk, file=pp)
      
      do nng=1,11
          do j=1,its
              read(jk,*) a,b,c,d,amp(jk,nng,j)
          enddo
      enddo
      close(jk)
      enddo
      open(100,file=trim(path1) // 'spect\tsp-'// trim(ngg)// '-' 
	*// trim(njj) // '.dat')
      
	AM=1 !E-12
      if(npr.EQ.10) AM=0.01 ! Пересчёт в м/с
	if(npr.EQ.11) AM=0.01 ! Пересчёт в м/с
	if(npr.EQ.12) AM=0.01 ! Пересчёт в м/с
      if(npr.EQ.20) AM=1E-4 ! Пересчёт в m**(-3) для электронной концентрации 

      do n=nd1,nd2
          do i=1,11
              write(100,*) n,mg(i),amp(n,i,nj)*AM
          enddo
      enddo
      
      close(100)
      enddo
22	return
      end