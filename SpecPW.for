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
      call PWJJ(path,nd1,nd2,its,nh,nz,npr,h)
	stop
	end
		
	subroutine PWJJ(path,nd1,nd2,its,nh,nz,npr,h)
      integer nd1, nd2, npr,kpars,nh,ids,its,nt,npril,h(5),h1,gg
      integer nz(7)
      dimension stet(its),amp(5,nd2,nh,its)
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
      character nhh*2
	character nh1*1
	character nh2*2
	character nnc*2
	character nc1*1
	character nc2*2
	character pp*200
	character njj*2
      character nj1*1
      character nj2*2
      
      do i = 1 , its
		stet(i)=90.-5*(i-1)
      enddo
    
	do ng=1,5

      write(ng1,10) ng

10	format(i1)
11	format(i2)

      path1=trim(path) // 'pw' // trim(ng1) // '\'
      
	do jk = nd1,nd2

      write(fn1,10) jk
	write(fn2,11) jk
	if (jk.LE.9) then
	fn="0" // trim(fn1)
	else 
	fn=trim(fn2)
     	end if

	pp=trim(path1) // 'PW' // trim(ng1) // '-' // trim(fn) // '.dat'
	
!      print*,pp

	open(jk, file=pp)
      
      do k=1,1 !nh
          do j=1,its
              read(jk,*) a,stet(j),c,d,amp(ng,jk,k,j)
          enddo
		enddo
      close(jk)
      enddo
	enddo
	
	AM=1 !E-12
      if(npr.EQ.10) AM=0.01 ! Пересчёт в м/с
	if(npr.EQ.11) AM=0.01 ! Пересчёт в м/с
	if(npr.EQ.12) AM=0.01 ! Пересчёт в м/с
      if(npr.EQ.20) AM=1E-4 ! Пересчёт в m**(-3) для электронной концентрации 


	do k=1,5
	if(h(k).EQ.0) goto 3

	write(nh1,10) h(k)
	write(nh2,11) h(k)
	
      if (h(k).LE.9) then
	nhh="0" // trim(nh1)
	else 
	nhh=trim(nh2)
     	end if
     	
	do j=1,7

	if(nz(j).EQ.0) goto 2
	write(nj1,10) nz(j)
	write(nj2,11) nz(j)
	
      if (nz(j).LE.9) then
	njj="0" // trim(nj1)
	else 
	njj=trim(nj2)
     	end if

  
	open(100,file=trim(path) // 'specpw\pwsp-' // trim(njj) // '-h-'// 
	*trim(nhh) // '.dat') 
   
      do n=nd1,nd2
!		write(100,*) n,0,0
          do ng=1,5
              write(100,*) n,ng,amp(ng,n,h(k),nz(j))*AM
          enddo
!		write(100,*) n,6,0
      enddo
      
      close(100)
2	enddo
3	enddo

22	return
      end
