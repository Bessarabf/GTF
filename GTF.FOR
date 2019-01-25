!	PARAMETER (kpars=19,nh=30,ids=72,its=37,nt=24,npril=2,
!     *            dtet=5.,ddolg=5.)

	character*140 path
	integer nd1, nd2, npr,kpars,nh,ids,its,nt,npril 
	integer h(4), ps(5), nz(7)
      dimension pars(8)

	open(100, file='info.txt')
      read(100,*) kpars,nh,ids,its,nt,npril,dtet,ddolg ! Параметры
	read(100,*) ps(1), ps(2), ps(3), ps(4), ps(5) ! Признаки запуска
      read(100,*) path					   ! Путь к папке с данными
	read(100,*) nd1, nd2				   ! Номера первого и последнего дней
	read(100,*) npr						   ! Номер параметра
	read(100,*) nz(1),nz(2),nz(3),nz(4),nz(5),nz(6),nz(7) ! Номера широтных точек
	read(100,*) h(1), h(2), h(3), h(4)	   ! Номера высотных точек
	close(100)
      
      if(npr.EQ.22) then
          nnp=1 ! Зональное поле
          npr=1
      endif
      
      if(npr.EQ.23) then
          nnp=2 ! Меридиональное поле
          npr=2
      endif
      
      if(npr.EQ.24) then
          nnp=3 ! TEC
          npr=3
      endif
      
      if(nnp.EQ.1.OR.nnp.EQ.2.OR.nnp.EQ.3) then
          nh=1
      endif
      
      print*,kpars,nh,ids,its,nt,npril,dtet,ddolg      
      
      if(nnp.EQ.1.OR.nnp.EQ.2.OR.nnp.EQ.3) then
          if(ps(1).EQ.1) then
		    call TIDE_alt(path,nd1,nd2,kpars,nh,ids,its,nt,npril,dtet,
     *ddolg,nnp)
          endif
      else
      if(ps(1).EQ.1) then
		call TIDE(path,nd1,nd2,kpars,nh,ids,its,nt,npril,dtet,ddolg)
      endif
      endif
      kpars=kpars+2
	call startpr(ps,path,nd1,nd2,npr,nz,h,kpars,nh,ids,its,nt,nnp)

	stop
	end
	
     	
	subroutine startpr(ps,path,nd1,nd2,npr,nz,h,kpars,nh,ids,its,nt,
     *nnp)
	integer h(4), ps(5), nz(7)
      dimension pars(8)
	character*140 path
	character*150 path2
	character fn1*2
	character fn2*2
	character pn*5
	integer nd1, nd2, npr

10	format(i1)
11	format(i2)

	write(fn1,10) npr	   ! Определение папки запсии по номеру параметра
	write(fn2,11) npr
	if (npr.LE.9) then
	pn="par0" // trim(fn1)
	else 
	pn="par" // trim(fn2)
     	end if

	path2=trim(path) // '\' // trim(pn) // '\' 
      if(nnp.EQ.1.OR.nnp.EQ.2.OR.nnp.EQ.3) then
          path=trim(path) // '\2009\01\' ! Для данных от GSMTIP Viewer
          path2=trim(path) // trim(pn) // '\'
      endif
      
	if(ps(2).EQ.1) then
		call PWSTR(npr,path,path2,nd1,nd2,kpars,nh,ids,its,nt,nnp)
	endif

	if(ps(3).EQ.1) then
		call HPW(path2,nd1,nd2,nz,h,kpars,nh,ids,its,nt,nnp)
	endif
	
	if(ps(4).EQ.1) then
		do i=1,7
			call PWWE(path2,nd1,nd2,i,kpars,nh,ids,its,nt)
		enddo	
	endif 

	if(ps(5).EQ.1) then
		do i=1,4
             if(h(i).NE.0) then
			call TIDESTR(path,path2,nd1,nd2,npr,h(i),kpars,nh,ids,its,nt,nnp)
             endif
		enddo
	endif
	

	return
	end
	
