!    Вариант для расчёта с данными от GSMTIP Viewer
      
      subroutine TIDE_alt(path,nd1,nd2,kpars,nh,ids,its,nt,npril,dtet,
     *ddolg)
      integer nd1, nd2, npr,kpars,nh,ids,its,nt,npril
      allocatable pgl(:,:,:,:,:)
      allocatable pgle(:,:,:,:,:)
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
      character k0*2
	character k1*1
	character k2*2
	character nnc*2
	character nc1*1
	character nc2*2
	character pp*140
      character pp1*140
      character p0*140
      character pk*140
      character tt*1
	integer*2 lstat
      kpall=3
      kpars=3
      allocate (pgl(kpall,nt,nh,its,ids))
      allocate (pgle(kpall,nt,nh,its,ids))
      allocate (tide24(kpall,npril,nh,its,ids)
     *	     ,tide12(kpall,npril,nh,its,ids)
     *	     ,tide8(kpall,npril,nh,its,ids))
      allocate (sutut(kpall,nh,its,ids),sutlt(kpall,nh,its,ids)
     *	,sutj(kpall,nh,its,ids)
     *	,dsutut(kpall,nh,its,ids),dsutlt(kpall,nh,its,ids)
     *	,dsutj(kpall,nh,its,ids))
      allocate (rads(nh),tut(nt),tl(nt),tet(its),dolg(ids))
      
      print*,'TideAnalise'
      print*,kpall,nt,nh,its,ids

      do jk = nd1,nd2 ! Цикл по дням

        print*,'current day = ',jk
	  lstat=-1
        call getarg(1,list,lstat)
        write(fn1,10) jk
10	  format(i1)
	  write(fn2,11) jk
11	  format(i2)
	  if (jk.LE.9) then
	    fn="0" // trim(fn1)
	  else 
	    fn=trim(fn2)
        end if
       p0=trim(path) // '\' // trim(fn) // '\quiet\' ! Путь к папке с данными за день
       do nnp=1,3 ! Цикл по номеру параметра
       do k=1,24	  ! Цикл по часам  
     	 write(k1,10) k ! Запись номера часа в форме строковой переменной
	 write(k2,11) k
	 if (k.LE.9) then
	 k0="0" // trim(k1)
	 else 
	 k0=trim(k2)
       end if
       
       if(nnp.EQ.1) then
	 pk=trim(p0)//'surface_file4.'//trim(k0)//'\'  
     *//'ev,mv#m geo LON_LAT 175 0 0.dat' ! Путь к файлу с данными
       endif
       
       if(nnp.EQ.2) then
	 pk=trim(p0)//'surface_file4.'//trim(k0)//'\'  
     *//'eu,mv#m geo LON_LAT 175 0 0.dat'
       endif
       
       if(nnp.EQ.3) then
	 pk=trim(p0)//'surface_file4.'//trim(k0)//'\'  
     *//'lg tec geo LON_LAT 20200 0 0.dat'
       endif
      
      open(55,file=trim(pk))
      read(55,*) tt
      do i=1,ids
          do j=1,its
              read(55,*) a,b,pgle(nnp,k,1,j,i)
              if(nnp.EQ.3) then
              pgle(nnp,k,1,j,i)=10**pgle(nnp,k,1,j,i)
              endif
          enddo
      enddo
      close(55)
       enddo
       enddo
      pp=trim(path) // '\' // trim(fn) // '\'
      
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

 !     call formPGL_PGI(pgl,rads,kpall,kpars,ins,nt,nh,its,ids,list,pp,
 !    *             dtet,ddolg)

c	print *,pgl(1,1,5,6,7)
   !   open(111,file=trim(pp) // 'sutvar-80.dat')
   !   open(222,file=trim(pp) // 'sutvar-319.dat')
   !   call testwr(pgl,kpars,nt,nh,its,ids,dolg,tet)
   !   close(111)
   !   close(222)
   !   open(112,file=trim(pp) // 'varint.dat')
   !   open(113,file=trim(pp) // 'sumdelt.dat')
      call tides(pgle,kpall,nt,nh,its,ids,tide24,tide12,tide8,dolg,dt)
   !   close(112)
   !   close(113)
c	print *,tide24(7,1,5,6,7)
	call means(pgle,kpall,nt,nh,its,ids,sutut,sutlt,sutj,
     *	dsutut,dsutlt,dsutj)  
      
!      call bongi2(tide24,npril,kpall,nh,its,ids)
!      call bongi2(tide12,npril,kpall,nh,its,ids)
!      call bongi2(tide8,npril,kpall,nh,its,ids)
!      call bongi(sutut,kpall,nh,its,ids)
!      call bongi(sutlt,kpall,nh,its,ids)
!      call bongi(sutj,kpall,nh,its,ids)
!      call bongi(dsutut,kpall,nh,its,ids)
!      call bongi(dsutlt,kpall,nh,its,ids)
!      call bongi(dsutj,kpall,nh,its,ids)
      
!с  Запись глобальных мсассивов характеристик приливов
      rads(1)=0
!      print*,rads
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
 102  format(3f6.1,i4,1x,1p,2e9.2)
      end do

      close(90)
      close(130)
      close(140)
      close(100)
      close(110)
      close(120)
      deallocate (pgl)
      deallocate (pgle)
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