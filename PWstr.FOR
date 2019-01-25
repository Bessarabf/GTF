!c              PWstr	5x5
!c	  
!с        
!с    np - 19- кол-во параметров, nt - 24 - кол-во моментов времени	
!с    30 - высоты, 37- широты, 72 - долготы
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

	subroutine PWSTR(npr,path,path2,nd1,nd2,kpars,nh,ids,its,nt,nnp)
      allocatable sutut(:,:,:,:),sutlt(:,:,:,:),
     *        	sutj(:,:,:,:), dsutut(:,:,:,:),
     *            dsutlt(:,:,:,:),dsutj(:,:,:,:),
     *	        dolg(:),tet(:),r(:),stet(:),tl(:),tut(:),
     *	        sg(:), pw(:,:,:),spw(:,:,:,:),sspw(:,:)

      dimension pars(8)
	character path*140
	character path2*150
	character fn1*2
	character fn2*2
	character fn*2
	character pp*140
	character p0*140
	character p1*140
	character p2*140
	character p3*140
	character p4*140
	character p5*140
      allocate (sutut(kpars,nh,its,ids),sutlt(kpars,nh,its,nt),
     *        	sutj(kpars,nh,its,nt), dsutut(kpars,nh,its,ids),
     *            dsutlt(kpars,nh,its,nt),dsutj(kpars,nh,its,nt),
     *	        dolg(ids),tet(its),r(nh),stet(its),tl(nt),tut(nt),
     *	        sg(5), pw(nh,its,ids),spw(2,5,nh,its),sspw(nh,its))
      
	print*,'PWSTR'
     	do jk = nd1,nd2 
      print*,'current day = ',jk
	write(fn1,10) jk ! Запись номера дня строковой переменной
10	format(i1)
	write(fn2,11) jk
11	format(i2)
	if (jk.LE.9) then
	fn="0" // trim(fn1)
	else 
	fn=trim(fn2)
     	end if

	pp=trim(path) // '\' // trim(fn) // '\'
	p0=trim(path2) // 'pw0' // '\' // 'PW0-' // trim(fn)
	p1=trim(path2) // 'pw1' // '\' // 'PW1-' // trim(fn)
	p2=trim(path2) // 'pw2' // '\' // 'PW2-' // trim(fn)
	p3=trim(path2) // 'pw3' // '\' // 'PW3-' // trim(fn)
	p4=trim(path2) // 'pw4' // '\'	// 'PW4-' // trim(fn)
	p5=trim(path2) // 'pw5' // '\' // 'PW5-' // trim(fn)
	
	open(9,file=trim(pp) // 'sutut.dat')
	open(13,file=trim(pp) // 'sutlt.dat')
	open(14,file=trim(pp) // 'sutj.dat')

      open(30,file=trim(p0) // '.dat')
	open(31,file=trim(p1) // '.dat')
	open(32,file=trim(p2) // '.dat')
	open(33,file=trim(p3) // '.dat')
	open(34,file=trim(p4) // '.dat')
	open(35,file=trim(p5) // '.dat')
	
	open(39,file=trim(pp) // 'PWS10.dat')
	open(40,file=trim(pp) // 'PWS12.dat')
	open(41,file=trim(pp) // 'PWS20.dat')

      np=npr
      
      if(nnp.EQ.1.OR.nnp.EQ.2.OR.nnp.EQ.3) then 
          nh=1 ! На случай двумерных параметров от GSMTIP Viwer
          np=nnp
      endif
      
	do i = 1 , its
	   stet(i)=90.-5*(i-1)
      enddo
!c           Чтение глобальных массивов параметров
	do m = 1 , np
        do k = 1 , nh
	    do i = 1 , its
	      do j = 1 , ids
		    read (9,*) dolg(j),tet(i),r(k),m0,sutut(m,k,i,j)
     *           ,dsutut(m,k,i,j)
	        if(m.eq.npr)pw(k,i,j)=sutut(m,k,i,j)
              
            end do
          enddo
        enddo
      enddo
      print*,pp
      do m = 1 , np
        do k = 1 , nh
	    do i = 1 , its
	      do ntt=1,nt
             read (13,*) tl(ntt),tet(i),r(k),m0,sutlt(m,k,i,ntt)
     *            ,dsutlt(m,k,i,ntt)
	       !print*,m,R(k),i,j
	       read (14,*) tut(ntt),tet(i),r(k),m0,sutj(m,k,i,ntt)
     *                      ,dsutj(m,k,i,ntt)
            enddo
	    enddo
	  enddo
        enddo
  !   	print *,dolg(1),tet(5),r(nh),npr,sutut(npr,nh,5,1),
   !  *         sutlt(npr,nh,5,1)
   !   pause
	

!         Спектр планетарных волн   
            
!         спектральные гармоники
      ng=5
      do nsg= 1, ng ! Массив номеров гармоник
         sg(nsg)=nsg  
      enddo          

      call strucpw(its,ids,nh,sg,ng,pw,spw,sspw,np)
c		       запись в файл
c           do 34 nsg=1, ng
      do 34 k =1,nh
        do 34 i = 1 , its
          write(30,204)stet(i),r(k),npr,sspw(k,i)
          write(31,203)sg(1),stet(i),r(k),npr,spw(1,1,k,i),spw(2,1,k,i)
	    write(32,203)sg(2),stet(i),r(k),npr,spw(1,2,k,i),spw(2,2,k,i)
 	    write(33,203)sg(3),stet(i),r(k),npr,spw(1,3,k,i),spw(2,3,k,i)
	    write(34,203)sg(4),stet(i),r(k),npr,spw(1,4,k,i),spw(2,4,k,i)
	    write(35,203)sg(5),stet(i),r(k),npr,spw(1,5,k,i),spw(2,5,k,i)
   34 continue
	do 36 m = 1, ng
	 do 36 i = 1, its
!	  write(39,203)sg(m),stet(i),r(10),npr,spw(1,m,10,i),spw(2,m,10,i)
!	  write(40,203)sg(m),stet(i),r(12),npr,spw(1,m,12,i),spw(2,m,12,i)
!	  write(41,203)sg(m),stet(i),r(20),npr,spw(1,m,20,i),spw(2,m,20,i)
   36 continue

103	format(3f6.0,i4,2p,2x,e9.2,2x,e9.2)
203	format(3f6.0,i4,2x,e9.2,2x,e9.2)
204	format(2f6.0,i4,2x,e9.2)
102	format(3f6.1,i4,1p,2e9.2)
      end do
      
      close(9)
	close(13)
	close(14)

      
	close(30)
	close(31)
	close(32)
	close(33)
	close(34)
	close(35)
	close(39)
	close(40)
	close(41)
      deallocate (sutut,sutlt,
     *        	sutj, dsutut,
     *            dsutlt,dsutj,
     *	        dolg,tet,r,stet,tl,tut,
     *	        sg, pw,spw,sspw)
	return
      end
!c
	
      subroutine strucpw(its,ids,nh,sg,ng,pw,spw,sspw,np)
      dimension pw(nh,its,ids),spw(2,ng,nh,its),sg(ng),sspw(nh,its)
      real*8 sumr,sumi
      pi=3.14152
          
      ar=2.0*pi/ids
      do 1 nsg = 1,ng
        do 2 i = 1, its
          do 2 k = 1 , nh 
            ssr=0.
            do 4 j = 1 , ids
              ssr=ssr+pw(k,i,j)
   4        continue
            ssr=ssr/ids
      
           sspw(k,i)=ssr
           sumr=0.
           sumi=0. 
           do 3 j= 2, ids
             ac=cos(sg(nsg)*ar*j)
             as=sin(sg(nsg)*ar*j)
             ac1=cos(sg(nsg)*ar*(j-1))
             as1=sin(sg(nsg)*ar*(j-1))
             sr=(pw(k,i,j)-ssr)*ac
             sr1=(pw(k,i,j-1)-ssr)*ac1
             rsr=(sr+sr1)/2.
             sm=(pw(k,i,j)-ssr)*as  
             sm1=(pw(k,i,j-1)-ssr)*as1  
             rsm=(sm+sm1)/2.
             sumr=sumr+rsr
             sumi=sumi+rsm
  3        continue
       
	     sumr=2*sumr/ids
	     sumi=2*sumi/ids
           spw(1,nsg,k,i)=sqrt(sumr**2+sumi**2)
           if(np.EQ.12) spw(1,nsg,k,i)=spw(1,nsg,k,i)/100
           fazr=datan2(sumi,sumr+0.01)
           fazg=fazr*180/pi
           Tmax=360./abs(sg(nsg)) ! Определение предельного значения фазы
           fazg = fazg+180 ! Перевод фазы в положительную область
           if (fazg.gt.Tmax) fazg=fazg-Tmax ! Перевод фазы в область [0; T]
           if (fazg.gt.Tmax) fazg=fazg-Tmax 
           spw(2,nsg,k,i)= fazg
  2      continue
  1   continue
      return
      end
