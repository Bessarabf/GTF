!c              MAPTIDE-Spectr
!c	  
!с        
!с    np - 16- кол-во параметров, nt - 24 - кол-во моментов времени	
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
	subroutine TIDESTR(path,path2,nd1,nd2,npr,nhr,kpars,nh,ids,its,nt,nnp)
      allocatable tide24(:,:,:,:,:),tide12(:,:,:,:,:),
     *          tide8(:,:,:,:,:)
            
      allocatable sutut(:,:,:,:),sutlt(:,:,:,:),
     *          sutj(:,:,:,:),dsutut(:,:,:,:),
     *          dsutlt(:,:,:,:),dsutj(:,:,:,:)
	allocatable dolg(:),tet(:),r(:),tut(:),tl(:),stet(:)
	allocatable tider24(:,:,:),tider12(:,:,:),tider8(:,:,:)
	allocatable sppril24(:,:,:),sppril12(:,:,:),sppril8(:,:,:),
     *           sg(:)

	integer nhr 
	character path*140
	character path2*150
	character path3*160
	character fn1*2
	character fn2*2
	character fh1*2
	character fh2*2
	character fh*3
	character fn*2
	character p0*140
	character sp8*140
	character sp12*140
	character sp24*140
	character pr8*140
	character pr12*140
	character pr24*140
	character prut*140
	character prlt*140
	character prj*140
      
      allocate (tide24(kpars,2,nh,its,ids),tide12(kpars,2,nh,its,ids),
     *          tide8(kpars,2,nh,its,ids))
            
      allocate (sutut(kpars,nh,its,ids),sutlt(kpars,nh,its,nt),
     *          sutj(kpars,nh,its,nt),dsutut(kpars,nh,its,ids),
     *          dsutlt(kpars,nh,its,nt),dsutj(kpars,nh,its,nt))
	allocate (dolg(ids),tet(its),r(nh),tut(nt),tl(nt),stet(its))
	allocate (tider24(2,its,ids),tider12(2,its,ids),tider8(2,its,ids))
	allocate (sppril24(2,its,11),sppril12(2,its,11),sppril8(2,its,11),
     *           sg(11))
      
      if(nnp.EQ.1.OR.nnp.EQ.2.OR.nnp.EQ.3) then
          nh=1
          np=nnp
      endif
      
      print*,'TideSTR'
      print*,"number of height node = ",nhr
	write(fh1,10) nhr
	write(fh2,11) nhr
	if (nhr.LE.9) then
	fh="h0" // trim(fh1)
	else 
	fh="h" // trim(fh2)
     	end if


     	do jk = nd1,nd2
	write(fn1,10) jk
10	format(i1)
	write(fn2,11) jk
11	format(i2)
	if (jk.LE.9) then
	fn="0" // trim(fn1)
	else 
	fn=trim(fn2)
      end if 
      
      p0=trim(path) // '\' // trim(fn) // '\'
	path3=trim(path2) // trim(fh) // '\'
	sp8=trim(path3) // 'spec' // '\' // 'spec8-'// trim(fn) 
	sp12=trim(path3) // 'spec' // '\' // 'spec12-'// trim(fn)
	sp24=trim(path3) // 'spec' // '\'// 'spec24-' // trim(fn)
	pr8=trim(path3) // 'par' // '\'// 'PAR8-'// trim(fn) 
	pr12=trim(path3) // 'par' // '\'// 'PAR12-'// trim(fn) 
	pr24=trim(path3) // 'par' // '\'// 'PAR24-'// trim(fn)
	prut=trim(path3) // 'par' // '\'// 'PARut-'// trim(fn)
	prlt=trim(path3) // 'par' // '\'// 'PARlt-'// trim(fn)
	prj=trim(path3) // 'par' // '\'// 'PARj-'// trim(fn)

	open(9,file=trim(p0) // 'sutut.dat')			
	open(13,file=trim(p0) // 'sutlt.dat')
	open(14,file=trim(p0) // 'sutj.dat')
	open(10,file=trim(p0) // 'tide24.dat')
	open(11,file=trim(p0) // 'tide12.dat')
	open(12,file=trim(p0) // 'tide8.dat')

      open(30,file=trim(sp8) //'.dat')
	open(31,file=trim(sp12) //'.dat')
	open(32,file=trim(sp24) //'.dat')

	open(15,file=trim(pr8) //'.dat')
	open(16,file=trim(pr12) //'.dat')
      open(17,file=trim(pr24) //'.dat')
	open(18,file=trim(prut) //'.dat')
	open(19,file=trim(prlt) //'.dat')
	open(20,file=trim(prj) //'.dat')			 

      np=kpars
	
	npril=2
      if(nnp.EQ.1.OR.nnp.EQ.2.OR.nnp.EQ.3) then
          nh=1
          np=nnp
      endif
      
	
!c           Чтение глобальных массивов параметров
	do m = 1 , np
	  do k = 1 , nh
	    do i = 1 , its
	     do j = 1 , ids
	
	       read (10,*)dolg(j),tet(i),r(k),m0,tide24(m,1,k,i,j),
     *             tide24(m,2,k,i,j)
	
             read (11,*)dolg(j),tet(i),r(k),m0,tide12(m,1,k,i,j),
     *            tide12(m,2,k,i,j)

             read (12,*)dolg(j),tet(i),r(k),m0,tide8(m,1,k,i,j),
     *            tide8(m,2,k,i,j)

           enddo
 	    enddo
	   enddo
	enddo
  
	do m = 1 , np
          do k = 1 , nh
	    do i = 1 , its
	      do j = 1 , ids
             read (9,*) dolg(j),tet(i),r(k),m0,sutut(m,k,i,j)
     *           ,dsutut(m,k,i,j)
              end do
            enddo
          enddo
        enddo

        do m = 1 , np
          do k = 1 , nh
	    do i = 1 , its
	      do ntt=1,nt
                read (13,*) tl(ntt),tet(i),r(k),m0,sutlt(m,k,i,ntt)
     *            ,dsutlt(m,k,i,ntt)
	       read (14,*) tut(ntt),tet(i),r(k),m0,sutj(m,k,i,ntt)
     *                      ,dsutj(m,k,i,ntt)

             enddo
	    enddo
	   enddo
	  enddo
     	print *,dolg(1),tet(5),r(nhr),npr,sutut(npr,nhr,5,1),
     *         sutlt(npr,nhr,5,1)

     
     
     
    
			

!с  Запись глобальных мсассивов характеристик приливов
	
      do  21 i = 1, its
	    stet(i)=90-tet(i)
	    do  21 j = 1 , ids
	      write (18,103) dolg(j),stet(i),r(nhr),npr,sutut(npr,nhr,i,j)
     *                    ,dsutut(npr,nhr,i,j)
	

21    continue
      do  26 i = 1, its
         do  26 j = 1 , nt
	
		write (19,103) tl(j),stet(i),r(nhr),npr,sutlt(npr,nhr,i,j)
     *,dsutlt(npr,nhr,i,j)
26     continue

	  do  27 i = 1, its
	    do  27 j = 1 , nt
	      write (20,103) tut(j),stet(i),r(nhr),npr,sutj(npr,nhr,i,j)
     *           , dsutj(npr,nhr,i,j)
 27	  continue
	

      do 22 i = 1, its
        do 22 j = 1 , ids
	
	   write(15,103)dolg(j),stet(i),r(nhr),npr,tide8(npr,1,nhr,i,j),
     *         tide8(npr,2,nhr,i,j)
	   write(16,103)dolg(j),stet(i),r(nhr),npr,tide12(npr,1,nhr,i,j),
     *          tide12(npr,2,nhr,i,j)
         write(17,103)dolg(j),stet(i),r(nhr),npr,tide24(npr,1,nhr,i,j),
     *          tide24(npr,2,nhr,i,j)
 22	    continue


!         Спектр приливов      
  	  

          do 33 i=1, its
           do 33 j = 1, ids
			
            tider24(1,i,j)= tide24(npr,1,nhr,i,j)
            tider24(2,i,j)= tide24(npr,2,nhr,i,j)
            tider12(1,i,j)= tide12(npr,1,nhr,i,j)
            tider12(2,i,j)= tide12(npr,2,nhr,i,j)
            tider8(1,i,j)= tide8(npr,1,nhr,i,j)
            tider8(2,i,j)= tide8(npr,2,nhr,i,j)

  33       continue
!         спектральные гармоники
            ng=11
            do nsg= 1, ng
             sg(nsg)= -6+nsg  
            enddo          


         call ppo(its,ids,1,sg,ng,tider24,sppril24)
         call ppo(its,ids,2,sg,ng,tider12,sppril12)
         call ppo(its,ids,3,sg,ng,tider8,sppril8)
c		       запись в файл
	
 !         print*,sppril24
 !         pause
           do 34 nsg=1, ng
             do 34 i = 1 , its
                write(30,203)sg(nsg),stet(i),r(nhr),npr,
     *                       sppril8(1,i,nsg),sppril8(2,i,nsg)
                write(31,203)sg(nsg),stet(i),r(nhr),npr,
     *          sppril12(1,i,nsg),sppril12(2,i,nsg)
                write(32,203)sg(nsg),stet(i),r(nhr),npr,
     *          sppril24(1,i,nsg),sppril24(2,i,nsg)
34     continue

103	format(3f6.0,i4,2p,2x,e9.2,2x,e9.2)
203	format(3f6.0,i4,2x,e9.2,2x,e9.2)
	
 102	format(3f6.1,i4,1p,2e9.2)
 	
      end do
      close(9)			
      close(13)
      close(14)
      close(10)
      close(11)
      close(12)
      close(30)
      close(31)
      close(32)
      close(15)
      close(16)
      close(17)
      close(18)
      close(19)
      close(20)
      deallocate (tide24,tide12,
     *          tide8)
            
      deallocate (sutut,sutlt,
     *          sutj,dsutut,
     *          dsutlt,dsutj)
	deallocate (dolg,tet,r,tut,tl,stet)
	deallocate (tider24,tider12,tider8)
	deallocate (sppril24,sppril12,sppril8,
     *           sg)
	return
      end
 
!c
	
       subroutine ppo(its,ids,npril,sg,ng,tider,sppril)
       dimension tider(2,its,ids), sppril(2,its,ng),sg(ng)
       dimension ap(its,ids),af(its,ids)

        pi=3.14152
       om=2*pi*npril/24.
!c	 om=2*pi*npril/ids !!
       do i = 1, its 
       do j = 1 , ids
        a = tider(1,i,j)
         b = tider(2,i,j)-(j-1)*24/ids
         ap(i,j)=a*cos(-om*b)
         af(i,j)=a*sin(-om*b)
       enddo
	enddo
        ar=2*pi/ids
       do 1 nsg = 1,ng
       do 2 i = 1, its
       sumr=0.
        sumi=0. 
        do 3 j= 2, ids
         ac=cos(sg(nsg)*ar*j)
         as=sin(sg(nsg)*ar*j)
         ac1=cos(sg(nsg)*ar*(j-1))
         as1=sin(sg(nsg)*ar*(j-1))
         sr=ap(i,j)*ac+af(i,j)*as
         sr1=ap(i,j-1)*ac1+af(i,j-1)*as1
          rsr=(sr+sr1)/2.
        sm=-ap(i,j)*as+af(i,j)*ac  
        sm1=-ap(i,j-1)*as1+af(i,j-1)*ac1  
         rsm=(sm+sm1)/2.
         sumr=sumr+rsr
          sumi=sumi+rsm
  3   continue
	sumr=sumr/ids
	sumi=sumi/ids
          sppril(1,i,nsg)=sqrt(sumr**2+sumi**2)
           fazr=atan2(sumi,sumr)
           fazg=fazr*180/pi
          sppril(2,i,nsg)= fazg
  2      continue
  1      continue
      return
      end