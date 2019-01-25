c         Считывание данных из глобальных массивов шара и
cс интерполяция в географические координаты
      subroutine filegeo(pgl,nh,its,ids,kpars,dtets,ddolgs,
     *          rads)
	 
      dimension pgl(kpars,nh,its,ids),
     *          rads(nh)
     	data PI/3.14159/
      allocatable p(:,:,:,:),tes(:),gir(:),gkoor(:,:,:)
      allocate(p(kpars,nh,its,ids))
      allocate (tes(its), gir(ids),gkoor(2,its,ids)) 
   
  900 format (a25)
  901 format (a1)
  902 format (F5.1,4F8.2)
  903 format (F5.1,3X,E8.3,8X,E8.3,5X,3F8.1)
  904 format (5E11.3)
!      open(11,file='d:\user\ivan\tide\F4.dat')
!      open(12,file='d:\user\ivan\tide\PGL.dat') 
!     
c***************************
c    чтение файла в геомагниной системе координат
c**************************************       
c      read (11,) pgl 
c*********************************
   
      do i=1,ids
        gir(i)=ddolgs*(i-1)
      enddo
      
      do i=1,its
        tes(i)=dtets*(i-1)
      enddo
	do i=1,its
	  tet=tes(i)
        do j=1,ids
	    dolg=gir(j)
          call ggmraw(0,dolg,tet,dolm,tetm)
          gkoor(1,i,j)=tetm
          gkoor(2,i,j)=dolm
	!    print*,'tetm,dolm',tet,dolg,tetm,dolm
	!    pause
        end do
      end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	pause
      p=pgl
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call intpa(tes,its,dtets,gir,ids,ddolgs,gkoor,p,
     *           pgl,kpars,nh)

!      print*,'filegeo: end of interpolation'

!      IF(KPARS.Le.16) THEN
      if(kpars.EQ.19) then
        call noznew(gir,tes,kpars,nh,its,ids,pgl)
      endif
      
!      END IF
c***************************
c    запись файла в географической системе координат
c**************************************       
c     write (12,) pgl

	deallocate(p,tes,gir,gkoor)
      return
      end

      subroutine noznew(gir,tes,kpars,nh,its,ids,pgl)
      dimension gir(ids),pgl(kpars,nh,its,ids),tes(its)
!	print*,'noznew'
 !     print*,kpars
       i=1
        do j=1,its
          do l=1,ids
            t=tes(j)
            f=gir(l)
            r=g11t31(f,t)
c            r=-r
            s=sin(r)
            c=cos(r)
            st=pgl(11,1,j,l)
            sf=pgl(12,1,j,l)
            p1=st*c-sf*s
            pgl(11,1,j,l)=p1
            p2=sf*c+st*s
            pgl(12,1,j,l)=p2
          enddo
        enddo
      return
      end

      subroutine intpa(tes,its,dtets,gir,ids,ddolgs,gkoor,p,
     *            pgl,kpars,nh)
      dimension tes(its),gir(ids),gkoor(2,its,ids),p(kpars,nh,its,ids),
     *          pgl(kpars,nh,its,ids)
  900 format (2e12.3)
  
	 do j=1,ids
        do i=2,its-1
          tet =gkoor(1,i,j)
          dolg=gkoor(2,i,j)
 !        print *,' i,tet,dolg',i,tet,dolg
         
          call find(its,tet, tes,in)
          call find(ids,dolg,gir,jn)
          tetin=tes(in)
          dx=(tet-tetin)/dtets
          dolgjn=gir(jn)
c         print *,' tetin,dolgjn',tetin,dolgjn
          dy=(dolg-dolgjn)/ddolgs
          jn1=jn+1
          if(jn.eq.ids)jn1=1
c         print *,' in,in+1,jn,jn+1',in,in+1,jn,jn1
          do 3 kpar = 1 , kpars
           do 4 k = 1,nh
			p1=p(kpar,k,in,jn)
			p2=p(kpar,k,in+1,jn)
			p3=p(kpar,k,in,jn1)
			p4=p(kpar,k,in+1,jn1)
			f1=p1+(p2-p1)*dx
			f2=p3+(p4-p3)*dx
			ff=f1+(f2-f1)*dy
  !         print *,' i j ',i,j,in,jn
          pgl(kpar,k,i,j)=ff
		
    4     continue

    3    continue
         


       enddo  
      enddo
      return
      end

	subroutine find(n,u,x,m)
      dimension x(n)
      data i/1/
      if(u.gt.x(n).or.u.lt. x(1))then
!	   print 900,u,x(1),x(n)
  900    format(' find',1p3E10.3 )
!	pause
	end if
c    *' значение u выходит за пределы массива x в ',a4/
c    *'   u=',e10.3,'  x(1)=',e10.3,'  x(n)=',e10.3)
      if(i.ge.n) i=1
      if(u.lt.x(i)) go to 10
      if(u.le.x(i+1)) go to 30

   10 i=1
      j=n+1
   20 k=(i+j)/2
      if(u.lt.x(k)) j=k
      if(u.ge.x(k)) i=k
      if(j.gt.i+1) go to 20
   30 m=i
      return
      end

      subroutine bongi(gins,ins,nh,its,ids)
      dimension gins(ins,nh,its,ids)
      i2=its-1
      do np=1,ins
          do k = 1 , nh
c         ssp - sum s.pole
c         snp - sum n.pole
              snp=0.
              ssp=0.
              do j = 1 , ids
                  snp=snp+gins(np,k,2,j)
                  ssp=ssp+gins(np,k,i2,j)
              enddo
c
              unp=snp/ids
              usp=ssp/ids
              do j=1,ids
                  gins(np,k,1,j)=unp
                  gins(np,k,its,j)=usp
              enddo
          enddo
      enddo
      return
      end
      subroutine bongi2(gins,npril,ins,nh,its,ids)
      dimension gins(ins,npril,nh,its,ids)
      i2=its-1
      do np=1,ins
          do k = 1 , nh
c         ssp - sum s.pole
c         snp - sum n.pole
              snp=0.
              ssp=0.
              do j = 1 , ids
                  snp=snp+gins(np,1,k,2,j)
                  ssp=ssp+gins(np,1,k,i2,j)
              enddo
c
              unp=snp/ids
              usp=ssp/ids
              do j=1,ids
                  gins(np,1,k,1,j)=unp
                  gins(np,1,k,its,j)=usp
                  gins(np,2,k,1,j)=0
                  gins(np,2,k,its,j)=0
              enddo
          enddo
      enddo
      return
      end
            
      subroutine bongi0(gins,ins,nh,its,ids)
      dimension gins(ins,nh,its,ids)
      i2=its-1
      do 3 i=1,3
          np=i
          if(i.eq.2)np=5
          if(i.eq.3)np=6
       do 2 k = 1 , nh
c      ssp - sum s.pole
c      snp - sum n.pole
        snp=0.
        ssp=0.
        do 1 j = 1 , ids
         snp=snp+gins(np,k,2,j)
         ssp=ssp+gins(np,k,i2,j)
   1    continue
c
        unp=snp/ids
        usp=ssp/ids
        do 4 j=1,ids
          gins(np,k,1,j)=unp
          gins(np,k,its,j)=usp
    4   continue
    2  continue
    3 continue
      return
      end
      
      subroutine bospgl(pgl,kpars,nh,its,ids,np)
      dimension pgl(kpars,nh,its,ids)
      i2=its-1
      do 1 k=1,nh
       ssp=0.
       snp=0.
       do 2 j=1,ids
        snp=snp+pgl(np,k,2,j)
        ssp=ssp+pgl(np,k,i2,j)
    2  continue
       unp=snp/ids
       usp=ssp/ids
       do 3 j=1,ids
        pgl(np,k,1,j)=unp
        pgl(np,k,its,j)=usp
    3  continue
    1 continue
      return
      end

