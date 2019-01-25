!c              HPW - программа для формирования файлов широтно-высотных	временных
!                распределений  планетарной волны с заданным зональным числом
!c	  
!с        
!с    np - 19- кол-во параметров, nt - 24 - кол-во моментов времени	
!с    30 - высоты, 37- широты, 72 - долготы
!	  
!	

!с	  
!
!	   
!с

	subroutine HPW(path,nd1,nd2,nz,h,kpars,nh,ids,its,nt,nnp)
      dimension pars(8)
      integer h(4), nz(7)
	character path*150
      if(nnp.EQ.1.OR.nnp.EQ.2.OR.nnp.EQ.3) then
          nh=1
          np=nnp
      endif
      print*,'HPW'
	ndd=nd2-nd1+1 ! Длина интервала дней
      call HPW0(path,nz,h,ndd,nd1,nd2,kpars,nh,ids,its,nt,nnp)
      return
      end
      
      subroutine HPW0(path,nz,h,ndd,nd1,nd2,kpars,nh,ids,its,nt,nnp)
      dimension ss(ndd,nh,its),sp(ndd,nh,its), stet(its), r(nh)
      dimension pars(8)
      integer h(4), nz(7)
      integer ng,npr
	character path*150
	character pp*150
	character vc*1
	character jj*4
	character j2*4
	character j0*4
	integer j
    
      if(nnp.EQ.1) then
          nh=1
          np=1
      endif
      
	do ng=0,5
      print*,'current mode = ', ng
      ! Открываем файлы
          
      write(vc,10) ng
      pp=trim(path) // 'pw' // vc // '\' ! Папка с исходными данными
      do j=nd1,nd2
	write(jj,10) j
	write(j2,11) j
      if (j.LE.9) then
	j0="0" // trim(jj)
      else 
	j0=trim(j2)
      end if
      open(j,file=trim(pp) //'PW' // vc // '-' // trim(j0) // '.dat')
      enddo
      
      do i = 1 , its
	    stet(i)=90.- 5*(i-1)
      enddo
      
      ! Считываем исходные данные для данной гармоники
      do j=1,ndd
          ndt=nd1+j-1
          do k = 1 , nh
	        do i = 1 , its
	            if(ng.EQ.0) then ! Чтение данных для фона
	                read(ndt,*)stet(i),r(k),npr,ss(j,k,i)
	            else ! Чтение данных для гармоник
	              read(ndt,*)q,stet(i),r(k),npr,ss(j,k,i),sp(j,k,i)
	            end if
	        enddo
	    enddo
      enddo
   
      ! Закрываем файлы
      
      do j=nd1,nd2
          close(j)
      enddo
	
      ! На фиксированной широте
      do i=1,7
          if(nz(i).NE.0) then
			call PWZ(ss,sp,r,stet,nz(i),pp,ng,ndd,nd1,nd2,kpars,nh,ids,
     *its,nt)
          endif
      enddo
      
      ! На фиксированной высоте
      do i=1,4
          if(h(i).NE.0) then
			call PWH(ss,sp,r,stet,h(i),path,ng,ndd,nd1,nd2,kpars,nh,ids,
     *its,nt)
          endif
      enddo
      
      end do
203   format(3f6.0,i4,2x,e9.2,2x,e9.2)
205   format(2f6.0,i4,2x,e9.2)   
11    format(i2)
10	format(i1)
	return
      end

      subroutine PWZ(ss,sp,r,stet,nzt,pp,ng,ndd,nd1,nd2,kpars,
     *nh,ids,its,nt) ! Пишем на фиксированной широте
      dimension ss(ndd,nh,its),sp(ndd,nh,its), stet(its), r(nh)
      dimension pars(8)
      character pp*150
      character fz1*1
	character fz2*2
	character fz*3
      character vc*1
      integer ng, nzt
      stetz=stet(nzt)
      write(fz1,10) nzt ! Для имени широтного файла
	write(fz2,11) nzt
	if (nzt.LE.9) then
	    fz="z0" // trim(fz1)
	else 
	    fz="z" // trim(fz2)
      end if
      write(vc,10) ng
      
      open(51,file=trim(pp) // 'PW' // vc // '-' // fz // '.dat')
	do i = 1,ndd
          ndt=nd1+i-1
              do k =1,nh
              if(ng.EQ.0) then
                  write(51,206)stetz,r(k),ndt,ss(i,k,nzt)
              else
                  write(51,204)stetz,r(k),ndt,ss(i,k,nzt),sp(i,k,nzt)
              end if
          enddo
      enddo
      close(51)
204   format(2f6.0,i4,2x,e9.2,2x,e9.2)
206	format(2f6.0,i4,2x,e9.2)
11    format(i2)
10	format(i1)
      return
      end 
      subroutine PWH(ss,sp,r,stet,nht,path,ng,ndd,nd1,nd2,kpars,
     *nh,ids,its,nt) ! На фиксированной высоте
      dimension ss(ndd,nh,its),sp(ndd,nh,its), stet(its), r(nh)
      dimension pars(8)
      character path*150
      character fh1*1
	character fh2*2
	character fh*3
      character vc*1
      integer ng, nht
      write(vc,10) ng
      write(fh1,10) nht ! Для имени высотного файла
	write(fh2,11) nht
	if (nht.LE.9) then
	    fh="h0" // trim(fh1)
	else 
	    fh="h" // trim(fh2)
      end if
      
      open(52,file=trim(path) // '\' // fh //
     *'\' // 'PW' // vc // '.dat')
      do i = 1,ndd
          ndt=nd1+i-1
          do k = 1,its
              if(ng.EQ.0) then
              write(52,206)stet(k),r(nht),ndt,ss(i,nht,k)
              else
              write(52,204)stet(k),r(nht),ndt,ss(i,nht,k),sp(i,nht,k)
              end if
          enddo
      enddo
      close(52)
204   format(2f6.0,i4,2x,e9.2,2x,e9.2)
206	format(2f6.0,i4,2x,e9.2)
11    format(i2)
10	format(i1)
      return
      end 