	 
! исправлено но€брь 2004
! nfile - логический  номер файла - 4 или 5, описан в open
! readfl - логическое условие запись в файл (false)или чтение (truth)
! pgl1 - массив размерностью kpar*nh*its*ids
! nh - число узлов по высоте
! ids - число узлов по долготе
! its -число узлов по широте
! mass- массив управл€ющих параметров
! ldor - длина записи - 4096 (4*1024) байта
! kdf - массив с номерами начальных записей. ѕомогает выделить параметры шара, трубки и т.д.
!  
      subroutine globGINS(pgl1,rads,kpar,ldor,
     *                  nh,kdf,kdu,isp,npgl,its,ids)
      !include 'parametr.inc'
      dimension pgl1(npgl),kdf(20),kdu(20),pole(ldor/4),rads(NH)

 !     print *,' globGINS - begin'
      npg=kpar*nh*its*ids
!	print*,npg,isp,ldor/4
! проверка соответстви€ длины массива и количества параметров. 
! Ќеобходимо дл€ соответстви€ версий File4
      if (npgl.lt.npg) then 
 !       print 700,npg,npgl
700     format (' gloGINS :   ********  error  ********'/
     *   '  npg=',i8,'  >   npgl=',i8,'  !!!!!!  STOP  !')
        stop
      end if  
      
! чтение 1-й записи и массива высот
      read(5,rec=1) pole
      rads=pole(106:135)
! номер начальной записи »Ќ“≈–ѕќЋя÷»ќЌЌџ’ параметров шара KDU(7)
      isp=kdf(7)+1
      mdor=ldor/4     ! число вещественных значений в записи (4 байта на число)
      k=1
      do j=1,kdu(7)-1
         read(5,rec=isp) pole
	   do i=1,mdor
            pgl1(k)=pole(i)
            k=k+1
         end do 
         isp=isp+1
      end do 
      ! последн€€ запись
	
	  read(5,rec=isp) pole
	  pgl1(k:npg)=pole(1:mdor)

!     print *, 'globGINS - end'
      return
      end
