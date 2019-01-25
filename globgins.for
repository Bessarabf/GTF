	 
! ���������� ������ 2004
! nfile - ����������  ����� ����� - 4 ��� 5, ������ � open
! readfl - ���������� ������� ������ � ���� (false)��� ������ (truth)
! pgl1 - ������ ������������ kpar*nh*its*ids
! nh - ����� ����� �� ������
! ids - ����� ����� �� �������
! its -����� ����� �� ������
! mass- ������ ����������� ����������
! ldor - ����� ������ - 4096 (4*1024) �����
! kdf - ������ � �������� ��������� �������. �������� �������� ��������� ����, ������ � �.�.
!  
      subroutine globGINS(pgl1,rads,kpar,ldor,
     *                  nh,kdf,kdu,isp,npgl,its,ids)
      !include 'parametr.inc'
      dimension pgl1(npgl),kdf(20),kdu(20),pole(ldor/4),rads(NH)

 !     print *,' globGINS - begin'
      npg=kpar*nh*its*ids
!	print*,npg,isp,ldor/4
! �������� ������������ ����� ������� � ���������� ����������. 
! ���������� ��� ������������ ������ File4
      if (npgl.lt.npg) then 
 !       print 700,npg,npgl
700     format (' gloGINS :   ********  error  ********'/
     *   '  npg=',i8,'  >   npgl=',i8,'  !!!!!!  STOP  !')
        stop
      end if  
      
! ������ 1-� ������ � ������� �����
      read(5,rec=1) pole
      rads=pole(106:135)
! ����� ��������� ������ ���������������� ���������� ���� KDU(7)
      isp=kdf(7)+1
      mdor=ldor/4     ! ����� ������������ �������� � ������ (4 ����� �� �����)
      k=1
      do j=1,kdu(7)-1
         read(5,rec=isp) pole
	   do i=1,mdor
            pgl1(k)=pole(i)
            k=k+1
         end do 
         isp=isp+1
      end do 
      ! ��������� ������
	
	  read(5,rec=isp) pole
	  pgl1(k:npg)=pole(1:mdor)

!     print *, 'globGINS - end'
      return
      end
