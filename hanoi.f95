program hanoi
 implicit none
 integer, dimension(:,:), allocatable :: p
 integer :: i, n, m, e, j, r4, r5, r6
 
 write(*,'(A)') 'TORRE DE HANOI!!'
 write(*,'(A)') ''
 write(*,'(A)') 'Objetivo: Ao informar qualquer numero inteiro de discos,'
 write(*,'(A)') 'o programa mostra passo-a-passo a solucao do exercicio'
 write(*,'(A)') 'Torre de Hanoi para esse numero de discos'
 write(*,'(A)') ''
 write(*,'(A)') 'Informe o numero de discos:' ;read(*,*) n
 m = 3
 e = (2**n)-1

 allocate(p(n,m))

 do i=1,n
   p(i,1) = i
 end do
 

 do i=1, n
   p(i,2) = 0
   p(i,3) = 0
 end do
 call foto (n, p)
 call zerar (r4,r5,r6)
 if (n.eq.1) then
   p(1,3) = p(1,1)
   p(1,1) = 0
   call foto (n, p)
   
 elseif (MOD(n,2) .eq. 0) then 
     do while (j.ne.e)
    
    call zerar (r4,r5,r6)
    call contador (n,p,r4,r5,r6)
    
    if (p(r4,1).gt.p(r5,2).and.p(r5,2).eq.0) then
    	p(r5,2) = p(r4,1)
        p(r4,1) = 0
        j = j + 1
        call foto (n,p)
         else if (p(r4,1).lt.p(r5,2).and.p(r4,1).eq.0) then
      p(r4,1) = p(r5,2)
      p(r5,2) = 0
      j = j + 1
      call foto (n,p)
    else if (p(r4,1).lt.p(r5,2).and.p(r5,2).ne.0) then
      p(r5-1,2) = p(r4,1)
      p(r4,1) = 0
      j = j + 1
      call foto(n,p)
    else if (p(r4,1).gt.p(r5,2).and.p(r5,2).ne.0) then
      p(r4-1,1) = p(r5,2)
      p(r5,2) = 0
      j = j + 1
      call foto (n,p)
    end if
if (j.eq.e ) then
  exit
end if
call zerar (r4,r5,r6)
call contador (n,p,r4,r5,r6)
    
    if (p(r4,1).gt.p(r6,3).and.p(r6,3).eq.0) then
    	p(r6,3) = p(r4,1)
        p(r4,1) = 0
        j = j + 1
        call foto (n,p)

    else if (p(r4,1).gt.p(r6,3).and.p(r6,3).ne.0) then
      p(r4-1,1) = p(r6,3)
      p(r6,3) = 0
      j = j + 1
      call foto (n,p)

    else if (p(r4,1).lt.p(r6,3).and.p(r4,1).eq.0) then
      p(r4,1) = p(r6,3)
      p(r6,3) = 0
      j = j + 1
      call foto (n,p)
    else if (p(r4,1).lt.p(r6,3).and.p(r6,3).ne.0) then
     p(r6-1,3) = p(r4,1)
     p(r4,1) = 0
     j = j + 1
     call foto(n,p)
    end if
if (j.eq.e ) then
  exit
end if
call zerar (r4,r5,r6)
call contador (n,p,r4,r5,r6)
    
    if (p(r5,2).gt.p(r6,3).and.p(r6,3).eq.0) then
    	p(r6,3) = p(r5,2)
        p(r5,2) = 0
        j = j + 1
        call foto (n,p)

    else if (p(r5,2).gt.p(r6,3).and.p(r6,3).ne.0) then
      p(r5-1,2) = p(r6,3)
      p(r6,3) = 0
      j = j + 1
      call foto (n,p)
    else if (p(r5,2).lt.p(r6,3).and.p(r6,3).ne.0) then
      p(r6-1,3) = p(r5,2)
      p(r5,2) = 0
      j = j + 1
      call foto(n,p)
    end if
if (j.eq.e ) then
  exit
end if
 end do
    
 elseif (MOD(n,2) .ne. 0) then 	
  do while (j.ne.e)
    
    call zerar (r4,r5,r6)
    call contador (n,p,r4,r5,r6)
    
    if (p(r4,1).gt.p(r6,3).and.p(r6,3).eq.0) then
    	p(r6,3) = p(r4,1)
        p(r4,1) = 0
        j = j + 1
        call foto (n,p)

    else if (p(r4,1).lt.p(r6,3).and.p(r6,3).ne.0) then
      p(r6-1,3) = p(r4,1)
      p(r4,1) = 0
      j = j + 1
      call foto(n,p)
    else if (p(r4,1).gt.p(r6,3).and.p(r6,3).ne.0) then
      p(r4-1,1) = p(r6,3)
      p(r6,3) = 0
      j = j + 1
      call foto (n,p)
    end if
if (j.eq.e ) then
  exit
end if
call zerar (r4,r5,r6)
call contador (n,p,r4,r5,r6)
    
    if (p(r4,1).gt.p(r5,2).and.p(r5,2).eq.0) then
    	p(r5,2) = p(r4,1)
        p(r4,1) = 0
        j = j + 1
        call foto (n,p)

    else if (p(r4,1).gt.p(r5,2).and.p(r5,2).ne.0) then
      p(r4-1,1) = p(r5,2)
      p(r5,2) = 0
      j = j + 1
      call foto (n,p)

    else if (p(r4,1).lt.p(r5,2).and.p(r4,1).eq.0) then
      p(r4,1) = p(r5,2)
      p(r5,2) = 0
      j = j + 1
      call foto (n,p)
    else if (p(r4,1).lt.p(r5,2).and.p(r5,2).ne.0) then
     p(r5-1,2) = p(r4,1)
     p(r4,1) = 0
     j = j + 1
     call foto(n,p)
    end if
if (j.eq.e ) then
  exit
end if
call zerar (r4,r5,r6)
call contador (n,p,r4,r5,r6)
    
    if (p(r5,2).gt.p(r6,3).and.p(r6,3).eq.0) then
    	p(r6,3) = p(r5,2)
        p(r5,2) = 0
        j = j + 1
        call foto (n,p)

    else if (p(r5,2).gt.p(r6,3).and.p(r6,3).ne.0) then
      p(r5-1,2) = p(r6,3)
      p(r6,3) = 0
      j = j + 1
      call foto (n,p)
    else if (p(r5,2).lt.p(r6,3).and.p(r6,3).ne.0) then
      p(r6-1,3) = p(r5,2)
      p(r5,2) = 0
      j = j + 1
      call foto(n,p)
    end if
if (j.eq.e ) then
  exit
end if
 end do
end if
 deallocate(p)
 
 
contains
 subroutine foto (linhas, matriz)
 integer, dimension(linhas, 3), intent(in) :: matriz
 integer, intent(in) :: linhas
 integer :: i
 print *, 'Atual Situacao'
 print *, ' '
  do i=1,linhas
   print *, matriz(i,1), matriz(i,2), matriz(i,3)
  end do
 print *, '------------------------------------------------'
 end subroutine foto

subroutine contador (linhas,matriz,r1,r2,r3)
integer, dimension(linhas,3), intent(in) :: matriz
integer, intent(in) :: linhas
integer, intent (out) :: r1,r2,r3
integer :: i,i1=0,i2=0,i3=0
r1 = i1
r2 = i2
r3 = i3
do i=1,linhas
  if(matriz(i,1).ne.0) then
    i1 = i
    exit
  end if
end do
do i=1,linhas
  if(matriz(i,2).ne.0) then
    i2 = i
    exit
  end if
end do
do i=1,linhas
  if(matriz(i,3).ne.0) then
    i3 = i
    exit
  end if
end do
if (i1.eq.0) then
  i1=linhas
end if
if (i2.eq.0) then
  i2 = linhas
end if
if (i3.eq.0) then
  i3 = linhas
end if
  r1=i1;r2=i2;r3=i3
i1=0;i2=0;i3=0
end subroutine contador

subroutine zerar (r7,r8,r9)
integer, intent(out) :: r7,r8,r9
r7 = 0
r8 = 0
r9 = 0
end subroutine zerar

end program hanoi