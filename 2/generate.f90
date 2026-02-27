program generate_test_data
   implicit none
   integer :: n, i
   real(8), allocatable :: dA(:), lA(:), uA(:)
   real(8), allocatable :: dB(:), lB(:), uB(:)
   character(100) :: arg
   real(8) :: r

   ! Чтение аргумента командной строки (размер n)
   call get_command_argument(1, arg)
   if (arg == '') then
      write(*,*) 'Usage: ./generate.x n'
      stop
   end if
   read(arg, *) n

   allocate(dA(n), lA(n), uA(n))
   allocate(dB(n), lB(n), uB(n))

   ! Заполнение диагоналей случайными числами в интервале (-5,5)
   call random_seed()
   do i = 1, n
      call random_number(r); dA(i) = 10.0d0*r - 5.0d0
      call random_number(r); dB(i) = 10.0d0*r - 5.0d0
      if (i < n) then
         call random_number(r); uA(i) = 10.0d0*r - 5.0d0
         call random_number(r); uB(i) = 10.0d0*r - 5.0d0
      else
         uA(i) = 0.0d0
         uB(i) = 0.0d0
      end if
      if (i > 1) then
         call random_number(r); lA(i) = 10.0d0*r - 5.0d0
         call random_number(r); lB(i) = 10.0d0*r - 5.0d0
      else
         lA(i) = 0.0d0
         lB(i) = 0.0d0
      end if
   end do

   ! Запись матрицы A в data1.dat (по строкам)
   open(10, file='data1.dat', action='write')
   write(10, '(A, I0)') '# ', n
   if (n == 1) then
      write(10, *) dA(1)
   else
      do i = 1, n
         if (i == 1) then
            write(10, *) dA(1), uA(1)
         else if (i == n) then
            write(10, *) lA(n), dA(n)
         else
            write(10, *) lA(i), dA(i), uA(i)
         end if
      end do
   end if
   close(10)

   ! Запись матрицы B в data2.dat
   open(11, file='data2.dat', action='write')
   write(11, '(A, I0)') '# ', n
   if (n == 1) then
      write(11, *) dB(1)
   else
      do i = 1, n
         if (i == 1) then
            write(11, *) dB(1), uB(1)
         else if (i == n) then
            write(11, *) lB(n), dB(n)
         else
            write(11, *) lB(i), dB(i), uB(i)
         end if
      end do
   end if
   close(11)

   deallocate(dA, lA, uA, dB, lB, uB)
   write(*,*) 'Test data generated with n =', n
end program generate_test_data
