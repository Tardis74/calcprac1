program generate_test_data
   implicit none
   integer :: n, i, j
   real(8), allocatable :: a_d(:), a_l(:), a_r(:)
   real(8), allocatable :: b_d(:), b_l(:), b_r(:)
   character(100) :: arg
   real(8) :: r

   ! Чтение аргумента командной строки (размер n)
   call get_command_argument(1, arg)
   if (arg == '') then
      write(*,*) 'Usage: ./generate.x n'
      stop
   end if
   read(arg, *) n

   allocate(a_d(n), a_l(n), a_r(n))
   allocate(b_d(n), b_l(n), b_r(n))

   ! Заполнение диагоналей случайными числами в интервале (-5,5)
   call random_seed()
   do i = 1, n
      call random_number(r); a_d(i) = 10.0d0*r - 5.0d0
      call random_number(r); b_d(i) = 10.0d0*r - 5.0d0
      if (i < n) then
         call random_number(r); a_r(i) = 10.0d0*r - 5.0d0
         call random_number(r); b_r(i) = 10.0d0*r - 5.0d0
      else
         a_r(i) = 0.0d0
         b_r(i) = 0.0d0
      end if
      if (i > 1) then
         call random_number(r); a_l(i) = 10.0d0*r - 5.0d0
         call random_number(r); b_l(i) = 10.0d0*r - 5.0d0
      else
         a_l(i) = 0.0d0
         b_l(i) = 0.0d0
      end if
   end do

   ! Запись матрицы A в data1.dat
   open(10, file='data1.dat', action='write')
   write(10, '(A, I0)') '# ', n
   if (n == 1) then
      write(10, *) a_d(1)
   else
      do j = 1, n
         if (j == 1) then
            write(10, *) a_d(1), a_l(2)        ! a11, a21
         else if (j == n) then
            write(10, *) a_r(n-1), a_d(n)      ! a_{n-1,n}, a_{n,n}
         else
            write(10, *) a_r(j-1), a_d(j), a_l(j+1)  ! a_{j-1,j}, a_{jj}, a_{j+1,j}
         end if
      end do
   end if
   close(10)

   ! Запись матрицы B в data2.dat
   open(11, file='data2.dat', action='write')
   write(11, '(A, I0)') '# ', n
   if (n == 1) then
      write(11, *) b_d(1)
   else
      do j = 1, n
         if (j == 1) then
            write(11, *) b_d(1), b_l(2)
         else if (j == n) then
            write(11, *) b_r(n-1), b_d(n)
         else
            write(11, *) b_r(j-1), b_d(j), b_l(j+1)
         end if
      end do
   end if
   close(11)

   deallocate(a_d, a_l, a_r, b_d, b_l, b_r)
   write(*,*) 'Test data generated with n =', n
end program generate_test_data
