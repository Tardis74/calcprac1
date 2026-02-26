program check_result
   implicit none
   integer :: n, i, j, low, high, idx, n_err
   real(8), allocatable :: A_full(:,:), B_full(:,:), C_full(:,:)
   real(8), allocatable :: ad_A(:), al_A(:), ar_A(:)
   real(8), allocatable :: ad_B(:), al_B(:), ar_B(:)
   real(8), allocatable :: Cdata(:)
   integer, allocatable :: offsetC(:)
   real(8) :: x1, x2, x3, diff, max_diff, tol
   character(100) :: header
   logical :: ok

   ! Чтение матрицы A из data1.dat
   open(10, file='data1.dat', status='old', action='read')
   read(10, '(A)') header
   read(header(2:), *) n
   allocate(ad_A(n), al_A(n), ar_A(n))
   al_A(1) = 0.0d0; ar_A(n) = 0.0d0
   if (n == 1) then
      read(10, *) ad_A(1)
   else
      do j = 1, n
         if (j == 1 .or. j == n) then
            read(10, *) x1, x2
            if (j == 1) then
               ad_A(1) = x1
               al_A(2) = x2
            else
               ar_A(n-1) = x1
               ad_A(n)   = x2
            end if
         else
            read(10, *) x1, x2, x3
            ar_A(j-1) = x1
            ad_A(j)   = x2
            al_A(j+1) = x3
         end if
      end do
   end if
   close(10)

   ! Чтение матрицы B из data2.dat
   open(11, file='data2.dat', status='old', action='read')
   read(11, '(A)') header
   allocate(ad_B(n), al_B(n), ar_B(n))
   al_B(1) = 0.0d0; ar_B(n) = 0.0d0
   if (n == 1) then
      read(11, *) ad_B(1)
   else
      do j = 1, n
         if (j == 1 .or. j == n) then
            read(11, *) x1, x2
            if (j == 1) then
               ad_B(1) = x1
               al_B(2) = x2
            else
               ar_B(n-1) = x1
               ad_B(n)   = x2
            end if
         else
            read(11, *) x1, x2, x3
            ar_B(j-1) = x1
            ad_B(j)   = x2
            al_B(j+1) = x3
         end if
      end do
   end if
   close(11)

   ! Восстановление полных матриц A и B
   allocate(A_full(n,n), B_full(n,n))
   A_full = 0.0d0
   B_full = 0.0d0
   do i = 1, n
      A_full(i,i) = ad_A(i)
      if (i > 1) A_full(i,i-1) = al_A(i)
      if (i < n) A_full(i,i+1) = ar_A(i)
   end do
   do i = 1, n
      B_full(i,i) = ad_B(i)
      if (i > 1) B_full(i,i-1) = al_B(i)
      if (i < n) B_full(i,i+1) = ar_B(i)
   end do

   ! Вычисление точного произведения
   allocate(C_full(n,n))
   C_full = matmul(A_full, B_full)

   ! Определяем длины столбцов и смещения для упакованного результата
   allocate(offsetC(n+1))
   offsetC(1) = 1
   do j = 1, n
      low = max(1, j-2)
      high = min(n, j+2)
      offsetC(j+1) = offsetC(j) + (high - low + 1)
   end do

   allocate(Cdata(offsetC(n+1)-1))

   ! Чтение упакованного результата из result.dat
   open(12, file='result.dat', status='old', action='read')
   read(12, '(A)') header
   do j = 1, n
      low = max(1, j-2)
      high = min(n, j+2)
      read(12, *) (Cdata(offsetC(j) + (i - low)), i = low, high)
   end do
   close(12)

   ! Сравнение с допуском (для двойной точности можно оставить 1e-12)
   tol = 1.0d-12
   max_diff = 0.0d0
   n_err = 0
   ok = .true.

   do j = 1, n
      low = max(1, j-2)
      high = min(n, j+2)
      do i = low, high
         idx = offsetC(j) + (i - low)
         diff = abs(Cdata(idx) - C_full(i,j))
         if (diff > tol * max(1.0d0, abs(C_full(i,j)))) then
            n_err = n_err + 1
            max_diff = max(max_diff, diff)
            if (n_err <= 10) then
               write(*, '(A,2I4,A,E15.6,A,E15.6)') 'Mismatch at (', i, j, '): computed=', Cdata(idx), ' exact=', C_full(i,j)
            end if
            ok = .false.
         end if
      end do
   end do

   if (ok) then
      write(*,*) 'Check passed: all elements match within tolerance.'
   else
      write(*,*) 'Check FAILED: total mismatches =', n_err, ' max diff =', max_diff
      stop 1
   end if

   deallocate(ad_A, al_A, ar_A, ad_B, al_B, ar_B, A_full, B_full, C_full, Cdata, offsetC)
end program check_result
