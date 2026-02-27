program check_result
   implicit none
   integer :: n, i, j, low, high, idx, n_err
   real(8), allocatable :: A_full(:,:), B_full(:,:), C_full(:,:)
   real(8), allocatable :: dA(:), lA(:), uA(:)
   real(8), allocatable :: dB(:), lB(:), uB(:)
   real(8), allocatable :: Cdata(:)
   integer, allocatable :: offsetC(:)
   real(8) :: x1, x2, x3, diff, max_diff, tol
   character(100) :: header
   logical :: ok

   ! Чтение матрицы A из data1.dat (по строкам)
   open(10, file='data1.dat', status='old', action='read')
   read(10, '(A)') header
   read(header(2:), *) n
   allocate(dA(n), lA(n), uA(n))
   lA(1) = 0.0d0; uA(n) = 0.0d0
   if (n == 1) then
      read(10, *) dA(1)
   else
      do i = 1, n
         if (i == 1) then
            read(10, *) dA(1), uA(1)
         else if (i == n) then
            read(10, *) lA(n), dA(n)
         else
            read(10, *) lA(i), dA(i), uA(i)
         end if
      end do
   end if
   close(10)

   ! Чтение матрицы B из data2.dat
   open(11, file='data2.dat', status='old', action='read')
   read(11, '(A)') header
   allocate(dB(n), lB(n), uB(n))
   lB(1) = 0.0d0; uB(n) = 0.0d0
   if (n == 1) then
      read(11, *) dB(1)
   else
      do i = 1, n
         if (i == 1) then
            read(11, *) dB(1), uB(1)
         else if (i == n) then
            read(11, *) lB(n), dB(n)
         else
            read(11, *) lB(i), dB(i), uB(i)
         end if
      end do
   end if
   close(11)

   ! Восстановление полных матриц A и B
   allocate(A_full(n,n), B_full(n,n))
   A_full = 0.0d0
   B_full = 0.0d0
   do i = 1, n
      A_full(i,i) = dA(i)
      if (i > 1) A_full(i,i-1) = lA(i)
      if (i < n) A_full(i,i+1) = uA(i)
   end do
   do i = 1, n
      B_full(i,i) = dB(i)
      if (i > 1) B_full(i,i-1) = lB(i)
      if (i < n) B_full(i,i+1) = uB(i)
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
   do i = 1, n
      low = max(1, i-2)
      high = min(n, i+2)
      ! Читаем строку, содержащую элементы C(i,j) для j от low до high
      read(12, *) (Cdata( offsetC(j) + (i - max(1,j-2)) ), j = low, high)
   end do
   close(12)

   ! Сравнение с допуском
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

   deallocate(dA, lA, uA, dB, lB, uB, A_full, B_full, C_full, Cdata, offsetC)
end program check_result
