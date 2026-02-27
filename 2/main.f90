program multiply_tridiagonal
	use mat_mult, only : mult_tridiag
	implicit none

	integer :: n, i, j, k
	real(8), allocatable :: dA(:), lA(:), uA(:)
	real(8), allocatable :: dB(:), lB(:), uB(:)
	real(8), allocatable :: Cdata(:)
	integer, allocatable :: offsetC(:)
	integer :: low, high
	character(100) :: header

	integer(8) :: t_start, t_end, t_rate
	real(8) :: time_read, time_mult, time_write

	call system_clock(count_rate=t_rate)

	   ! Чтение матрицы A из файла data1.dat
	write(*,*) 'Чтение матрицы A...'
	call system_clock(t_start)

	open(unit=10, file='data1.dat', status='old', action='read')
	read(10, '(A)') header
	read(header(2:), *) n                     ! извлекаем размер n
	allocate(dA(n), lA(n), uA(n))
	lA(1) = 0.0d0
   	uA(n) = 0.0d0

	if (n == 1) then
		read(10, *) dA(1)
	else
		do j = 1, n
			if (j == 1) then
				read(10, *) dA(1), uA(1)
         		else if (j == n) then
            			read(10, *) lA(n), dA(n)
            		else
            			read(10, *) lA(j), dA(j), uA(j)
         		end if
      		end do
	end if
	close(10)

	call system_clock(t_end)
	time_read = real(t_end - t_start, kind=8) / t_rate
	write(*,*) 'Чтение A завершено. Время =', time_read, 'с'

	! Чтение матрицы B из файла data2.dat (аналогично)
	write(*,*) 'Чтение матрицы B...'
	call system_clock(t_start)

	open(unit=11, file='data2.dat', status='old', action='read')
	read(11, '(A)') header
	allocate(dB(n), lB(n), uB(n))
	lB(1) = 0.0d0
   	uB(n) = 0.0d0

	if (n == 1) then
		read(11, *) dB(1)
	else
		do j = 1, n
			if (j == 1) then
				read(11, *) dB(1), uB(1)
			else if (j == n) then
            			read(11, *) lB(n), dB(n)
			else
				read(11, *) lB(j), dB(j), uB(j)
			end if
		end do
	end if
	close(11)

	call system_clock(t_end)
	time_read = time_read + real(t_end - t_start, kind=8) / t_rate
	write(*,*) 'Чтение B завершено. Общее время чтения =', time_read, 'с'

	! Вызов подпрограммы умножения (из модуля)
	write(*,*) 'Умножение матриц...'
	call system_clock(t_start)
	call mult_tridiag(n, dA, lA, uA, dB, lB, uB, Cdata, offsetC)
	call system_clock(t_end)
	time_mult = real(t_end - t_start, kind=8) / t_rate
	write(*,*) 'Умножение завершено. Время =', time_mult, 'с'
	
	! Запись результата в файл result.dat
	write(*,*) 'Начинаем запись результата...'
	call system_clock(t_start)

	open(unit=12, file='result.dat', action='write')
	write(12, '(A, I0)') '# ', n
	do i = 1, n
		low = max(1, i-2)
		high = min(n, i+2)
		do j = low, high
			write(12, '(E23.15)', advance='no') Cdata( offsetC(j) + (i - max(1,j-2)) )
		end do
		write(12, *)
	end do
	close(12)

	call system_clock(t_end)
	time_write = real(t_end - t_start, kind=8) / t_rate
	write(*,*) 'Запись завершена. Время =', time_write, 'с'

	write(*,*) 'Программа выполнена успешно.'
	
	deallocate(dA, lA, uA, dB, lB, uB, Cdata, offsetC)

end program multiply_tridiagonal
