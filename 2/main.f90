program multiply_tridiagonal
	use mat_mult, only : mult_tridiag
	implicit none

	integer :: n, j, k
	real(8), allocatable :: ad_A(:), al_A(:), ar_A(:)
	real(8), allocatable :: ad_B(:), al_B(:), ar_B(:)
	real(8), allocatable :: Cdata(:)
	integer, allocatable :: offsetC(:)
	real(8) :: x1, x2, x3
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
	allocate(ad_A(n), al_A(n), ar_A(n))
	al_A(1) = 0.0d0
	ar_A(n) = 0.0d0

	if (n == 1) then
		read(10, *) ad_A(1)
	else
		do j = 1, n
			if (j == 1 .or. j == n) then
				read(10, *) x1, x2
				if (j == 1) then
					ad_A(1) = x1
               				al_A(2) = x2
            			else   ! j == n
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

	call system_clock(t_end)
	time_read = real(t_end - t_start, kind=8) / t_rate
	write(*,*) 'Чтение A завершено. Время =', time_read, 'с'

	! Чтение матрицы B из файла data2.dat (аналогично)
	write(*,*) 'Чтение матрицы B...'
	call system_clock(t_start)

	open(unit=11, file='data2.dat', status='old', action='read')
	read(11, '(A)') header
	allocate(ad_B(n), al_B(n), ar_B(n))
	al_B(1) = 0.0d0
	ar_B(n) = 0.0d0

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

	call system_clock(t_end)
	time_read = time_read + real(t_end - t_start, kind=8) / t_rate
	write(*,*) 'Чтение B завершено. Общее время чтения =', time_read, 'с'

	! Вызов подпрограммы умножения (из модуля)
	write(*,*) 'Умножение матриц...'
	call system_clock(t_start)
	call mult_tridiag(n, ad_A, al_A, ar_A, ad_B, al_B, ar_B, Cdata, offsetC)
	call system_clock(t_end)
	time_mult = real(t_end - t_start, kind=8) / t_rate
	write(*,*) 'Умножение завершено. Время =', time_mult, 'с'
	
	! Запись результата в файл result.dat
	write(*,*) 'Начинаем запись результата...'
	call system_clock(t_start)

	open(unit=12, file='result.dat', action='write')
	write(12, '(A, I0)') '# ', n
	do j = 1, n
		write(12, *) (Cdata(offsetC(j) + k - 1), k = 1, offsetC(j+1)-offsetC(j))
	end do
	close(12)

	call system_clock(t_end)
	time_write = real(t_end - t_start, kind=8) / t_rate
	write(*,*) 'Запись завершена. Время =', time_write, 'с'

	write(*,*) 'Программа выполнена успешно.'

	! Освобождение памяти
	deallocate(ad_A, al_A, ar_A, ad_B, al_B, ar_B, Cdata, offsetC)

end program multiply_tridiagonal
