module mat_mult
	implicit none
	private
	public :: mult_tridiag

contains
	subroutine mult_tridiag(n, ad_A, al_A, ar_A, ad_B, al_B, ar_B, Cdata, offsetC)
		integer, intent(in) :: n	!размер матриц
		real(8), intent(in) :: ad_A(:), al_A(:), ar_A(:)	!диагонали A: главная, нижняя, верхняя
		real(8), intent(in) :: ad_B(:), al_B(:), ar_B(:)	!диагонали B
		real(8), allocatable, intent(out) :: Cdata(:)	!одномерный массив, содержащий элементы C по столбцам
		integer, allocatable, intent(out) :: offsetC(:)	!массив длины n+1, где offsetC(j) - позиция начала строки j в Cdata

		integer :: i, j, d, low, high, pos0, idx
		real(8) :: b_left, b_mid, b_right, c
		integer, allocatable :: lenC(:)

		!определение длины столбцов матрицы C (от 3 до 5 элементов)
		allocate(lenC(n))
		do j = 1, n
			low = max(1, j-2)
			high = min(n, j+2)
			lenC(j) = high - low + 1
		end do

		!смещения для каждого столбца в упакованном массиве
		allocate(offsetC(n+1))
		offsetC(1) = 1
			do j = 2, n+1
			offsetC(j) = offsetC(j-1) + lenC(j-1)
		end do

		!память под упакованную матрицу C
		allocate(Cdata(offsetC(n+1)-1))

		! Заполняем C по столбцам
		do j = 1, n
			! Три ненулевых элемента столбца j матрицы B
			b_left  = 0.0d0
			if (j > 1) b_left  = ar_B(j-1)   ! B(j-1, j)
				b_mid   = ad_B(j)                 ! B(j, j)
				b_right = 0.0d0
				if (j < n) b_right = al_B(j+1)    ! B(j+1, j)

				low = max(1, j-2)
				high = min(n, j+2)
				pos0 = offsetC(j) - 1   ! база для индексации внутри столбца j

				do i = low, high
					d = i - j
					select case(d)
						case(-2)
							c = ar_A(i) * b_left
						case(-1)
							c = ad_A(i) * b_left + ar_A(i) * b_mid
						case(0)
							c = ad_A(i) * b_mid
							if (i > 1) c = c + al_A(i) * b_left
							if (i < n) c = c + ar_A(i) * b_right
						case(1)
							c = al_A(i) * b_mid + ad_A(i) * b_right
						case(2)
							c = al_A(i) * b_right
						case default
							c = 0.0d0   ! не должно достигаться
					end select

					idx = i - low + 1          ! позиция внутри столбца (1..lenC(j))
					Cdata(pos0 + idx) = c
				end do
		end do

	end subroutine mult_tridiag

end module mat_mult
