# Компилятор и флаги
FC = gfortran
FFLAGS = -O2 -Wall -Wextra

# Имена исполняемых файлов
MAIN_EXE = main
GEN_EXE = generate
CHECK_EXE = check

# Размер матрицы по умолчанию (можно переопределить при вызове make)
N ?= 10

# Объектные файлы
MOD_OBJ = mat_mult.o
MAIN_OBJ = main.o
GEN_OBJ = generate.o
CHECK_OBJ = check.o

# Цели по умолчанию
all: $(MAIN_EXE) $(GEN_EXE) $(CHECK_EXE)

# Сборка основной программы
$(MAIN_EXE): $(MAIN_OBJ) $(MOD_OBJ)
	$(FC) $(FFLAGS) -o $@ $^

# Сборка генератора
$(GEN_EXE): $(GEN_OBJ)
	$(FC) $(FFLAGS) -o $@ $^

# Сборка проверяющей программы
$(CHECK_EXE): $(CHECK_OBJ)
	$(FC) $(FFLAGS) -o $@ $^

# Компиляция модуля
$(MOD_OBJ): mat_mult.f90
	$(FC) $(FFLAGS) -c $<

# Компиляция main.f90 (зависит от модуля)
$(MAIN_OBJ): main.f90 $(MOD_OBJ)
	$(FC) $(FFLAGS) -c $<

# Компиляция generate.f90
$(GEN_OBJ): generate.f90
	$(FC) $(FFLAGS) -c $<

# Компиляция check.f90
$(CHECK_OBJ): check.f90
	$(FC) $(FFLAGS) -c $<

# Генерация тестовых данных с заданным размером N
generate: $(GEN_EXE)
	@echo "Generating test data with n=$(N)..."
	./$(GEN_EXE) $(N)

# Запуск основной программы (после генерации данных)
run: $(MAIN_EXE)
	@echo "Running main program..."
	./$(MAIN_EXE)

# Полный тест: генерация -> умножение -> проверка (только для малых N!)
test: generate run $(CHECK_EXE)
	@echo "Checking result..."
	./$(CHECK_EXE)
	@echo "Test completed."

# Бенчмарк: генерация -> умножение (без проверки, для больших N)
benchmark: generate run
	@echo "Benchmark completed."

# Очистка объектных и модульных файлов, исполняемых
clean:
	rm -f *.o *.mod $(MAIN_EXE) $(GEN_EXE) $(CHECK_EXE)

# Полная очистка (включая сгенерированные данные)
distclean: clean
	rm -f data1.dat data2.dat result.dat

.PHONY: all clean distclean generate run test benchmark
