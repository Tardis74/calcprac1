# Компилятор
FPC = fpc

# Флаги компиляции
# -Mobjfpc        - объектный режим (для модулей)
# -O3             - максимальная оптимизация
# -CfAVX2         - использовать инструкции AVX2
# -OpCOREAVX2     - оптимизировать под архитектуру с AVX2
# -Fu.            - путь к модулям (текущая директория)
CFLAGS = -Mobjfpc -O3 -CfAVX2 -OpCOREAVX2 -dUseCThreads -Fu.

# Имя исполняемого файла
TARGET = matrix_mul

# Исходные файлы
MAIN = main.pas
UNIT = matrix_utils.pas

# Объектные файлы (компилятор создаёт их автоматически, но указываем для порядка)
# .ppu - скомпилированный модуль, .o - объектный файл
UNIT_PPU = matrix_utils.ppu
UNIT_O = matrix_utils.o
MAIN_O = main.o

# Правило по умолчанию
all: $(TARGET)

# Сборка исполняемого файла
$(TARGET): $(MAIN) $(UNIT)
	$(FPC) $(CFLAGS) -o$(TARGET) $(MAIN)

# Очистка сгенерированных файлов
clean:
	rm -f $(UNIT_PPU) $(UNIT_O) $(MAIN_O) *.ppu *.o *.~* *.bak

# Установка, что clean не является файлом
.PHONY: all clean
